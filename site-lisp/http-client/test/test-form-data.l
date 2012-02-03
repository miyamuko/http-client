; -*- mode: lisp; package: http-client.test.form-data; encoding: shift_jis -*-

;; Copyright (c) 2012 MIYAMUKO Katsuyuki.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require "lisp-unit")

(require "http-client")
(require "http-client/test/helper")

(defpackage :http-client.test.form-data
  (:use :lisp :editor
   :lisp-unit
   :http-client.base
   :http-client.test.helper
   )
  )

(in-package :http-client.test.form-data)

(remove-all-tests :http-client.test.form-data)

(define-test multipart/form-data-p
  (assert-equal nil (multipart/form-data-p '((q "hoge"))))
  (assert-equal t (multipart/form-data-p '((q :value "hoge"))))
  (assert-equal t (multipart/form-data-p '((q :file "hoge.png"))))
  (assert-equal t (multipart/form-data-p '((q :file nil :value "hoge"))))
  (assert-equal t (multipart/form-data-p '((q :file nil :value "hoge" :content-type "text/html"))))
  )

(define-test build-multipart/form-data
  (assert-equal '((:data "--boundary\r\n" :size 12)
                  (:data "Content-Disposition: form-data; name=\"q\"\r\n\r\nhoge" :size 48)
                  (:data "\r\n" :size 2)
                  (:data "--boundary--\r\n" :size 14))
      (build-multipart/form-data '((q "hoge")) *encoding-sjis* "boundary"))
  (assert-equal '((:data "--boundary\r\n" :size 12)
                  (:data "Content-Disposition: form-data; name=\"q\"\r\n\r\nhoge" :size 48)
                  (:data "\r\n" :size 2)
                  (:data "--boundary--\r\n" :size 14))
      (build-multipart/form-data '((q :value "hoge")) *encoding-sjis* "boundary"))
  (assert-equal `((:data "--boundary\r\n" :size 12)
                  (:data "Content-Disposition: form-data; name=\"q\"; filename=\"test-file-lf-utf8.bin\"\r\n\r\n" :size 78)
                  (:file ,(namestring "test-file-lf-utf8.bin") :size 37)
                  (:data "\r\n" :size 2)
                  (:data "--boundary--\r\n" :size 14))
      (build-multipart/form-data '((q :file "test-file-lf-utf8.bin")) *encoding-sjis* "boundary"))
  )

(define-test calc-multipart/form-data-size
  (assert-equal 76
      (calc-multipart/form-data-size
       (build-multipart/form-data '((q "hoge")) *encoding-sjis* "boundary")))
  (assert-equal 76
      (calc-multipart/form-data-size
       (build-multipart/form-data '((q :value "hoge")) *encoding-sjis* "boundary")))
  (assert-equal 143
      (calc-multipart/form-data-size
       (build-multipart/form-data '((q :file "test-file-lf-utf8.bin")) *encoding-sjis* "boundary")))
  )

(define-test make-form-data-input-stream
  (let ((buffer (make-vector 4096 :element-type 'character :fill-pointer 0)))
    (assert-equal (concat "--boundary\r\n"
                          "Content-Disposition: form-data; name=\"q\"\r\n"
                          "\r\n"
                          "hoge\r\n"
                          "--boundary--\r\n")
        (read-into buffer (make-form-data-input-stream
                           (build-multipart/form-data '((q "hoge")) *encoding-sjis* "boundary"))))
    (assert-equal (concat "--boundary\r\n"
                          "Content-Disposition: form-data; name=\"q\"\r\n\r\nhoge"
                          "\r\n"
                          "--boundary--\r\n")
        (read-into buffer (make-form-data-input-stream
                           (build-multipart/form-data '((q :value "hoge")) *encoding-sjis* "boundary"))))
    (assert-equal (concat "--boundary\r\n"
                          "Content-Disposition: form-data; name=\"q\"; filename=\"test-file-lf-utf8.bin\"\r\n\r\n"
                          (bin (utf8 "xyzzy\n"))
                          (bin (utf8 "XYZZY\n"))
                          (bin (utf8 "ÇòÇôÇöÇöÇô\n"))
                          (bin (utf8 "èLÇ¢ñÍ"))
                          "\r\n"
                          "--boundary--\r\n")
        (read-into buffer (make-form-data-input-stream
                           (build-multipart/form-data '((q :file "test-file-lf-utf8.bin")) *encoding-sjis* "boundary"))))
    ))
