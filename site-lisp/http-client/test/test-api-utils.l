; -*- mode: lisp; package: http-client.test.api-utils; encoding: shift_jis -*-

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

(defpackage :http-client.test.api-utils
  (:use :lisp :editor
   :lisp-unit
   :http-client
   :http-client.test.helper
   )
  )

(in-package :http-client.test.api-utils)

(remove-all-tests :http-client.test.api-utils)

(define-test http-compose-query
  (assert-equal "/search?q=xyzzy%20%93%C7%82%DD%95%FB&num=30"
      (http-compose-query "/search"
                          '((q "xyzzy ì«Ç›ï˚") (num 30))))
  (assert-equal "/search?q=xyzzy%20%E8%AA%AD%E3%81%BF%E6%96%B9&num=30"
      (http-compose-query "/search"
                          `((q ,(utf8 "xyzzy ì«Ç›ï˚")) (num 30))))
  (assert-equal "/search?q=xyzzy%20%E8%AA%AD%E3%81%BF%E6%96%B9&num=30"
      (http-compose-query "/search"
                          '((q "xyzzy ì«Ç›ï˚") (num 30))
                          *encoding-utf8n*))
  (assert-equal "/search?q=xyzzy%20%93%C7%82%DD%95%FB&num=30"
      (http-compose-query "/search"
                          "q=xyzzy%20%93%C7%82%DD%95%FB&num=30"))
  )

(define-test http-compose-form-data
  (multiple-value-bind (form-data boundary)
      (http-compose-form-data '(("name" "value")) nil)
    (assert-equal (format nil "~{~A\r\n~}"
                          (list
                           (concat "--" boundary)
                           "Content-Disposition: form-data; name=\"name\""
                           ""
                           "value"
                           (concat "--" boundary "--")))
        form-data))
  (multiple-value-bind (form-data boundary)
      (http-compose-form-data `(("name1" "Ç†")
                                ("name2" :value "Ç¢")
                                ("name3" :value "Ç§" :content-type "text/plain")
                                ("file1" :file "test-file-crlf-sjis.bin")
                                ("file2" :file ,(truename "test-file-lf-utf8.bin")
                                 :content-transfer-encoding "binary")
                                )
                              nil *encoding-utf8n*)
    (assert-equal (format nil "~{~A\r\n~}"
                          (list
                           (concat "--" boundary)
                           "Content-Disposition: form-data; name=\"name1\""
                           ""
                           (utf8 "Ç†")
                           (concat "--" boundary)
                           "Content-Disposition: form-data; name=\"name2\""
                           ""
                           (utf8 "Ç¢")
                           (concat "--" boundary)
                           "Content-Disposition: form-data; name=\"name3\""
                           "Content-Type: text/plain"
                           ""
                           (utf8 "Ç§")
                           (concat "--" boundary)
                           "Content-Disposition: form-data; name=\"file1\"; filename=\"test-file-crlf-sjis.bin\""
                           ""
                           (bin "xyzzy\r\nXYZZY\r\nÇòÇôÇöÇöÇô\r\nèLÇ¢ñÍ")
                           (concat "--" boundary)
                           "Content-Disposition: form-data; name=\"file2\"; filename=\"test-file-lf-utf8.bin\""
                           "Content-Transfer-Encoding: binary"
                           ""
                           (bin (utf8 "xyzzy\nXYZZY\nÇòÇôÇöÇöÇô\nèLÇ¢ñÍ"))
                           (concat "--" boundary "--")))
        form-data))
  (let* ((result )
         (s (make-general-output-stream #'(lambda (chunk)
                                            (push chunk result)))))
    (multiple-value-bind (form-data boundary)
        (http-compose-form-data '(("name" "value")) s)
      (assert-eql s form-data)
      (assert-equal (format nil "~{~A\r\n~}"
                            (list
                             (concat "--" boundary)
                             "Content-Disposition: form-data; name=\"name\""
                             ""
                             "value"
                             (concat "--" boundary "--")))
          (format nil "~{~A~}" (nreverse result)))))
  )
