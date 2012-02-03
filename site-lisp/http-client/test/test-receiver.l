; -*- mode: lisp; package: http-client.test.receiver; encoding: shift_jis -*-

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

(defpackage :http-client.test.receiver
  (:use :lisp :editor
   :lisp-unit
   :http-client
   :http-client.base
   :http-client.test.helper
   )
  )

(in-package :http-client.test.receiver)

(remove-all-tests :http-client.test.receiver)

(define-test http-string-receiver/encoding
  (let ((r (funcall (http-string-receiver :encoding nil) nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢"))
    (funcall r (utf8 "Ç§Ç¶Ç®"))
    (let ((result (funcall r nil)))
      (assert-type function result)
      (assert-equal (utf8 "Ç†Ç¢Ç§Ç¶Ç®")
          (funcall result))))

  (let ((r (funcall (http-string-receiver :encoding *encoding-utf8n*) nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢"))
    (funcall r (utf8 "Ç§Ç¶Ç®"))
    (let ((result (funcall r nil)))
      (assert-type function result)
      (assert-equal "Ç†Ç¢Ç§Ç¶Ç®"
          (funcall result))))

  (let ((r (funcall (http-string-receiver) nil '(("Content-Type" . "text/plain; charset=utf-8")) nil)))
    (funcall r (utf8 "Ç†Ç¢"))
    (funcall r (utf8 "Ç§Ç¶Ç®"))
    (let ((result (funcall r nil)))
      (assert-type function result)
      (assert-equal "Ç†Ç¢Ç§Ç¶Ç®"
          (funcall result))))
  )

(define-test http-string-receiver/normalize-eol-code
  (let ((r (funcall (http-string-receiver :encoding nil) nil nil nil)))
    (funcall r (utf8 "A\r\nB\rC\nD"))
    (funcall r (utf8 "\r\n"))
    (funcall r (utf8 "E"))
    (let ((result (funcall r nil)))
      (assert-type function result)
      (assert-equal (utf8 "A\r\nB\rC\nD\r\nE")
          (funcall result))))

  (let ((r (funcall (http-string-receiver :encoding *encoding-utf8n*) nil nil nil)))
    (funcall r (utf8 "A\r\nB\rC\nD"))
    (funcall r (utf8 "\r\n"))
    (funcall r (utf8 "E"))
    (let ((result (funcall r nil)))
      (assert-type function result)
      (assert-equal "A\nB\nC\nD\nE"
          (funcall result))))
  )


(define-test http-general-receiver/chunk
  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line nil :encoding nil)
                     nil nil nil)))
    (funcall r "Ç†Ç¢Ç§Ç¶Ç®\r\n")
    (funcall r "Ç©Ç´Ç≠\n")
    (funcall r "ÇØÇ±\nÇ≥ÇµÇ∑")
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 3 (length result))
      (assert-equal (list "Ç†Ç¢Ç§Ç¶Ç®\r\n"
                          "Ç©Ç´Ç≠\n"
                          "ÇØÇ±\nÇ≥ÇµÇ∑")
          result)
      ))
  )

(define-test http-general-receiver/line
  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line t :encoding nil)
                     nil nil nil)))
    (funcall r "Ç†Ç¢Ç§Ç¶Ç®\r\n")
    (funcall r "Ç©Ç´Ç≠\n")
    (funcall r "ÇØÇ±\nÇ≥ÇµÇ∑")
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 4 (length result))
      (assert-equal (list "Ç†Ç¢Ç§Ç¶Ç®\n"
                          "Ç©Ç´Ç≠\n"
                          "ÇØÇ±\n"
                          "Ç≥ÇµÇ∑")
          result)
      ))
  )

(define-test http-general-receiver/utf8-line
  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line t :encoding *encoding-utf8n*)
                     nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 4 (length result))
      (assert-equal (list "Ç†Ç¢Ç§Ç¶Ç®\n"
                          "Ç©Ç´Ç≠\n"
                          "ÇØÇ±\n"
                          "Ç≥ÇµÇ∑")
          result)
      ))
  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line t)
                     nil '(("content-type" . "text/html; charset=\"utf-8\"")) nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 4 (length result))
      (assert-equal (list "Ç†Ç¢Ç§Ç¶Ç®\n"
                          "Ç©Ç´Ç≠\n"
                          "ÇØÇ±\n"
                          "Ç≥ÇµÇ∑")
          result)
      ))
  )

(define-test http-general-receiver/utf8-line-without-encoding
  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line t :encoding nil)
                     nil nil nil)))
    ;; UTF-8 Ç»ï∂éöóÒÅAÇ©Ç¬ÅA:encoding ÇéwíËÇµÇ»Ç¢èÍçáÇÕ
    ;; ê≥ÇµÇ≠ï™äÑÇ≈Ç´Ç»Ç¢
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 3 (length result))
      (assert-equal (list (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n")
                          (utf8 "Ç©Ç´Ç≠\nÇØÇ±\n")
                          (utf8 "Ç≥ÇµÇ∑"))
          result)
      ))
  )

(define-test http-oport-receiver
  (let* ((s (make-string-output-stream))
         (r (funcall (http-oport-receiver s #'get-output-stream-string
                                          :encoding nil)
                     nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (assert-equal (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\nÇ©Ç´Ç≠\nÇØÇ±\nÇ≥ÇµÇ∑")
        (funcall r nil)))
  )

(define-test http-oport-receiver/encoding
  (let* ((s (make-string-output-stream))
         (r (funcall (http-oport-receiver s #'get-output-stream-string
                                          :encoding *encoding-utf8n*)
                     nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (assert-equal "Ç†Ç¢Ç§Ç¶Ç®\nÇ©Ç´Ç≠\nÇØÇ±\nÇ≥ÇµÇ∑"
        (funcall r nil)))

  (let* ((s (make-string-output-stream))
         (r (funcall (http-oport-receiver s #'get-output-stream-string)
                     nil '(("content-type" . "application/json; charset=utf-8")) nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (assert-equal "Ç†Ç¢Ç§Ç¶Ç®\nÇ©Ç´Ç≠\nÇØÇ±\nÇ≥ÇµÇ∑"
        (funcall r nil)))
  )

(define-test http-buffer-receiver
  (let ((r (funcall (http-buffer-receiver "test")
                    nil nil nil)))
    (funcall r "Ç†Ç¢Ç§Ç¶Ç®\r\n")
    (funcall r "Ç©Ç´Ç≠\r\n")
    (funcall r "ÇØÇ±\r\nÇ≥ÇµÇ∑")
    (let ((result (funcall r nil)))
      (assert-type buffer result)
      (assert-equal "Ç†Ç¢Ç§Ç¶Ç®\r\nÇ©Ç´Ç≠\r\nÇØÇ±\r\nÇ≥ÇµÇ∑"
          (with-selected-buffer result
            (buffer-substring (point-min) (point-max))))
      (delete-buffer result)))

  (let* ((b (get-buffer-create "test"))
         (r (funcall (http-buffer-receiver b)
                    nil nil nil)))
    (funcall r "Ç†Ç¢Ç§Ç¶Ç®\r\n")
    (funcall r "Ç©Ç´Ç≠\r\n")
    (funcall r "ÇØÇ±\r\nÇ≥ÇµÇ∑")
    (let ((result (funcall r nil)))
      (assert-type buffer result)
      (assert-eq b result)
      (assert-equal "Ç†Ç¢Ç§Ç¶Ç®\r\nÇ©Ç´Ç≠\r\nÇØÇ±\r\nÇ≥ÇµÇ∑"
          (with-selected-buffer result
            (buffer-substring (point-min) (point-max))))
      (delete-buffer result)))

  (let ((b (get-buffer-create "test"))
        (r (funcall (http-buffer-receiver "test")
                    nil nil nil)))
    (funcall r "Ç†Ç¢Ç§Ç¶Ç®\r\n")
    (funcall r "Ç©Ç´Ç≠\r\n")
    (funcall r "ÇØÇ±\r\nÇ≥ÇµÇ∑")
    (let ((result (funcall r nil)))
      (assert-type buffer result)
      (assert-false (eq b result))
      (assert-equal "Ç†Ç¢Ç§Ç¶Ç®\r\nÇ©Ç´Ç≠\r\nÇØÇ±\r\nÇ≥ÇµÇ∑"
          (with-selected-buffer result
            (buffer-substring (point-min) (point-max))))
      (delete-buffer b)
      (delete-buffer result)))
  )

(define-test http-buffer-receiver/utf8-crlf
  (let ((r (funcall (http-buffer-receiver "test" :encoding *encoding-utf8n*)
                    nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\r\n"))
    (funcall r (utf8 "ÇØÇ±\r\nÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-type buffer result)
      (assert-equal *eol-crlf* (buffer-eol-code result))
      (assert-equal "Ç†Ç¢Ç§Ç¶Ç®\nÇ©Ç´Ç≠\nÇØÇ±\nÇ≥ÇµÇ∑"
          (with-selected-buffer result
            (buffer-substring (point-min) (point-max))))
      (delete-buffer result)))
  )

(define-test http-buffer-receiver/sjis-cr
  (let ((r (funcall (http-buffer-receiver "test" :encoding *encoding-sjis*)
                    nil nil nil)))
    (funcall r (sjis "Ç†Ç¢Ç§Ç¶Ç®\r"))
    (funcall r (sjis "Ç©Ç´Ç≠\r"))
    (funcall r (sjis "ÇØÇ±\rÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-type buffer result)
      (assert-equal *eol-cr* (buffer-eol-code result))
      (assert-equal "Ç†Ç¢Ç§Ç¶Ç®\nÇ©Ç´Ç≠\nÇØÇ±\nÇ≥ÇµÇ∑"
          (with-selected-buffer result
            (buffer-substring (point-min) (point-max))))
      (delete-buffer result)))
  )

(define-test http-buffer-receiver/euc-lf
  (let ((r (funcall (http-buffer-receiver "test" :encoding *encoding-euc-jp*)
                    nil nil nil)))
    (funcall r (euc "Ç†Ç¢Ç§Ç¶Ç®\n"))
    (funcall r (euc "Ç©Ç´Ç≠\n"))
    (funcall r (euc "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-type buffer result)
      (assert-equal *eol-lf* (buffer-eol-code result))
      (assert-equal "Ç†Ç¢Ç§Ç¶Ç®\nÇ©Ç´Ç≠\nÇØÇ±\nÇ≥ÇµÇ∑"
          (with-selected-buffer result
            (buffer-substring (point-min) (point-max))))
      (delete-buffer result)))
  )

(define-test http-null-receiver
  (let ((r (funcall (http-null-receiver)
                    nil nil nil)))
    (funcall r (euc "Ç†Ç¢Ç§Ç¶Ç®\n"))
    (funcall r (euc "Ç©Ç´Ç≠\n"))
    (funcall r (euc "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (assert-true (null (funcall r nil))))
  )

(define-test http-file-receiver
  (let ((r (funcall (http-file-receiver "test.txt")
                    nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\rÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-equal (namestring "test.txt") result)
      (assert-equal (bin (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\nÇ©Ç´Ç≠\nÇØÇ±\rÇ≥ÇµÇ∑"))
          (map 'string 'code-char
               (alexandria:read-file-into-byte-vector "test.txt")))))
  (delete-file "test.txt")
  )

(define-test http-file-receiver/append
  (let ((r (funcall (http-file-receiver "test.txt")
                    nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\rÇ≥ÇµÇ∑"))
    (funcall r nil))
  (let ((r (funcall (http-file-receiver "test.txt" :if-exists :append)
                    nil nil nil)))
    (funcall r (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\n"))
    (funcall r (utf8 "Ç©Ç´Ç≠\n"))
    (funcall r (utf8 "ÇØÇ±\rÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-equal (namestring "test.txt") result)
      (assert-equal (bin (utf8 "Ç†Ç¢Ç§Ç¶Ç®\r\nÇ©Ç´Ç≠\nÇØÇ±\rÇ≥ÇµÇ∑Ç†Ç¢Ç§Ç¶Ç®\r\nÇ©Ç´Ç≠\nÇØÇ±\rÇ≥ÇµÇ∑"))
          (map 'string 'code-char
               (alexandria:read-file-into-byte-vector "test.txt")))))
  (delete-file "test.txt")
  )

(define-test http-file-receiver/text
  (let ((r (funcall (http-file-receiver "test.txt" :encoding :text)
                    nil nil nil)))
    (funcall r (euc "Ç†Ç¢Ç§Ç¶Ç®\n"))
    (funcall r (euc "Ç©Ç´Ç≠\n"))
    (funcall r (euc "ÇØÇ±\nÇ≥ÇµÇ∑"))
    (let ((result (funcall r nil)))
      (assert-equal (namestring "test.txt") result)
      (assert-equal (bin (euc "Ç†Ç¢Ç§Ç¶Ç®\r\nÇ©Ç´Ç≠\r\nÇØÇ±\r\nÇ≥ÇµÇ∑"))
          (map 'string 'code-char
               (alexandria:read-file-into-byte-vector "test.txt")))))
  (delete-file "test.txt")
  )

(define-test http-file-receiver/binary-file
  (let ((r (funcall (http-file-receiver "test.txt")
                    nil nil nil))
        (bin (alexandria:read-file-into-byte-vector "test-file.zip")))
    (funcall r (map 'string 'code-char bin))
    (let ((result (funcall r nil)))
      (assert-equal (namestring "test.txt") result)
      (assert-equalp bin
          (alexandria:read-file-into-byte-vector "test.txt"))))
  (delete-file "test.txt")
  )

(define-test http-cond-receiver
  (let ((cr (http-cond-receiver (status headers content-length)
              ((= status 200)
               (http-string-receiver))
              (t
               (http-buffer-receiver "test")))))
    (let ((r (funcall cr 200 nil nil)))
      (funcall r "hoge")
      (let ((result (funcall r nil)))
        (assert-type function result)
        (assert-equal "hoge" (funcall result))))
    (let ((r (funcall cr 201 nil nil)))
      (funcall r "hoge")
      (let ((result (funcall r nil)))
        (assert-type buffer result)
        (assert-equal "hoge"
            (with-selected-buffer result
              (buffer-substring (point-min) (point-max))))
        (delete-buffer result)))
    ))
