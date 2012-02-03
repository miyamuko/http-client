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
   :http-client.test.helper
   )
  )

(in-package :http-client.test.receiver)

(remove-all-tests :http-client.test.receiver)

(define-test http-string-receiver/encoding
  (let ((r (funcall (http-string-receiver :encoding nil) nil nil nil)))
    (funcall r (utf8 "‚ ‚¢"))
    (funcall r (utf8 "‚¤‚¦‚¨"))
    (let ((result (funcall r nil)))
      (assert-type function result)
      (assert-equal (utf8 "‚ ‚¢‚¤‚¦‚¨")
          (funcall result))))

  (let ((r (funcall (http-string-receiver :encoding *encoding-utf8n*) nil nil nil)))
    (funcall r (utf8 "‚ ‚¢"))
    (funcall r (utf8 "‚¤‚¦‚¨"))
    (let ((result (funcall r nil)))
      (assert-type function result)
      (assert-equal "‚ ‚¢‚¤‚¦‚¨"
          (funcall result))))

  (let ((r (funcall (http-string-receiver) nil '(("Content-Type" . "text/plain; charset=utf-8")) nil)))
    (funcall r (utf8 "‚ ‚¢"))
    (funcall r (utf8 "‚¤‚¦‚¨"))
    (let ((result (funcall r nil)))
      (assert-type function result)
      (assert-equal "‚ ‚¢‚¤‚¦‚¨"
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


(define-test http-general-receiver/encoding
  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line nil :encoding nil)
                     nil nil nil)))
    (funcall r (utf8 "‚ ‚¢‚¤‚¦‚¨\r\n"))
    (funcall r (utf8 "‚©‚«‚­"))
    (funcall r (utf8 "‚¯‚±\n‚³‚µ‚·"))
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 3 (length result))
      (assert-equal (list (utf8 "‚ ‚¢‚¤‚¦‚¨\r\n")
                          (utf8 "‚©‚«‚­")
                          (utf8 "‚¯‚±\n‚³‚µ‚·"))
          result)
      ))

  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line nil :encoding *encoding-utf8n*)
                     nil nil nil)))
    (funcall r (utf8 "‚ ‚¢‚¤‚¦‚¨\n"))
    (funcall r (utf8 "‚©‚«‚­"))
    (funcall r (utf8 "‚¯‚±\n‚³‚µ‚·"))
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 2 (length result))
      (assert-equal (list "‚ ‚¢‚¤‚¦‚¨\n"
                          "‚©‚«‚­‚¯‚±\n"
                          "‚³‚µ‚·")
          result)
      ))
  )

(define-test http-general-receiver/line+normalize-eol-code
  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line nil)
                     nil nil nil)))
    (funcall r (utf8 "A\r\nB\rC\nD"))
    (funcall r (utf8 "\r\n"))
    (funcall r (utf8 "E"))
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 3 (length result))
      (assert-equal '("A\r\nB\rC\nD"
                      "\r\n"
                      "E")
          result)
      ))

  (let* ((lines nil)
         (r (funcall (http-general-receiver #'(lambda (line)
                                                (if line
                                                    (push line lines)
                                                  (reverse lines)))
                                            :line t)
                     nil nil nil)))
    (funcall r (utf8 "A\r\nB\rC\nD"))
    (funcall r (utf8 "\r\n"))
    (funcall r (utf8 "‚ ‚¢‚¤"))
    (let ((result (funcall r nil)))
      (assert-type list result)
      (assert-eql 5 (length result))
      (assert-equal (list "A\n" "B\n" "C\n" "D\n" (utf8 "‚ ‚¢‚¤"))
          result)
      ))
  )
