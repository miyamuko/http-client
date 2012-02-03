; -*- mode: lisp; package: http-client.test.types; encoding: shift_jis -*-

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

(defpackage :http-client.test.types
  (:use :lisp :editor
   :lisp-unit
   :http-client
   :http-client.base
   :http-client.test.helper
   )
  )

(in-package :http-client.test.types)

(remove-all-tests :http-client.test.types)

(define-test http-progress-percent
  (let ((p (http-client::make-http-progress :response-p nil :total 150)))
    (assert-eql 0 (http-progress-percent p))
    (setf (http-progress-current p) 75)
    (assert-eql 50 (http-progress-percent p))
    (setf (http-progress-current p) 149)
    (assert-eql 99 (http-progress-percent p))
    (setf (http-progress-current p) 150)
    (assert-eql 100 (http-progress-percent p))
    )
  (let ((p (http-client::make-http-progress :response-p nil :total nil)))
    (assert-eql nil (http-progress-percent p))
    (setf (http-progress-current p) 10)
    (assert-eql nil (http-progress-percent p))
    )
  )

(define-test http-progress-print-function
  (let ((p (http-client::make-http-progress :response-p t :total 1500)))
    (assert-equal "Receive 0/1,500 bytes (0%)"
        (format nil "~A" p))
    (setf (http-progress-current p) 750)
    (assert-equal "Receive 750/1,500 bytes (50%)"
        (format nil "~A" p))
    (setf (http-progress-current p) 1499)
    (assert-equal "Receive 1,499/1,500 bytes (99%)"
        (format nil "~A" p))
    (setf (http-progress-current p) 1500)
    (assert-equal "Receive 1,500/1,500 bytes (100%)"
        (format nil "~A" p))
    )
  (let ((p (http-client::make-http-progress :response-p nil)))
    (assert-equal "Send 0 bytes"
        (format nil "~A" p))
    (setf (http-progress-current p) 750)
    (assert-equal "Send 750 bytes"
        (format nil "~A" p))
    (setf (http-progress-current p) 1499)
    (assert-equal "Send 1,499 bytes"
        (format nil "~A" p))
    (setf (http-progress-current p) 1500)
    (assert-equal "Send 1,500 bytes"
        (format nil "~A" p))
    )
  )
