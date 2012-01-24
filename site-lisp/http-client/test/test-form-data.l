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
   :http-client
   )
  )

(in-package :http-client.test.form-data)

(remove-all-tests :http-client.test.form-data)

(define-test http-client/form-data/multipart-form-data-p
  (assert-equal nil (http-client::multipart-form-data-p '((q "hoge"))))
  (assert-equal t (http-client::multipart-form-data-p '((q :value "hoge"))))
  (assert-equal t (http-client::multipart-form-data-p '((q :file "hoge.png"))))
  (assert-equal t (http-client::multipart-form-data-p '((q :file nil :value "hoge"))))
  (assert-equal t (http-client::multipart-form-data-p '((q :file nil :value "hoge" :content-type "text/html"))))
  )
