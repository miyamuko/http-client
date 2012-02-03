; -*- mode: lisp; package: http-client.test.misc; encoding: shift_jis -*-

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

(defpackage :http-client.test.misc
  (:use :lisp :editor
   :lisp-unit
   :http-client
   :http-client.base
   :http-client.test.helper
   )
  )

(in-package :http-client.test.misc)

(remove-all-tests :http-client.test.misc)

(define-test detect-encoding
  (assert-equal nil
      (http-client::detect-encoding nil nil))
  (assert-equal nil
      (http-client::detect-encoding nil '(("Content-Type" . "text/html; charset=utf-8"))))

  (assert-equal *encoding-sjis*
      (http-client::detect-encoding *encoding-sjis* nil))
  (assert-equal *encoding-sjis*
      (http-client::detect-encoding *encoding-sjis* '(("Content-Type" . "text/html; charset=utf-8"))))

  (assert-equal nil
      (http-client::detect-encoding t nil))
  (assert-equal nil
      (http-client::detect-encoding t '(("Content-Type" . "text/html; charset=hoge"))))

  (assert-equal *encoding-utf8n*
      (http-client::detect-encoding t '(("Content-Type" . "text/html; charset=utf-8"))))
  (assert-equal *encoding-utf8n*
      (http-client::detect-encoding t '(("Content-Type" . "text/html; charset=UTF-8"))))
  (assert-equal *encoding-utf8n*
      (http-client::detect-encoding t '(("Content-Type" . "text/html; charset=\"UTF-8\""))))
  (assert-equal *encoding-utf8n*
      (http-client::detect-encoding t '(("Content-Type" . "text/plain; charset=UTF-8"))))
  (assert-equal *encoding-utf8n*
      (http-client::detect-encoding t '(("Content-Type" . "application/json;charset=\"UTF-8\""))))
  (assert-equal *encoding-sjis*
      (http-client::detect-encoding t '(("Content-Type" . "text/html; charset=Shift_JIS"))))
  (assert-equal *encoding-euc-jp*
      (http-client::detect-encoding t '(("Content-Type" . "text/html; charset=EUC-JP"))))
  (assert-equal *encoding-euc-jp*
      (http-client::detect-encoding t '(("content-type" . "text/html; charset=EUC-JP"))))
  )
