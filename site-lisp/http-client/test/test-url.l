; -*- mode: lisp; package: http-client.base.test.url; encoding: shift_jis -*-

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

(defpackage :http-client.base.test.url
  (:use :lisp :editor
   :lisp-unit
   :http-client.base
   )
  )

(in-package :http-client.base.test.url)

(remove-all-tests :http-client.base.test.url)

(define-test add-query-params
  (assert-equal "/search?q=xyzzy&num=50"
      (add-query-params "/search" '(:q "xyzzy" :num 50)))
  (assert-equal "/search?q=xyzzy&num=50"
      (add-query-params "/search" '((:q . "xyzzy") (:num . 50))))
  (assert-equal "/search?q=xyzzy&num=50"
      (add-query-params "/search" '((:q "xyzzy") (:num 50))))

  (assert-equal "/search?ie=UTF-8&q=xyzzy&num=50"
      (add-query-params "/search?ie=UTF-8" '((:q "xyzzy") (:num 50))))

  (assert-equal "?q=xyzzy&num=50"
      (add-query-params nil '((:q "xyzzy") (:num 50))))
  (assert-equal "?q=xyzzy&num=50"
      (add-query-params "" '((:q "xyzzy") (:num 50))))
  (assert-equal "/?q=xyzzy&num=50"
      (add-query-params "/" '((:q "xyzzy") (:num 50))))
  )


(define-test add-query-string
  (assert-equal "/?q=xyzzy&num=50"
      (add-query-string "/" "q=xyzzy&num=50"))
  (assert-equal "/?q=xyzzy&num=50"
      (add-query-string "/?" "q=xyzzy&num=50"))

  (assert-equal "/?ie=UTF-8&q=xyzzy&num=50"
      (add-query-string "/?ie=UTF-8" "q=xyzzy&num=50"))
  (assert-equal "/?ie=UTF-8&q=xyzzy&num=50"
      (add-query-string "/?ie=UTF-8&" "q=xyzzy&num=50"))
  )
