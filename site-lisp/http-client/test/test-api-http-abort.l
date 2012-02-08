; -*- mode: lisp; package: http-client.test.api-http-abort; encoding: shift_jis -*-

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

;;
;; https://github.com/miyamuko/node-test-httpd を利用したテスト。
;;

;;; Code:

(require "lisp-unit")

(require "json")
(require "http-client")
(require "http-client/test/helper")

(defpackage :http-client.test.api-http-abort
  (:use :lisp :editor
   :lisp-unit
   :json
   :http-client
   :http-client.test.helper
   )
  )

(in-package :http-client.test.api-http-abort)

(remove-all-tests :http-client.test.api-http-abort)

(define-test http-abort
  (let ((client (http-get "http://www.google.co.jp/")))
    (flet ((ping ()
             (values (http-request-aborted-p client)
                     (http-request-waiting-p client)
                     (http-request-completed-p client))))
      (assert-equal (values nil t nil) (ping))
      (http-request-abort client)
      (assert-equal (values t t nil) (ping))
      (http-response-wait client)
      (assert-equal (values t nil t) (ping))
      ))
  )

(define-test http-abort/callback
  (let (onabort-call-p
        onabort-arg
        oncomplete-call-p
        oncomplete-args
        onerror-call-p
        onerror-arg)
    (let ((client (http-get "http://www.google.co.jp/"
                            :onabort #'(lambda ()
                                         (setf onabort-call-p t))
                            :onerror #'(lambda (err)
                                         (setf onerror-call-p t
                                               onerror-arg err))
                            :oncomplete #'(lambda (&rest args)
                                            (setf oncomplete-call-p t
                                                  oncomplete-args args))
                            )))
      (http-request-abort client)
      (assert-false onabort-call-p)
      (assert-false onerror-call-p)
      (assert-false oncomplete-call-p)

      (http-response-wait client)
      (assert-true onabort-call-p)
      (assert-false onerror-call-p)
      (assert-false oncomplete-call-p)
      ))
  )
