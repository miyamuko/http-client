; -*- mode: lisp; package: http-client.test.api-http-head; encoding: shift_jis -*-

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

(defpackage :http-client.test.api-http-head
  (:use :lisp :editor
   :lisp-unit
   :json
   :http-client
   :http-client.test.helper
   )
  )

(in-package :http-client.test.api-http-head)

(remove-all-tests :http-client.test.api-http-head)

(define-test http-head
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-head "http://localhost:3000/snoop"))
    (assert-equal 200 status)
    (assert-equal "application/json; charset=utf-8"
        ($ headers "content-type"))
    (assert-equal "" body)
    )
  )

(define-test http-head/headers
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-head "http://localhost:3000/snoop"
                  :headers `(:X-Yzzy-Version ,(software-version)
                             :X-Hoge 1
                             :X-Hoge 2
                             :X-Hoge 3)))
    (assert-equal "" body)
    )
  )

(define-test http-head/query
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-head "http://localhost:3000/snoop"
                  :query '((q "xyzzy 読み方") (num 50))
                  :encoding *encoding-sjis*))
    (assert-equal "" body)
    )
  )

(define-test http-head/progress
  (let (onprogress-called-p)
    (multiple-value-bind (body status headers uri)
        (http-response-values
         (http-head "http://localhost:3000/snoop"
                    :onprogress #'(lambda (p)
                                    (setf onprogress-called-p t))))
      (assert-false onprogress-called-p)
      ))
  )

(define-test http-head/no-redirect
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-head "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=301"))
    (assert-equal 200 status)
    (assert-equal "" body))
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-head "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=302"))
    (assert-equal 200 status)
    (assert-equal "" body))
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-head "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=301"
                  :no-redirect t))
    (assert-equal 301 status)
    (assert-equal "http://localhost:3000/snoop"
        ($ headers "Location")))
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-head "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=302"
                  :no-redirect t))
    (assert-equal 302 status)
    (assert-equal "http://localhost:3000/snoop"
        ($ headers "Location")))
  )

(define-test http-head/no-redirect-callback
  (let (body status headers uri)
    (http-response-wait
     (http-head "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=302"
                :no-redirect t
                :oncomplete #'(lambda (&rest args)
                                (setf (values body status headers uri)
                                      (values-list args)))))
    (assert-equal 302 status)
    (assert-equal "http://localhost:3000/snoop"
        ($ headers "Location")))
  )

(define-test http-head/oncomplete
  (let (oncomplete-called-p
        onerror-called-p
        body status headers uri
        error)
    (http-response-wait
     (http-head "http://localhost:3000/snoop"
                :oncomplete #'(lambda (&rest args)
                                (setf oncomplete-called-p t)
                                (setf (values body status headers uri)
                                      (values-list args)))
                :onerror #'(lambda (err)
                             (setf onerror-called-p t
                                   error err))))
    (assert-equal nil onerror-called-p)
    (assert-equal nil error)
    (assert-equal t oncomplete-called-p)
    (assert-equal "" body)
    (assert-equal 200 status)
    (assert-equal "application/json; charset=utf-8"
        ($ headers "content-type"))
    (assert-equal "http://localhost:3000/snoop"
        uri)
    )
  )

(define-test http-head/async-onerror
  (let (oncomplete-called-p
        onerror-called-p
        body status headers uri
        error)
    (http-response-wait
     (http-head "http://localhost:3000/disconnect"
                :oncomplete #'(lambda (&rest args)
                                (setf oncomplete-called-p t)
                                (setf (values body status headers uri)
                                      (values-list args)))
                :onerror #'(lambda (err)
                             (setf onerror-called-p t
                                   error err)))
     :signal-error nil)
    (assert-equal t onerror-called-p)
    (assert-type http-request-error error)
    (assert-equal :invalid-server-response
        (http-error-keyword error))
    (assert-equal nil oncomplete-called-p)
    )
  )

(define-test http-head/async-handler-error
  (let (onerror-called-p
        error)
    (http-response-wait
     (http-head "http://localhost:3000/snoop"
                :oncomplete #'(lambda (&rest args)
                                (plain-error "hoge"))
                :onerror #'(lambda (err)
                             (setf onerror-called-p t
                                   error err)))
     :signal-error nil)
    (assert-equal t onerror-called-p)
    (assert-type plain-error error)
    (assert-equal "hoge"
        (plain-error-format-string error))
    )
  )
