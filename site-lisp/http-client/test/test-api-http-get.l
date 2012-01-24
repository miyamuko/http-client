; -*- mode: lisp; package: http-client.test.api-http-get; encoding: shift_jis -*-

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
;; xl-winhttp/test/httpd.js を利用したテスト。
;;

;;; Code:

(require "lisp-unit")

(require "json")
(require "http-client")
(require "http-client/test/helper")

(defpackage :http-client.test.api-http-get
  (:use :lisp :editor
   :lisp-unit
   :json
   :http-client
   :http-client.test.helper
   )
  )

(in-package :http-client.test.api-http-get)

(remove-all-tests :http-client.test.api-http-get)

(define-test http-client/api-http-get
  (multiple-value-bind (status headers body)
      (http-get "localhost:3000" "/snoop")
    (assert-equal "200" status)
    (assert-equal '("application/json; charset=utf-8")
        ($ headers "content-type"))
    (let ((json (json-decode body)))
      (assert-equal "GET" ($ json "method"))
      (assert-equal nil ($ json "query"))
      (assert-equal nil ($ json "body"))
      (assert-equal (format nil "xyzzy.http/~A" (software-version))
          ($ json "headers" "user-agent"))
      )
    )
  )

(define-test http-client/api-http-get/header
  (multiple-value-bind (status headers body)
      (http-get "localhost:3000" "/snoop"
                :X-Yzzy-Version (software-version)
                :X-Hoge 1
                :X-Hoge 2
                :X-Hoge 3)
    (let ((json (json-decode body)))
      (assert-equal (software-version) ($ json "headers" "X-Yzzy-Version"))
      ;; Express がまとめてる?
      (assert-equal "1, 2, 3" ($ json "headers" "X-Hoge"))
      )
    )
  )

(define-test http-client/api-http-get/query
  (multiple-value-bind (status headers body)
      (http-get "localhost:3000" '("/snoop" (q "xyzzy 読み方") (num 50))
                :request-encoding *encoding-sjis*)
    (let ((json (json-decode body)))
      (assert-equal "xyzzy%20%93%C7%82%DD%95%FB" ($ json "query" "q"))
      (assert-equal "50" ($ json "query" "num"))
      )
    )
  )

(define-test http-client/api-http-get/no-redirect
  (multiple-value-bind (status headers body)
      (http-get "localhost:3000" "/redirect?url=http://localhost:3000/snoop&status=301")
    (assert-equal "200" status)
    (assert-equal "GET" ($ (json-decode body) "method")))
  (multiple-value-bind (status headers body)
      (http-get "localhost:3000" "/redirect?url=http://localhost:3000/snoop&status=302")
    (assert-equal "200" status)
    (assert-equal "GET" ($ (json-decode body) "method")))
  (multiple-value-bind (status headers body)
      (http-get "localhost:3000" "/redirect?url=http://localhost:3000/snoop&status=301"
                :no-redirect t)
    (assert-equal "301" status)
    (assert-equal '("http://localhost:3000/snoop")
        ($ headers "Location")))
  (multiple-value-bind (status headers body)
      (http-get "localhost:3000" "/redirect?url=http://localhost:3000/snoop&status=302"
                :no-redirect t)
    (assert-equal "302" status)
    (assert-equal '("http://localhost:3000/snoop")
        ($ headers "Location")))
  )

(define-test http-client/api-http-get/sink
  (let* ((sink-chunks nil)
         (sink-abort-p nil)
         (sink-close-p nil)
         (sink-flush-p nil)
         (flusher-call-p nil)
         (sink (make-general-output-stream
                #'(lambda (chunk)
                    (push chunk sink-chunks))
                #'(lambda (abort)
                    (setf sink-abort-p abort)
                    (setf sink-close-p t))
                #'(lambda ()
                    (setf sink-flush-p t)))))
    (multiple-value-bind (status headers flusher-result)
        (http-get "localhost:3000" "/snoop"
                  :sink sink
                  :flusher #'(lambda (sink headers)
                               (setf flusher-call-p t)
                               (list sink headers)))
      (assert-equal (list sink headers) flusher-result)
      ;; close は http-get 内では呼ばれない
      (assert-equal nil sink-abort-p)
      (assert-equal nil sink-close-p)
      (assert-equal t sink-flush-p)
      (assert-equal t flusher-call-p)
      (let ((json (json-decode (format nil "~{~A~}" (nreverse sink-chunks)))))
        (assert-equal "GET" ($ json "method"))
        )

      (close sink :abort t)
      (assert-equal t sink-close-p)
      (assert-equal t sink-abort-p)
      )
    )
  )

(define-test http-client/api-http-get/flusher
  (let* ((sink-flush-p nil)
         (sink (make-general-output-stream
                #'(lambda (chunk)) nil
                #'(lambda ()
                    (setf sink-flush-p t)))))
    (multiple-value-bind (status headers flusher-result)
        (http-get "localhost:3000" "/snoop"
                  :sink sink
                  :flusher nil)
      (assert-equal nil flusher-result)
      ;; flusher が nil でも stream は flush されるか
      (assert-equal t sink-flush-p)
      )
    )
  )

(define-test http-client/api-http-get/null-sink-flusher
  (multiple-value-bind (status headers flusher-result)
      (http-get "localhost:3000" "/snoop"
                :sink nil
                :flusher nil)
    (assert-equal nil flusher-result)
    )
  )

(define-test http-client/api-http-get/async-oncomplete
  (let (oncomplete-called-p
        oncomplete-args
        onerror-called-p
        onerror-args)
    (http-get "localhost:3000" "/snoop"
              :async t
              :oncomplete #'(lambda (&rest args)
                              (setf oncomplete-called-p t
                                    oncomplete-args args))
              :onerror #'(lambda (&rest args)
                           (setf onerror-called-p t
                                 onerror-args args)))
    (sleep-for 1)
    (assert-equal nil onerror-called-p)
    (assert-equal nil onerror-args)
    (assert-equal t oncomplete-called-p)
    (assert-equal "200" (nth 0 oncomplete-args))
    (assert-equal '("application/json; charset=utf-8")
        ($ (nth 1 oncomplete-args) "content-type"))
    (let ((json (json-decode (nth 2 oncomplete-args))))
      (assert-equal "GET" ($ json "method")))
    )
  )

(define-test http-client/api-http-get/async-onerror
  (let (oncomplete-called-p
        oncomplete-args
        onerror-called-p
        onerror-args)
    (http-get "localhost:3000" "/disconnect"
              :async t
              :oncomplete #'(lambda (&rest args)
                              (setf oncomplete-called-p t
                                    oncomplete-args args))
              :onerror #'(lambda (err)
                           (setf onerror-called-p t
                                 onerror-args err)))
    (sleep-for 1)
    (assert-equal t onerror-called-p)
    (assert-type winhttp:winhttp-condition onerror-args)
    (assert-equal :invalid-server-response
        (winhttp:winhttp-condition-error-keyword onerror-args))
    (assert-equal nil oncomplete-called-p)
    (assert-equal nil oncomplete-args)
    )
  )

(define-test http-client/api-http-get/async-handler-error
  (let (onerror-called-p
        onerror-args)
    (http-get "localhost:3000" "/snoop"
              :async t
              :oncomplete #'(lambda (&rest args)
                              (plain-error "hoge"))
              :onerror #'(lambda (err)
                           (setf onerror-called-p t
                                 onerror-args err)))
    (sleep-for 1)
    (assert-equal t onerror-called-p)
    (assert-type plain-error onerror-args)
    (assert-equal "hoge"
        (plain-error-format-string onerror-args))
    )
  )
