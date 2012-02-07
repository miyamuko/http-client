; -*- mode: lisp; package: http-client.test.api-http-post; encoding: shift_jis -*-

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

(defpackage :http-client.test.api-http-post
  (:use :lisp :editor
   :lisp-unit
   :json
   :http-client
   :http-client.test.helper
   )
  )

(in-package :http-client.test.api-http-post)

(remove-all-tests :http-client.test.api-http-post)

(define-test http-post
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/snoop" "foo=bar"))
    (assert-equal 200 status)
    (assert-equal "application/json; charset=utf-8"
        ($ headers "content-type"))
    (let ((json (json-decode body)))
      (assert-equal "POST" ($ json "method"))
      (assert-equal nil ($ json "query"))
      (assert-equal '(("foo" . "bar")) ($ json "body"))
      (assert-equal (format nil "xyzzy/~A" (software-version))
          ($ json "headers" "user-agent"))
      (assert-equal "application/x-www-form-urlencoded"
          ($ json "headers" "content-type"))
      (assert-equal "7"
          ($ json "headers" "content-length"))
      )
    )
  )

(define-test http-post/headers
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/snoop" "foo=bar"
                  :headers `(:X-Yzzy-Version ,(software-version)
                             :X-Hoge 1
                             :X-Hoge 2
                             :X-Hoge 3)))
    (let ((json (json-decode body)))
      (assert-equal (software-version) ($ json "headers" "X-Yzzy-Version"))
      ;; Express がまとめてる?
      (assert-equal "1, 2, 3" ($ json "headers" "X-Hoge"))
      (assert-equal '(("foo" . "bar")) ($ json "body"))
      )
    )
  )

(define-test http-post/query
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/snoop" "foo=bar"
                  :query '((q "xyzzy 読み方") (num 50))
                  :encoding *encoding-sjis*))
    (let ((json (json-decode body)))
      (assert-equal "xyzzy%20%93%C7%82%DD%95%FB" ($ json "query" "q"))
      (assert-equal "50" ($ json "query" "num"))
      (assert-equal '(("foo" . "bar")) ($ json "body"))
      )
    )
  )

(define-test http-post/urlencoded
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/snoop"
                  '((name1 "hoge")
                    (name2 "fuga"))))
    (let ((json (json-decode body)))
      (assert-equal '(("name1" . "hoge")
                      ("name2" . "fuga"))
          ($ json "body"))
      (assert-equal "application/x-www-form-urlencoded"
          ($ json "headers" "content-type"))
      (assert-equal "21"
          ($ json "headers" "content-length"))
      ))
  )

(define-test http-post/urlencoded-progress
  (let (request-progress-list
        response-progress-list)
    (multiple-value-bind (body status headers uri)
        (http-response-values
         (http-post "http://localhost:3000/snoop"
                    '((name1 "hoge")
                      (name2 "fuga"))
                    :onprogress #'(lambda (p)
                                    (if (http-progress-response-p p)
                                        (push p response-progress-list)
                                      (push p request-progress-list)))))
      (assert-true (every 'http-progress-p request-progress-list))
      (assert-true (every 'http-progress-p response-progress-list))
      (assert-equal 100 (http-progress-percent (car request-progress-list)))
      (assert-equal 100 (http-progress-percent (car response-progress-list)))
      )))

(define-test http-post/multipart
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/snoop"
                  '((name1 :value "hoge")
                    (name2 :value "fuga"))))
    (let ((json (json-decode body)))
      (assert-equal '(("name1" . "hoge")
                      ("name2" . "fuga"))
          ($ json "body"))
      (assert-string-match "^multipart/form-data; boundary=.+?$"
          ($ json "headers" "content-type"))
      ))
  )

(define-test http-post/multipart-progress
  (let (request-progress-list
        response-progress-list)
    (multiple-value-bind (body status headers uri)
        (http-response-values
         (http-post "http://localhost:3000/snoop"
                    '((name1 :value "hoge")
                      (name2 :value "fuga"))
                    :onprogress #'(lambda (p)
                                    (if (http-progress-response-p p)
                                        (push p response-progress-list)
                                      (push p request-progress-list)))))
      (assert-true (every 'http-progress-p request-progress-list))
      (assert-true (every 'http-progress-p response-progress-list))
      (assert-equal 100 (http-progress-percent (car request-progress-list)))
      (assert-equal 100 (http-progress-percent (car response-progress-list)))
      )))

(define-test http-post/file-upload
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/snoop"
                  '((file1 :file "test-file.zip")
                    (file2 :file "test-file-crlf-sjis.bin")
                    (name1 "hoge")
                    (name2 "fuga"))))
    (let ((json (json-decode body)))
      (assert-equal '(("name1" . "hoge")
                      ("name2" . "fuga"))
          ($ json "body"))
      (let ((path ($ json "files" "file1" "path")))
        (assert-equal
            (md5-file "test-file.zip")
            (md5-file path))
        (delete-file path))
      (let ((path ($ json "files" "file2" "path")))
        (assert-equal
            (md5-file "test-file-crlf-sjis.bin")
            (md5-file path))
        (delete-file path))
      ))
  )

(define-test http-post/file-upload-progress
  (let (request-progress-list
        response-progress-list)
    (multiple-value-bind (body status headers uri)
        (http-response-values
         (http-post "http://localhost:3000/snoop"
                    '((file1 :file "../api/api.lc"))
                    :onprogress #'(lambda (p)
                                    (if (http-progress-response-p p)
                                        (push p response-progress-list)
                                      (push p request-progress-list)))))
      (assert-true (every 'http-progress-p request-progress-list))
      (assert-true (every 'http-progress-p response-progress-list))
      (assert-equal 100 (http-progress-percent (car request-progress-list)))
      (assert-equal 100 (http-progress-percent (car response-progress-list)))
      (let ((json (json-decode body)))
        (let ((path ($ json "files" "file1" "path")))
          (assert-equal
              (md5-file "../api/api.lc")
              (md5-file path))
          (delete-file path))
        )))
  )

(define-test http-post/no-redirect
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=301"
                  "foo=bar"))
    (assert-equal 200 status)
    (let ((json (json-decode body)))
      (assert-equal nil ($ json "body"))
      (assert-equal "GET" ($ json "method"))))
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=302"
                  "foo=bar"))
    (assert-equal 200 status)
    (let ((json (json-decode body)))
      (assert-equal nil ($ json "body"))
      (assert-equal "GET" ($ json "method"))))
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=301"
                  "foo=bar"
                  :no-redirect t))
    (assert-equal 301 status)
    (assert-equal "http://localhost:3000/snoop"
        ($ headers "Location")))
  (multiple-value-bind (body status headers uri)
      (http-response-values
       (http-post "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=302"
                  "foo=bar"
                  :no-redirect t))
    (assert-equal 302 status)
    (assert-equal "http://localhost:3000/snoop"
        ($ headers "Location")))
  )

(define-test http-post/no-redirect-callback
  (let (body status headers uri)
    (http-response-wait
     (http-post "http://localhost:3000/redirect?url=http://localhost:3000/snoop&status=302"
                "foo=bar"
                :no-redirect t
                :oncomplete #'(lambda (&rest args)
                                (setf (values body status headers uri)
                                      (values-list args)))))
    (assert-equal 302 status)
    (assert-equal "http://localhost:3000/snoop"
        ($ headers "Location")))
  )

(define-test http-post/oncomplete
  (let (oncomplete-called-p
        onerror-called-p
        body status headers uri error)
    (http-response-wait
     (http-post "http://localhost:3000/snoop"
                "foo=bar"
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
    (let ((json (json-decode body)))
      (assert-equal "POST" ($ json "method"))
      (assert-equal '(("foo" . "bar")) ($ json "body")))
    (assert-equal 200 status)
    (assert-equal ($ headers "content-type")
        "application/json; charset=utf-8")
    (assert-equal "http://localhost:3000/snoop" uri)
    )
  )

(define-test http-post/async-onerror
  (let (oncomplete-called-p
        onerror-called-p
        body status headers uri error)
    (http-response-wait
     (http-post "http://localhost:3000/disconnect"
                "foo=bar"
                :oncomplete #'(lambda (&rest args)
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

(define-test http-post/async-handler-error
  (let (onerror-called-p
        error)
    (http-response-wait
     (http-post "http://localhost:3000/snoop"
                "foo=bar"
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
