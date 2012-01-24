; -*- mode: lisp; package: http-client.api; encoding: shift_jis -*-

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "http-client/api/package")
  (require "http-client/api/request-sync")
  (require "http-client/api/request-async")
  )

(in-package :http-client.api)

(export '(*http-user-agent*

          http-get
          http-head
          http-post
          http-put
          http-delete
          http-request

          http-compose-query
          http-compose-form-data
          http-secure-connection-available?
          ))

(defun http-get (server request-uri &rest headers &key
                        request-encoding proxy
                        sink flusher no-redirect
                        secure async
                        oncomplete onerror
                        &allow-other-keys)
  (apply 'http-request "GET" server request-uri nil headers))

(defun http-head (server request-uri &rest headers &key
                         request-encoding proxy no-redirect
                         secure async
                         oncomplete onerror
                         &allow-other-keys)
  (apply 'http-request "HEAD" server request-uri nil headers))

(defun http-post (server request-uri body &rest headers &key
                         request-encoding proxy
                         sink flusher no-redirect
                         secure async
                         oncomplete onerror
                         &allow-other-keys)
  (apply 'http-request "POST" server request-uri body headers))

(defun http-put (server request-uri body &rest headers &key
                        request-encoding proxy
                        sink flusher
                        secure async
                        oncomplete onerror
                        &allow-other-keys)
  (apply 'http-request "PUT" server request-uri body headers))

(defun http-delete (server request-uri &rest headers &key
                           request-encoding proxy
                           sink flusher
                           secure async
                           oncomplete onerror
                           &allow-other-keys)
  (apply 'http-request "DELETE" server request-uri nil headers))

(defun http-request (method server request-uri body &rest headers &key
                            request-encoding proxy
                            sink flusher no-redirect
                            secure async
                            oncomplete onerror
                            &allow-other-keys)
  (apply (if async 'http-request-async 'http-request-sync)
         method server request-uri body headers))


(defun http-compose-query (path params &optional encoding)
  (compose-query (cons path params) encoding))

(defun http-compose-form-data (params port &optional encoding)
  (check-type port (or output-stream null))
  (let* ((boundary (make-boundary))
         (form-data (build-multipart/form-data params encoding boundary)))
    (values (cond (port
                   (write-multipart/form-data (rcurry 'princ port) form-data)
                   port)
                  (t
                   (with-output-to-string (s)
                     (write-multipart/form-data (rcurry 'princ s) form-data))))
            boundary)))

(defun http-secure-connection-available? ()
  t)


(provide "http-client/api/api")

;;; End
