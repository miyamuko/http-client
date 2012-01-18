; -*- mode: lisp; package: http-client.gauche; encoding: shift_jis -*-

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
  (require "http-client/gauche/package")
  (require "http-client/gauche/utils")
  )

(in-package :http-client.gauche)

(export '(*http-user-agent*

          http-get
          http-head
          http-post
          http-put
          http-delete

          http-compose-query
          http-compose-form-data
          http-secure-connection-available?
          ))

(defvar *http-user-agent* (format nil "xyzzy.http/~A" (software-version)))

(defun http-get (server request-uri &rest headers &key request-encoding proxy sink flusher no-redirect secure async &allow-other-keys)
  (apply 'http-request server request-uri "GET" headers))

(defun http-head (server request-uri &rest headers &key request-encoding proxy no-redirect secure async &allow-other-keys)
  (apply 'http-request server request-uri "HEAD" headers))

(defun http-post (server request-uri body &rest headers &key request-encoding proxy sink flusher no-redirect secure async &allow-other-keys)
  (apply 'http-request server request-uri "POST" headers))

(defun http-put (server request-uri body &rest headers &key request-encoding proxy sink flusher secure async &allow-other-keys)
  (apply 'http-request server request-uri "PUT" headers))

(defun http-delete (server request-uri &rest headers &key request-encoding proxy sink flusher secure async &allow-other-keys)
  (apply 'http-request server request-uri "DELETE" headers))


(defun http-request (server request-uri method
                            &rest headers
                            &key request-encoding proxy body sink flusher no-redirect secure async
                            &allow-other-keys)
  (remove-from-plistf headers :request-encoding :proxy :body :sink :flusher :no-redirect :secure :async)
  (unless sink
    (setf sink (make-default-sink)))
  (unless flusher
    (setf flusher (make-default-flusher)))
  (multiple-value-bind (host port)
      (parse-server server :secure secure)
    (let ((session (get-session-create :async async)))
      (winhttp:with-connect (connection session host port)
        (winhttp:with-open-request (request connection method request-uri :secure secure)
          ;; header
          (when headers
            (winhttp:add-request-headers request headers))
          (unless (winhttp:query-request-header request :user-agent)
            (winhttp:add-request-headers request `(:user-agent ,*http-user-agent*) :add-if-new))
          ;; TODO async
          ;; TODO proxy
          ;; TODO no-redirect
          ;; TODO request-encoding (body request-uri)
          ;; TODO body (write-data)
          ;; send request
          (winhttp:send-request request :data body)
          ;; receive response
          (winhttp:receive-response request)
          (loop
            (let ((n (winhttp:query-data-available request)))
              (when (<= n 0)
                (return))
              (let ((chunk (winhttp:read-data request n)))
                (princ chunk sink))))
          (let ((headers (merge-header-alist
                          (parse-raw-headers (winhttp:query-response-header request :raw-headers)))))
            (values (winhttp:query-response-header request :status-code)
                    headers
                    (funcall flusher sink headers)
                    )))))))

(defun http-compose-query (path params &optional encoding)
  )

(defun http-compose-form-data (params port &optional encoding)
  )

(defun http-secure-connection-available? ()
  t)


(provide "http-client/gauche/api")

;;; End
