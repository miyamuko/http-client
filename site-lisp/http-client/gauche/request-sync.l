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
  (require "http-client/gauche/request")
  (require "http-client/gauche/header")
  (require "http-client/gauche/types")
  (require "http-client/gauche/url")
  (require "http-client/gauche/form-data")
  )

(in-package :http-client.gauche)

(defun http-request-sync (server request-uri method
                                 &rest headers
                                 &key request-encoding proxy body
                                 (sink (make-default-sink))
                                 (flusher (make-default-flusher))
                                 no-redirect secure async
                                 &allow-other-keys)
  (check-type server string)
  (check-type request-uri (or string list))
  (check-type method (or string symbol))
  (check-type sink (or output-stream null))
  (remove-from-plistf headers :request-encoding :proxy :body :sink :flusher :no-redirect :secure :async :oncomplete :onerror)
  (multiple-value-bind (host port)
      (parse-server server :secure secure)
    (let ((session (get-session-create :async async)))
      (winhttp:with-connect (connection session host port)
        (winhttp:with-open-request (request connection method
                                            (compose-query request-uri request-encoding)
                                            :secure secure)
          (setup-headers request headers)
          (setup-options request proxy no-redirect)
          (send-request request body request-encoding)
          (receive-response request sink)
          (finish-response request sink flusher)
          )))))

(defun send-request (request body request-encoding)
  (check-type body (or string list))
  (cond ((null body)
         (winhttp:send-request request))
        ((multipart-form-data-p body)
         (let* ((boundary (make-boundary))
                (form-data (build-multipart/form-data body request-encoding boundary))
                (total-length (calc-multipart/form-data-size form-data)))
           (add-content-type-header request :form-data t :boundary boundary)
           (winhttp:send-request request :total-length total-length)
           (write-multipart/form-data (curry #'winhttp:write-data request) form-data)
           ))
        (t
         (add-content-type-header request :urlencoded t)
         (winhttp:send-request request :data (url-encode-params body request-encoding))
         )))


(defun receive-response (request sink)
  (winhttp:receive-response request)
  (long-operation
    (loop
     (let ((n (winhttp:query-data-available request)))
       (when (<= n 0)
         (return))
       (let ((chunk (winhttp:read-data request n)))
         (princ chunk sink)))
     (do-events))))


(provide "http-client/gauche/request-sync")

;;; End
