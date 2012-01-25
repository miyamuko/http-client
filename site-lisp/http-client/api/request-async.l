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
  (require "http-client/api/request")
  (require "http-client/api/header")
  (require "http-client/api/types")
  (require "http-client/api/url")
  )

(in-package :http-client.api)

(defun http-request-async (method server request-uri body
                                  &rest headers
                                  &key request-encoding proxy
                                  (sink (make-default-sink))
                                  (flusher (make-default-flusher))
                                  no-redirect secure async
                                  (oncomplete #'(lambda (status-code response-header response-text)
                                                  (message "~A" status-code)))
                                  (onerror #'(lambda (err)
                                               (errbox "Error: ~A" err)))
                                  &allow-other-keys)
  (check-type server string)
  (check-type request-uri (or string list))
  (check-type method (or string symbol))
  (check-type sink (or output-stream null))
  (remove-from-plistf headers :request-encoding :proxy :sink :flusher :no-redirect :secure :async :oncomplete :onerror)
  (multiple-value-bind (scheme _ _ host port path extra secure)
      (winhttp:crack-url (create-url server request-uri
                                     :encoding request-encoding
                                     :secure secure))
    (let* ((session (get-session-create :async async))
          (connection (winhttp:connect session host port))
          (request (winhttp:open-request connection method `(,path ,extra)
                                         :accept "*/*"
                                         :secure secure)))
      (setup-options request proxy no-redirect)
      (setup-headers request headers)
      (multiple-value-bind (content-type content-length form-data multipart-p)
          (build-form-data body request-encoding)
        (when content-type
          (add-default-header request :content-type content-type))
        (when content-length
          (add-default-header request :content-length content-length))
        (http-set-callback request)
        (on-send request
          #'(lambda (n)
              (cond ((and multipart-p form-data)
                     (setf form-data
                           (write-multipart/form-data-async
                            (curry #'winhttp:write-data request)
                            form-data)))
                    (t
                     (winhttp:receive-response request)))))
        (on-data request
          #'(lambda (chunk)
              (princ chunk sink)))
        (on-end request
          #'(lambda ()
                   (multiple-value-call oncomplete
                     (finish-response request sink flusher))))
        (on-close request
          #'(lambda (err)
                     (winhttp:close-handle connection)
                     (when err
                       (funcall onerror err))))
        (on-error request
          #'(lambda (err)
              (funcall onerror err)))
        (if multipart-p
            (winhttp:send-request request)
          (progn
            (winhttp:send-request request :data form-data)
            (setf form-data nil)))
        request))))


(provide "http-client/api/request-async")

;;; End
