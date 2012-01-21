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
  (require "http-client/gauche/header")
  )

(in-package :http-client.gauche)

(defparameter *http-user-agent* (format nil "xyzzy.http/~A" (software-version)))


(defun make-default-sink ()
  (make-string-output-stream))

(defun make-default-flusher ()
  #'(lambda (sink headers)
      (unwind-protect
          (get-output-stream-string sink)
        (close sink))))

(defun setup-headers (request headers)
  (when headers
    (winhttp:add-request-headers request headers))
  (add-default-header request :user-agent *http-user-agent*)
  )

(defun setup-options (request proxy no-redirect)
  (when proxy
    (winhttp:set-option request :proxy :proxy-name proxy :access-type :named-proxy))
  ;; デフォルトは :disallow-https-to-http
  (winhttp:set-option request :redirect-policy (if no-redirect :never :always))
  )

(defun multipart-form-data-p (body)
  ;; multipart/form-data
  ;;   '(("upload" :file "logo.png" :content-type "image/png"))
  ;;   '(("submit" :value "OK"))
  ;; application/x-www-form-encoded
  ;;   '(("submit" "OK"))
  ;;   "submit=OK"
  (and (listp body)
       (some #'(lambda (param)
                 (< 2 (length param)))
             body)))

(defun finish-response (request sink flusher)
  (let ((headers (query-response-header-alist request)))
    (values (winhttp:query-response-header request :status-code)
            headers
            (when flusher
              (funcall flusher sink headers))
            )))


(provide "http-client/gauche/request")

;;; End
