; -*- mode: lisp; package: http-client.async; encoding: shift_jis -*-

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
  (require "http-client/async/package")
  (require "http-client/async/agent")
  (require "http-client/async/events")
  (require "http-client/async/types")
  )

(in-package :http-client.async)

(export '(make-http-request
          ))

(defun make-http-request (method uri &key headers default-headers options credentials handlers)
  (check-type method (or string symbol))
  (check-type uri string)
  (multiple-value-bind (scheme _ _ host port path extra secure)
      (winhttp:crack-url uri)
    (let* ((session (get-session-create :async t))
           (connection (winhttp:connect session host port))
           (request (winhttp:open-request connection method `(,path ,extra)
                                          :secure secure)))
      (setup-headers request headers)
      (setup-default-headers request default-headers)
      (setup-options request options)
      (setup-credentials request credentials)
      (http-set-callback request)
      (labels ((cleanup (err)
                 (ignore-errors
                  (winhttp:close-handle connection))
                 err))
        (doplist (k v handlers)
          (if (member k '(:close :error))
              (on k request (compose v #'cleanup))
            (on k request v))
          ))
      request)))


(defun setup-headers (request headers)
  (when headers
    (winhttp:add-request-headers request headers)))

(defun setup-default-headers (request default-headers)
  (dolist (header default-headers)
    (when header
      ;; ‚·‚Å‚É“¯–¼‚Ìƒwƒbƒ_‚ª‚ ‚Á‚½‚ç’Ç‰Á‚µ‚È‚¢
      (apply 'add-default-header request header))))

(defun setup-options (request options)
  (dolist (opt options)
    (when opt
      (apply 'winhttp:set-option request opt))))

(defun setup-credentials (request credentials)
  (dolist (cred credentials)
    (when cred
      (apply 'winhttp:set-credentials request cred))))


(provide "http-client/async/request")

;;; End
