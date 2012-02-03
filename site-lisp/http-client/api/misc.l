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
  )

(in-package :http-client.api)

(defun set-proxy (request uri proxy proxy-supplied-p)
  (multiple-value-bind (proxy-name proxy-bypass access-type)
      (get-proxy-option request uri proxy proxy-supplied-p)
  (winhttp:set-option request :proxy
                      :access-type access-type
                      :proxy-name proxy-name
                      :proxy-bypass proxy-bypass)))

(defun get-proxy-option (request uri proxy proxy-supplied-p)
  (cond (proxy-supplied-p
         (etypecase proxy
           (null (values nil nil :no-proxy))
           (string (values proxy nil :named-proxy))
           (list (values-list proxy))))
        (t
         (get-proxy-by-ie-config (winhttp:connection-parent
                                  (winhttp:request-parent request))
                                 uri))
        ))


(defun detect-encoding (encoding headers)
  (cond ((char-encoding-p encoding)
         encoding)
        ((or (not encoding)
             (eq encoding :binary))
         nil)
        (t
         (detect-encoding-by-content-type headers))
        ))


(provide "http-client/api/misc")

;;; End
