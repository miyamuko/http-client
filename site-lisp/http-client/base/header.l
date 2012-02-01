; -*- mode: lisp; package: http-client.base; encoding: shift_jis -*-

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
  (require "http-client/base/package")
  (require "http-client/base/utils")
  )

(in-package :http-client.base)

(export '(add-default-header
          content-type-header-value
          format-form-data-header
          raw-headers->text
          raw-headers->alist
          ))

(defparameter *content-type-form-urlencoded* "application/x-www-form-urlencoded")
(defparameter *content-type-form-data* "multipart/form-data; boundary=")

(defun add-default-header (request header value)
  (when (and header value
             (not (winhttp:query-request-header request header)))
    (winhttp:add-request-header request header value)))

(defun content-type-header-value (&key urlencoded form-data boundary)
  (cond (urlencoded
         *content-type-form-urlencoded*)
        (form-data
         (concat *content-type-form-data* boundary))
        ))

(defun format-form-data-header (name filename content-type content-transfer-encoding header-plist)
  (let (r)
    (flet ((h (fmt &rest args)
             (push (apply 'format nil fmt args) r)))
      (h "Content-Disposition: form-data; name=\"~A\"~@[; filename=\"~A\"~]" name filename)
      (when content-type
        (h "Content-Type: ~A" content-type))
      (when content-transfer-encoding
        (h "Content-Transfer-Encoding: ~A" content-transfer-encoding))
      (doplist (k v header-plist)
        (when (and k v)
          (h "~A: ~A" k v))))
    (format nil "~{~A\r\n~}\r\n" (nreverse r))
    ))

(defun raw-headers->text (raw-headers)
  (substitute #\LFD #\NUL raw-headers))

(defun raw-headers->alist (raw-headers)
  ;; 最初の要素は HTTP ステータスなどなので cdr する
  (remove-if #'null (cdr (mapcar #'split-field (split-header raw-headers)))))

(defun split-header (raw-headers)
  (when raw-headers
    (split-string raw-headers #\NUL)))

(defun split-field (header)
  (when (and header (string-matchp "^\\([^:]+\\):\\(.+\\)$" header))
    (cons (strip (match-string 1)) (strip (match-string 2)))))


(provide "http-client/base/header")

;;; End
