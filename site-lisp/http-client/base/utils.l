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
  )

(in-package :http-client.base)

(export '(values-plist
          parse-raw-headers
          encode
          ))

(defun values-plist (plist keys)
  (values-list
   (mapcar #'(lambda (k)
               (getf plist k))
           keys)))


(defun parse-raw-headers (raw-headers)
  (remove-if #'null (mapcar #'split-field (split-header raw-headers))))

(defun split-header (raw-headers)
  (when raw-headers
    (split-string raw-headers #\NUL)))

(defun split-field (header)
  (when (and header (string-matchp "^\\([^:]+\\):\\(.+\\)$" header))
    (cons (strip (match-string 1)) (strip (match-string 2)))))

(defun strip (str)
  (string-trim " \t\r\n\f" str))

(defun encode (v encoding)
  (typecase v
    (null "")
    (string
     (if encoding
         (convert-encoding-from-internal encoding v)
       v))))


(provide "http-client/base/utils")

;;; End
