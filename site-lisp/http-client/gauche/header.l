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
  )

(in-package :http-client.gauche)

(defparameter *http-user-agent* (format nil "xyzzy.http/~A" (software-version)))
(defparameter *content-type-form-urlencoded* "application/x-www-form-urlencoded")
(defparameter *content-type-form-data* "multipart/form-data; boundary=")
(defparameter *default-content-type* "application/octet-stream")

(defun merge-header-alist (alist)
  (loop with r
    for (k . v) in alist
    for key = (string-downcase k)
    do (if (assoc key r :test #'equal)
           (appendf (cdr (assoc key r :test #'equal)) (list v))
         (push (list key v) r))
    finally (return (nreverse r))))

(defun add-content-type-header (request &key urlencoded form-data boundary)
  (if urlencoded
      (add-default-header request :content-type *content-type-form-urlencoded*)
    (add-default-header request :content-type (concat *content-type-form-data* boundary))))

(defun add-default-header (request header value)
  (unless (winhttp:query-request-header request header)
    (winhttp:add-request-header request header value)))

(defun format-form-data-header (name filename content-type content-transfer-encoding header-plist)
  (let (r)
    (flet ((h (fmt &rest args)
             (push (apply 'format nil fmt args) r)))
      (h "Content-Disposition: form-data; name=\"~A\"~@[; filename=\"~A\"~]" name filename)
      (h "Content-Type: ~A" content-type)
      (h "Content-Transfer-Encoding: ~A" content-transfer-encoding)
      (doplist (k v header-plist)
        (when (and k v)
          (h "~A: ~A" k v))))
    (format nil "~{~A\r\n~}\r\n" (nreverse r))
    ))

(defun encoding->charset (encoding)
  (when (equal encoding *encoding-utf8n*)
    (setf encoding *encoding-utf8*))
  (cadr (assoc encoding *character-set-alist* :test #'equal :key #'symbol-value)))

(defun make-boundary (&optional (len 60))
  ;; ランダムな文字列を base64 してboundary を生成する。
  ;; base64 はサイズが 4/3 倍になるので、len * 3/4 が
  ;; 生成するランダムな文字列のサイズ。
  ;;
  ;; 参考
  ;;   http://suika.fam.cx/~wakaba/wiki/sw/n/boundary
  ;;   http://tools.ietf.org/html/rfc3548
  ;;   http://tools.ietf.org/html/rfc2046
  (let* ((bytes (ceiling (* len (/ 3 4.0))))
         (bin (make-random-string bytes))
         (base64 (si:base64-encode bin))
         (r (substitute-string base64 "[=\n]" "")))
    (substring r 0 (min len (length r)))))

(defun make-random-string (&optional (size 16))
  (let (r)
    (dotimes (i size)
      (push (random 256) r))
    (map 'string 'code-char (nreverse r))))


(provide "http-client/gauche/header")

;;; End
