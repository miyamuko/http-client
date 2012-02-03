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

(export '(with-selected-buffer
          make-buffer
          values-plist
          encode
          decode
          errbox
          string-byte-size
          ))

(defmacro with-selected-buffer (buffer &body body)
  `(with-set-buffer
     (let ((#1=#:new ,buffer)
           (#2=#:old (selected-buffer)))
       (unwind-protect
           (if (get-buffer-window #1#)
               (with-selected-window
                 (set-window (get-buffer-window #1#))
                 ,@body)
             (progn
               (set-buffer #1#)
               ,@body))
         (set-buffer #2#)))))
(setf (get 'with-selected-buffer 'lisp-indent-hook) 0)


(defun make-buffer (&optional (size winhttp:+default-write-buffer-size+))
  (make-vector size :element-type 'character :fill-pointer 0))

(defun values-plist (plist keys)
  (values-list
   (mapcar #'(lambda (k)
               (getf plist k))
           keys)))

(defun encode (v encoding &optional output-stream)
  (typecase v
    (null "")
    (string
     (if encoding
         (convert-encoding-from-internal encoding v output-stream)
       v))))

(defun decode (v encoding &optional output-stream)
  (typecase v
    (null "")
    (string
     (if encoding
         (convert-encoding-to-internal encoding v output-stream)
       v))))

(defun errbox (fmt &rest args)
  (message-box (apply #'format nil fmt args) nil '(:ok :exclamation)))

(defun string-byte-size (str)
  (1- (si:chunk-size (si:make-string-chunk str))))

(defun strip (str)
  (string-trim " \t\r\n\f" str))


(provide "http-client/base/utils")

;;; End
