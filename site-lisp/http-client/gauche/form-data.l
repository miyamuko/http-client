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
  )

(in-package :http-client.gauche)

;; '((:data "..." :size 3)
;;   (:file "a.txt" :size 3000))
(defun build-multipart/form-data (body request-encoding boundary)
  (let (*result*)
    (declare (special *result*))
    (dolist (param body)
      (destructuring-bind (name &rest value-plist)
          param
        (let ((keys '(:value :file :content-type :content-transfer-encoding)))
          (destructuring-bind (value file content-type content-transfer-encoding)
              (mapcar #'(lambda (k) (getf value-plist k)) keys)
            (setf value-plist (apply 'remove-from-plist value-plist keys))
            (add-data "--~A\r\n" boundary)
            (funcall (if file
                         'build-multipart/form-data-file
                       'build-multipart/form-data-value)
                     name (or file value) request-encoding
                     content-type content-transfer-encoding value-plist)
            (add-data "\r\n")
            ))
        ))
    (add-data "--~A--\r\n" boundary)
    (nreverse *result*)))

(defun build-multipart/form-data-file (name file request-encoding
                                            content-type content-transfer-encoding header-plist)
  (let* ((filename (file-namestring file))
         (headers (format-form-data-header name (encode filename request-encoding)
                                           (or content-type "application/octet-stream")
                                           (or content-transfer-encoding "binary")
                                           header-plist)))
    (add-data headers)
    (add-file file)))

(defun build-multipart/form-data-value (name value request-encoding
                                             content-type content-transfer-encoding header-plist)
  (let ((headers (format-form-data-header name nil
                                          (or content-type (format nil "text/plain~@[; charset=~A~]"
                                                                   (encoding->charset request-encoding)))
                                          (or content-transfer-encoding "binary")
                                          header-plist)))
    (add-data "~A~A" headers (encode value request-encoding))))

(defun write-multipart/form-data (writer form-data)
  (dolist (form form-data)
    (let ((file (getf form :file))
          (data (getf form :data)))
      (cond (data
             (funcall writer data))
            (t
             (let ((buf (make-vector 8192 :element-type 'character :fill-pointer 0)))
               (with-open-file (f file :direction :input :encoding :binary)
                 (read-into buf f)
                 (funcall writer buf)))
             )))))

(defun calc-multipart/form-data-size (form-data)
  (reduce #'(lambda (acc form)
              (incf acc (getf form :size)))
          form-data
          :initial-value 0
          ))


(defun add-data (fmt &rest args)
  (let ((data (if args
                  (apply 'format nil fmt args)
                fmt)))
    (push (list :data data :size (string-byte-size data)) *result*)))

(defun add-file (file)
  (push (list :file file :size (file-length file)) *result*))

(defun string-byte-size (str)
  (1- (si:chunk-size (si:make-string-chunk str))))


(provide "http-client/gauche/form-data")

;;; End
