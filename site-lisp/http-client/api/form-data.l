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

;; '((:data "..." :size 3)
;;   (:file "a.txt" :size 3000))
(defun build-multipart/form-data (body request-encoding boundary)
  (let (*result*)
    (declare (special *result*))
    (dolist (param body)
      (destructuring-bind (name &rest value-plist)
          param
        (add-data "--~A\r\n" boundary)
        (if (= 1 (length value-plist))
            (build-multipart/form-data-value name (car value-plist) request-encoding nil nil nil)
          (let ((keys '(:value :file :content-type :content-transfer-encoding)))
            (multiple-value-bind (value file content-type content-transfer-encoding)
                (values-plist value-plist keys)
              (setf value-plist (apply 'remove-from-plist value-plist keys))
              (funcall (if file
                           'build-multipart/form-data-file
                         'build-multipart/form-data-value)
                       name (or file value) request-encoding
                       content-type content-transfer-encoding value-plist))))
        (add-data "\r\n")
        ))
    (add-data "--~A--\r\n" boundary)
    (nreverse *result*)))

(defun build-multipart/form-data-file (name filename request-encoding
                                            content-type content-transfer-encoding header-plist)
  (setq file (namestring filename))
  (when (special-file-p filename)
    (error "~a�̓f�o�C�X�t�@�C���ł�" filename))
  (when (file-directory-p filename)
    (error "~a�̓f�B���N�g���ł�" filename))
  (unless (and (valid-path-p filename)
               (file-exist-p filename))
    (error "~a\n�t�@�C���܂��̓f�B���N�g����������܂���" filename))
  (let* ((basename (file-namestring file))
         (headers (format-form-data-header name (encode basename request-encoding)
                                           content-type
                                           content-transfer-encoding
                                           header-plist)))
    (add-data headers)
    (add-file file)))

(defun build-multipart/form-data-value (name value request-encoding
                                             content-type content-transfer-encoding header-plist)
  (let ((headers (format-form-data-header name nil
                                          content-type
                                          content-transfer-encoding
                                          header-plist)))
    (add-data "~A~A" headers (encode value request-encoding))))

(defun write-multipart/form-data (writer form-data)
  (dolist (form form-data)
    (multiple-value-bind (file data)
        (values-plist form '(:file :data))
      (cond (data
             (funcall writer data))
            (t
             (let ((buf (make-write-buffer)))
               (with-open-file (f file :direction :input :encoding :binary)
                 (while (read-into buf f nil nil)
                   (funcall writer buf))))
             )))))

(defun write-multipart/form-data-async (writer form-data)
  (destructuring-bind (form-data &rest rest)
      form-data
    (if (write-multipart/form-data-async1 writer form-data)
        rest
      (write-multipart/form-data-async writer rest))))

(defun write-multipart/form-data-async1 (writer form)
  (multiple-value-bind (file data)
      (values-plist form '(:file :data))
    (cond (data
           (funcall writer data)
           t)
          (t
           (let ((buf (make-write-buffer)))
             (when (stringp file)
               (setf file (open file :direction :input :encoding :binary))
               (setf (getf form :file) file))
             (cond ((read-into buf file nil nil)
                    (funcall writer buf)
                    t)
                   (t
                    (close file)
                    nil))))
          )))

(defun make-write-buffer (&optional (size winhttp:+default-write-buffer-size+))
  (make-vector size :element-type 'character :fill-pointer 0))

(defun calc-multipart/form-data-size (form-data)
  (reduce #'(lambda (acc form)
              (incf acc (getf form :size)))
          form-data
          :initial-value 0
          ))

(defun make-boundary (&optional (len 50))
  ;; �Q�l
  ;;   http://suika.fam.cx/~wakaba/wiki/sw/n/boundary
  ;;   http://tools.ietf.org/html/rfc3548
  ;;   http://tools.ietf.org/html/rfc2046
  (let ((r nil)
        (prefix "boundary-"))
    (dotimes (i (- len (length prefix)))
      (push (random 36) r))
    (format nil "~A~{~36,R~}" prefix r)))


(defun add-data (fmt &rest args)
  (let* ((data (if args
                   (apply 'format nil fmt args)
                 fmt))
         (size (string-byte-size data)))
    (or (when-let (head (car *result*))
          (multiple-value-bind (curr-data curr-size)
              (values-plist head '(:data :size))
            (when curr-data
              (setf (getf head :data) (concat curr-data data)
                    (getf head :size) (+ curr-size size)))))
        (push (list :data data :size size) *result*))))

(defun add-file (file)
  (push (list :file file :size (file-length file)) *result*))

(defun string-byte-size (str)
  (1- (si:chunk-size (si:make-string-chunk str))))


(provide "http-client/api/form-data")

;;; End