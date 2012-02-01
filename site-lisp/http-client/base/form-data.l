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

(export '(parse-form-data
          multipart/form-data-p
          build-multipart/form-data
          make-form-data-input-stream
          calc-multipart/form-data-size
          make-boundary
          ))


(defun parse-form-data (body encoding)
  (check-type body (or string list))
  (cond ((null body)
         (values nil
                 nil
                 (make-form-data-input-stream nil)))
        ((multipart/form-data-p body)
         (let* ((boundary (make-boundary))
                (form-data (build-multipart/form-data body encoding boundary))
                (total-length (calc-multipart/form-data-size form-data)))
           (values (content-type-header-value :form-data t :boundary boundary)
                   total-length
                   (make-form-data-input-stream form-data))
           ))
        (t
         (let ((urlencoded (url-encode-params body encoding)))
           (values (content-type-header-value :urlencoded t)
                   nil
                   (make-form-data-input-stream urlencoded)))
         )))

(defun multipart/form-data-p (body)
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

;; '((:data "..." :size 3)
;;   (:file "a.txt" :size 3000))
(defun build-multipart/form-data (body encoding boundary)
  (let (*result*)
    (declare (special *result*))
    (dolist (param body)
      (destructuring-bind (name &rest value-plist)
          param
        (add-data "--~A\r\n" boundary)
        (if (= 1 (length value-plist))
            (build-multipart/form-data-value name (car value-plist) encoding nil nil nil)
          (let ((keys '(:value :file :content-type :content-transfer-encoding)))
            (multiple-value-bind (value file content-type content-transfer-encoding)
                (values-plist value-plist keys)
              (setf value-plist (apply 'remove-from-plist value-plist keys))
              (funcall (if file
                           'build-multipart/form-data-file
                         'build-multipart/form-data-value)
                       name (or file value) encoding
                       content-type content-transfer-encoding value-plist))))
        (add-data "\r\n")
        ))
    (add-data "--~A--\r\n" boundary)
    (nreverse *result*)))

(defun build-multipart/form-data-file (name filename encoding
                                            content-type content-transfer-encoding header-plist)
  (setq file (namestring filename))
  (when (special-file-p filename)
    (error "~aはデバイスファイルです" filename))
  (when (file-directory-p filename)
    (error "~aはディレクトリです" filename))
  (unless (and (valid-path-p filename)
               (file-exist-p filename))
    (error "~a\nファイルまたはディレクトリが見つかりません" filename))
  (let* ((basename (file-namestring file))
         (headers (format-form-data-header name (encode basename encoding)
                                           content-type
                                           content-transfer-encoding
                                           header-plist)))
    (add-data headers)
    (add-file file)))

(defun build-multipart/form-data-value (name value encoding
                                             content-type content-transfer-encoding header-plist)
  (let ((headers (format-form-data-header name nil
                                          content-type
                                          content-transfer-encoding
                                          header-plist)))
    (add-data "~A~A" headers (encode value encoding))))

(defun add-data (fmt &rest args)
  (let ((data (if args
                  (apply 'format nil fmt args)
                fmt)))
    (push (list :data data :size (string-byte-size data)) *result*)))

(defun add-file (file)
  (push (list :file file :size (file-length file)) *result*))


(defun make-form-data-input-stream (form-data)
  (let ((buf (make-buffer)))
    (make-general-input-stream
     ;; input-fn
     #'(lambda ()
         (etypecase form-data
           (null nil)
           (string
            (prog1
                form-data
              (setf form-data nil)))
           (list
            (while form-data
              (destructuring-bind (form &rest rest)
                  form-data
                (multiple-value-bind (data data-available-p next-p)
                    (next-multipart/form-data form buf)
                  (when next-p
                    (setf form-data rest))
                  (when data-available-p
                    (return data))))))))
     ;; close-fn
     #'(lambda ()
         (setf form-data nil))
     ;; listen-fn
     #'(lambda ()
         (not (null form-data)))
     )))

(defun next-multipart/form-data (form buf)
  (multiple-value-bind (file data)
      (values-plist form '(:file :data))
    (cond (data
           (values data t t))
          (t
           (when (stringp file)
             (setf file (open file :direction :input :encoding :binary :share nil))
             (setf (getf form :file) file))
           (handler-case
               (cond ((read-into buf file nil nil)
                      (values buf t nil))
                     (t
                      (close file)
                      (values nil nil t)))
             (error (c)
               (when (and (streamp file) (open-stream-p file))
                 (close file))
               (si:*throw-error c))))
           )))


(defun calc-multipart/form-data-size (form-data)
  (reduce #'(lambda (acc form)
              (incf acc (getf form :size)))
          form-data
          :initial-value 0
          ))


(defun make-boundary (&key (size 50) (prefix "boundary-"))
  ;; 参考
  ;;   http://suika.fam.cx/~wakaba/wiki/sw/n/boundary
  ;;   http://tools.ietf.org/html/rfc3548
  ;;   http://tools.ietf.org/html/rfc2046
  (let (r)
    (dotimes (i (- size (length prefix)))
      (push (random 36) r))
    (format nil "~A~{~36,R~}" prefix r)))


(provide "http-client/base/form-data")

;;; End
