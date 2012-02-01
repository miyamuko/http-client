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

(export '(http-string-receiver
          http-file-receiver
          http-buffer-receiver
          http-null-receiver
          http-stream-chunk-receiver
          http-stream-line-receiver
          ))

(defun http-string-receiver ()
  #'(lambda (status headers content-length)
      (let ((sink (make-string-output-stream)))
        #'(lambda (chunk)
            (if chunk
                (princ chunk sink)
              (get-output-stream-string sink)))
        )))

(defun http-file-receiver (filename &key encoding if-exists share)
  (setf filename (namestring filename))
  #'(lambda (status headers content-length)
      (let ((sink (open filename :direction :output
                        :encoding (or encoding :binary)
                        :if-exists (or if-exists :new-version)
                        :share share)))
        #'(lambda (chunk)
            (if chunk
                (princ chunk sink)
              (unwind-protect
                  filename
                (close sink))))
        )))

(defun http-buffer-receiver (buffer)
  #'(lambda (status headers content-length)
      ;; TODO encoding?
      (let* ((buffer (if (and (bufferp buffer) (not (deleted-buffer-p buffer)))
                         buffer
                       (create-new-buffer buffer)))
             (sink (make-buffer-stream buffer)))
        #'(lambda (chunk)
            (if chunk
                (princ chunk sink)
              buffer))
        )))

(defun http-null-receiver ()
  #'(lambda (status headers content-length)
      #'(lambda (chunk)
          ;; do nothing
          )
      ))

(defun http-stream-chunk-receiver (callback &key if-not-success (http-string-receiver))
  #'(lambda (status headers content-length)
      (if (<= 200 status 299)
          #'(lambda (chunk)
              (funcall callback chunk))
        (funcall if-not-success status headers content-length)
        )))

(defun http-stream-line-receiver (callback &key if-not-success (http-string-receiver))
  #'(lambda (status headers content-length)
      (if (<= 200 status 299)
          (let (buffer)
            #'(lambda (chunk)
                (setf buffer (concat buffer chunk))
                (while (string-match "\r\n\\|\r\\|\n" buffer)
                  (let ((before (substring buffer 0 (match-end 0)))
                        (after (substring buffer (match-end 0))))
                    (funcall callback before)
                    (setf buffer after)))
                ;; ストリームの終端に達した場合 (chunk が nil)、
                ;; 残っているバッファの内容をコールバックに伝える
                (unless chunk
                  (when (string/= "" buffer)
                    (funcall callback buffer))
                  (funcall callback nil)
                  (setf buffer nil))))
        (funcall if-not-success status headers content-length)
        )))


(provide "http-client/api/receiver")

;;; End
