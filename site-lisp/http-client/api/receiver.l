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
          http-oport-receiver
          http-null-receiver
          http-general-receiver
          http-cond-receiver
          ))

(defun http-string-receiver ()
  #'(lambda (status headers content-length)
      (let ((sink (if content-length
                      (si:*make-string-output-stream-from-string (make-buffer content-length))
                    (make-string-output-stream))))
        (make-stream-callback sink #'get-output-stream-string
                              :enclose t)
        )))

(defun http-file-receiver (filename &key encoding if-exists share)
  ;; default-directory が変わっているかもしれないので、
  ;; この時点でフルパスにしておく
  (setf filename (namestring filename))
  #'(lambda (status headers content-length)
      (let ((sink (open filename :direction :output
                        :encoding (or encoding :binary)
                        :if-exists (or if-exists :new-version)
                        :share share)))
        (make-stream-callback sink #'namestring)
        )))

(defun http-buffer-receiver (buffer)
  #'(lambda (status headers content-length)
      ;; TODO encoding?
      (let* ((buffer (if (and (bufferp buffer) (not (deleted-buffer-p buffer)))
                         buffer
                       (create-new-buffer buffer)))
             (sink (make-buffer-stream buffer)))
        (make-stream-callback sink #'buffer-stream-buffer)
        )))

(defun http-oport-receiver (sink flusher &key close finish-output)
  (check-type sink output-stream)
  #'(lambda (status headers content-length)
      (make-stream-callback sink flusher
                            :close close
                            :finish-output finish-output)
      ))

(defun http-null-receiver ()
  #'(lambda (status headers content-length)
      #'(lambda (chunk)
          ;; do nothing
          )
      ))

(defun http-general-receiver (callback &key line)
  #'(lambda (status headers content-length)
      (if line
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
                  (setf buffer nil)
                  (funcall callback nil))))
        #'(lambda (chunk)
            (funcall callback chunk))
        )))

(defmacro http-cond-receiver ((status headers content-length) &body forms)
  `#'(lambda (,status ,headers ,content-length)
       (funcall (cond ,@forms)
                ,status ,headers ,content-length)))
(setf (get 'http-cond-receiver 'ed:lisp-indent-hook) 1)

(defun make-stream-callback (sink flusher &key enclose (close t) (finish-output t))
  (check-type sink output-stream)
  (unless flusher
    (setf flusher #'identity))
  #'(lambda (chunk)
      (if chunk
          (princ chunk sink)
        (unwind-protect
            (progn
              (when finish-output
                (finish-output sink))
              (let ((r (funcall flusher sink)))
                (if enclose
                    ;; http-client を表示したときにレスポンスの内容が
                    ;; 長々と出力されないようにクロージャで隠蔽する
                    ;; See also: http-response-result
                    #'(lambda () r)
                  r)))
          (when close
            (close sink))))))


(provide "http-client/api/receiver")

;;; End
