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

(defun http-string-receiver (&key (encoding t))
  #'(lambda (status headers content-length)
      (let ((sink (if content-length
                      (si:*make-string-output-stream-from-string (make-buffer content-length))
                    (make-string-output-stream))))
        (make-stream-callback sink #'get-output-stream-string
                              :encoding (detect-encoding encoding headers)
                              :normalize-eol-code t
                              :enclose t)
        )))

(defun http-file-receiver (filename &key (encoding :binary) (if-exists :new-version) (share nil))
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

(defun http-buffer-receiver (buffer &key (encoding t))
  #'(lambda (status headers content-length)
      (let ((buffer (if (and (bufferp buffer) (not (deleted-buffer-p buffer)))
                        buffer
                      (create-new-buffer buffer))))
        (let ((sink (make-buffer-stream buffer))
              (encoding (detect-encoding encoding headers)))
          (when encoding
            (set-buffer-fileio-encoding encoding buffer))
          (make-stream-callback sink #'(lambda (_)
                                         (fix-eol-code buffer :normalize (not (null encoding)))
                                         buffer)
                                :encoding encoding
                                ;; buffer-eol-code を設定する必要があるので、eol-code は flusher で変換
                                :normalize-eol-code nil)
          ))))

(defun http-oport-receiver (sink flusher &key (encoding t) close finish-output)
  (check-type sink output-stream)
  #'(lambda (status headers content-length)
      (make-stream-callback sink flusher
                            :encoding (detect-encoding encoding headers)
                            :normalize-eol-code t
                            :close close
                            :finish-output finish-output)
      ))

(defun http-null-receiver ()
  #'(lambda (status headers content-length)
      #'(lambda (chunk)
          ;; do nothing
          )
      ))

(defun http-general-receiver (callback &key (encoding t) line)
  #'(lambda (status headers content-length)
      (let ((encoding (detect-encoding encoding headers)))
        (cond ((or encoding line)
               (make-line-callback callback :encoding encoding :normalize-eol-code t))
              (t
               callback)
              ))))

(defmacro http-cond-receiver ((status headers content-length) &body forms)
  `#'(lambda (,status ,headers ,content-length)
       (funcall (cond ,@forms)
                ,status ,headers ,content-length)))
(setf (get 'http-cond-receiver 'ed:lisp-indent-hook) 1)

(defun make-stream-callback (sink flusher &key normalize-eol-code encoding enclose (close t) (finish-output t))
  (check-type sink output-stream)
  (unless flusher
    (setf flusher #'identity))
  (labels ((finish-stream ()
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
                 (close sink))))
           (write-stream (chunk)
             (if chunk
                 (princ chunk sink)
               (finish-stream))))
    (if encoding
        ;; chunk はバイト単位で文字の途中で分割されているかもしれないため、
        ;; 行単位でエンコーディングの変換を行う
        (make-line-callback #'write-stream
                            :normalize-eol-code normalize-eol-code
                            :encoding encoding)
      #'write-stream)))

(defun make-line-callback (callback &key encoding normalize-eol-code)
  (let (buffer)
    #'(lambda (chunk)
        (setf buffer (call-with-each-line (concat buffer chunk)
                                          (if encoding
                                              #'(lambda (line)
                                                  (call-with-each-line (decode line encoding) callback
                                                                       :normalize-eol-code normalize-eol-code
                                                                       :eos-p t))
                                            callback)
                                          :normalize-eol-code normalize-eol-code
                                          :eos-p (not chunk)))
        (unless chunk
          (funcall callback nil)))))

(defun call-with-each-line (buffer callback &key normalize-eol-code eos-p)
  (while (string-match "\r\n\\|\r\\|\n" buffer)
    (let ((before (if normalize-eol-code
                      (concat (substring buffer 0 (match-beginning 0)) "\n")
                    (substring buffer 0 (match-end 0))))
          (after (substring buffer (match-end 0))))
      (funcall callback before)
      (setf buffer after)))
  ;; ストリームの終端に達した場合 (chunk が nil)、
  ;; 残っているバッファの内容をコールバックに伝える
  (when (and eos-p (< 0 (length buffer)))
    (funcall callback buffer)
    (setf buffer nil))
  buffer)


(provide "http-client/api/receiver")

;;; End
