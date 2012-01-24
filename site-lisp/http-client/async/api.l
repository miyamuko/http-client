; -*- mode: lisp; package: http-client.async; encoding: shift_jis -*-

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
  (require "http-client/async/package")
  (require "http-client/async/types")
  (require "http-client/async/utils")
  (require "http-client/async/events")
  )

(in-package :http-client.async)

(export '(parse-url

          http-get
          http-request

          http-pause
          http-resume

          http-request-abort
          http-request-write
          http-request-end

          http-response-status-code
          http-response-version
          http-response-headers

          ))

(defun parse-url (url)
  (multiple-value-bind (scheme user pass host port path extra secure-p)
      (winhttp:crack-url url)
    `(:hostname ,host
      :port ,port
      :path ,(format nil "~A~A" (or path "/") (or extra ""))
      :auth ,(when (and user pass)
               (format nil "~A:~A" user pass))
      :secure ,secure-p
      )))

(defun http-get (option-plist &optional callback)
  (setf (getf option-plist :method) :GET)
  (http-request option-plist callback))

(defun http-request (option-plist &optional callback)
  (multiple-value-bind (host hostname port method path headers auth secure)
      (option-values option-plist)
    (let* ((session (get-session-create :async t))
           (connection (get-connection-create session (or hostname host) port))
           (request (winhttp:open-request connection method path
                                          :accept "*/*"
                                          :secure secure))
           (context (make-context)))
      ;; header
      (when headers
        (winhttp:add-request-headers request headers))
      (when auth
        (multiple-value-bind (auth-scheme user pass)
            (parse-auth auth)
          (winhttp:set-credentials request :server auth-scheme user pass)))
      ;; callback
      (setf (winhttp:request-context request) context)
      (winhttp:set-status-callback request 'request-callback)
      (when callback
        (on :response request callback))
      ;; send request
      (with-job-queue request
        (winhttp:send-request request))
      request
      )))


(defun http-pause (response)
  (let ((ctx (winhttp:request-context response)))
    (setf (context-paused-p ctx) t)))

(defun http-resume (response)
  (let ((ctx (winhttp:request-context response)))
    (setf (context-paused-p ctx) nil)
    (when-let (events (context-paused-events ctx))
      (setf (context-paused-events ctx) nil)
      (dolist (event events)
        (destructuring-bind (event args) event
          (apply 'request-callback event response args)))
      )))


(defun request-callback (event req &rest args)
  (let ((ctx (winhttp:request-context req)))
    (cond ((context-paused-p ctx)
           (put-paused-events ctx (list event args)))
          (t
           (setf (context-last-event ctx) event)
;           (msgbox "~S ~S~%~%~S" event args req)
           (request-callback1 ctx event req args))
          )))

(defun request-callback1 (ctx event req args)
  (destructuring-case `(,event ,req ,@args)
    ;; 100 �ԑ�
    ((:intermediate-response req status)
     (case status
       ;; 100 Continue
       (100 (emit ctx :continue))
       ;; 101 Switching Protocols
       (101 (emit ctx :upgrade req))
       ;; 102 Processing
       ;; nop
       ))
    ;; ���N�G�X�g�w�b�_���M����
    ((:send-request-complete req)
     (emit ctx :send nil)
     (when-let (job (take-waiting-jobs ctx))
       (funcall job)))
    ;; ���N�G�X�g�{�f�B���M
    ((:write-complete req n)
     (emit ctx :send n)
     (when-let (job (take-waiting-jobs ctx))
       (funcall job)))
    ;; ���X�|���X�w�b�_��M����
    ((:headers-available req)
     (emit ctx :response req)
     (winhttp:query-data-available req))
    ;; ���X�|���X�{�f�B��M�`�F�b�N
    ((:data-available req n)
     (cond ((<= n 0)
            ;; EOF reached
            (emit ctx :end)
            (winhttp:close-handle req))
           (t
            ;; Data available
            (winhttp:read-data req n))))
    ;; ���X�|���X�{�f�B��M
    ((:read-complete req data n)
     (emit ctx :data data)
     (winhttp:query-data-available req))
    ;; ���M�G���[
    ((:request-error req type error)
     (winhttp:close-handle req)
     (emit ctx :error error))
    ;; �R�[���o�b�N���ŃG���[
    ((:callback-error req error)
     (winhttp:close-handle req)
     (emit ctx :error error))
    ;; Ctrl-g �������ꂽ
    ((:callback-quit req error)
     (winhttp:close-handle req)
     (emit ctx :error error))
    ))

(defun http-request-abort (request)
  (let ((ctx (winhttp:request-context request)))
    (unless (context-aborted-p ctx)
      (setf (context-aborted-p ctx) t)
      (winhttp:close-handle request)
      )))

(defun http-request-write (request chunk &optional (encoding *encoding-utf8n*))
  (with-job-queue request
   (winhttp:write-data request (encode chunk encoding))))

(defun http-request-end (request &optional data (encoding *encoding-utf8n*))
  (when data
    (http-request-write request data encoding))
  (with-job-queue request
   (winhttp:receive-response request)))


(defun http-response-status-code (response)
  (winhttp:query-response-header response :status-code :type :number))

(defun http-response-version (response)
  (when (string-matchp "^HTTP/\\([0-9]+\\).\\([0-9]+\\)"
                       (winhttp:query-response-header response :version))
    (flet ((match-int (n)
             (parse-integer (match-string n))))
      (+ (match-int 1) (/ (match-int 2) 10.0)))))

(defun http-response-headers (response &optional header)
  (if header
      (winhttp:query-response-header response header)
    (parse-raw-headers (winhttp:query-response-header response :raw-headers))))


(provide "http-client/async/api")

;;; End