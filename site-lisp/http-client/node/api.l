; -*- mode: lisp; package: http-client.node; encoding: shift_jis -*-

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
  (require "http-client/node/package")
  (require "http-client/node/types")
  (require "http-client/node/utils")
  (require "http-client/node/events")
  )

(in-package :http-client.node)

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
  (multiple-value-bind (host hostname port method path headers auth)
      (option-values option-plist)
    (let* ((session (get-session-create :async t))
           (connection (get-connection-create session (or hostname host) port))
           (request (winhttp:open-request connection method path :accept "*/*"))
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
    ;; 100 番台
    ((:intermediate-response req status)
     (case status
       ;; 100 Continue
       (100 (emit ctx :continue))
       ;; 101 Switching Protocols
       (101 (emit ctx :upgrade req))
       ;; 102 Processing
       ;; nop
       ))
    ;; リクエストヘッダ送信完了
    ((:send-request-complete req)
     (emit ctx :send nil)
     (when-let (job (take-waiting-jobs ctx))
       (funcall job)))
    ;; リクエストボディ送信
    ((:write-complete req n)
     (emit ctx :send n)
     (when-let (job (take-waiting-jobs ctx))
       (funcall job)))
    ;; レスポンスヘッダ受信完了
    ((:headers-available req)
     (let ((content-length (winhttp:query-response-header req :content-length :type :number)))
       (setf (context-max-read-length ctx) content-length))
     (emit ctx :response req)
     (read-data req ctx))
    ;; レスポンスボディ受信チェック
    ((:data-available req n)
     (cond ((<= n 0)
            ;; EOF reached
            (emit ctx :end)
            (winhttp:close-handle req))
           (t
            ;; Data available
            (winhttp:read-data req n))))
    ;; レスポンスボディ受信
    ((:read-complete req data n)
     (incf (context-total-read-length ctx) n)
     (emit ctx :data data)
     (read-data req ctx))
    ;; 送信エラー
    ((:request-error req type error)
     (winhttp:close-handle req)
     (emit ctx :error error))
    ;; コールバック内でエラー
    ((:callback-error req error)
     (winhttp:close-handle req)
     (emit ctx :error error))
    ;; Ctrl-g を押された
    ((:callback-quit req error)
     (winhttp:close-handle req)
     (emit ctx :error error))
    ))

(defun read-data (req ctx)
  (let ((max-read-length (context-max-read-length ctx))
        (total-read-length (context-total-read-length ctx)))
    (cond ((null max-read-length)
           ;; content-length が分からないなら到着している分だけ読み込む。
           (winhttp:query-data-available req))
          (t
           ;; content-length が分かっているなら、バッファサイズ分を
           ;; 一気に読み込むことでイベントの発生回数を減らす。
           (let ((n (min winhttp:+default-read-buffer-size+
                         (- max-read-length total-read-length))))
             (cond ((<= n 0)
                    ;; EOF reached
                    (emit ctx :end)
                    (winhttp:close-handle req))
                   (t
                    (winhttp:read-data req n))
                   )))
          )))

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


(provide "http-client/node/api")

;;; End
