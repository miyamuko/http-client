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
  (require "http-client/api/types")
  (require "http-client/api/receiver")
  )

(in-package :http-client.api)

(export '(*http-user-agent*
          ;; request functions
          http-get
          http-post
          http-head
          http-delete
          http-put
          http-request
          ;; control functions
          http-request-abort
          http-response-wait
          ;; request predicates
          http-request-aborted-p
          http-request-waiting-p
          http-request-completed-p
          ;; request accessors
          http-request-uri
          http-request-header
          http-request-header-alist
          ;; response accessors
          http-response-result
          http-response-status
          http-response-status-text
          http-response-header
          http-response-header-alist
          http-response-values
          ;; utilities
          http-compose-query
          http-compose-form-data
          ;; End
          ))


(defvar *http-user-agent* (format nil "xyzzy/~A" (software-version)))

;;; request functions

(progn
  (defun http-get #1=(uri &rest args &key
                          headers query encoding
                          auth proxy-auth proxy no-redirect
                          receiver
                          wait
                          oncomplete onabort onerror)
    (apply 'http-request "GET" uri nil args))

  (defun http-post #2=(uri body &rest args &key
                           headers query encoding
                           auth proxy-auth proxy no-redirect
                           receiver
                           wait
                           oncomplete onabort onerror)
    (apply 'http-request "POST" uri body args))

  (defun http-head #1#
    (apply 'http-request "HEAD" uri nil args))

  (defun http-delete #1#
    (apply 'http-request "DELETE" uri nil args))

  (defun http-put #2#
    (apply 'http-request "PUT" uri body args))
  )

(defun http-request (method uri body &rest args &key
                            headers query encoding
                            auth proxy-auth proxy no-redirect
                            (receiver (http-string-receiver))
                            wait
                            oncomplete onabort onerror)
  (multiple-value-bind (content-type content-length body-stream)
      (parse-form-data body encoding)
    (let ((buf (make-buffer))
          (client (make-http-client))
          request ondata)
      (flet ((emit (handler &rest args)
               (when handler
                 (apply handler args)))
             (emit-error (err)
               (set-on-error client err)
               (when err
                 (let ((handler (if (and (typep err 'winhttp:winhttp-condition)
                                         (eq (winhttp:winhttp-condition-error-keyword err)
                                             :operation-cancelled))
                                    onabort onerror)))
                   (when handler
                     (funcall handler err))))))
        (setf request
              (make-http-request
               method
               (http-compose-query uri query encoding)
               :headers headers
               :default-headers `(,(when content-type `(:Content-Type ,content-type))
                                  ,(when content-length `(:Content-Length ,content-length))
                                  (:User-Agent ,*http-user-agent*)
                                  )
               :options `(,(when proxy `(:proxy :proxy-name ,proxy :access-type :named-proxy))
                          ;; デフォルトは :disallow-https-to-http
                          (:redirect-policy ,(if no-redirect :never :always))
                          )
               :credentials `(,(when auth `(:server ,@auth))
                              ,(when proxy-auth `(:proxy ,@proxy-auth))
                              )
               :handlers
               `(:response ,#'(lambda ()
                                (set-on-response client)
                                (setf ondata
                                      (emit receiver
                                            (http-response-status client)
                                            (http-response-header-alist client)
                                            (winhttp:query-response-header request :content-length :type :number)
                                            )))
                 :send ,#'(lambda (n)
                            (if (listen body-stream)
                                (winhttp:write-data request (read-into buf body-stream nil nil))
                              (winhttp:receive-response request)))
                 :data ,#'(lambda (chunk)
                            (emit ondata chunk))
                 :end ,#'(lambda ()
                           (set-on-end client (emit ondata nil))
                           (apply #'emit oncomplete
                                  (multiple-value-list (http-response-values client))))
                 :close ,#'emit-error
                 :error ,#'emit-error
                 )
               ))
        (setf client (make-http-client :request request))
        (winhttp:send-request request :data (read-into buf body-stream nil nil))
        (when wait
          (http-response-wait client))
        client
        ))))


;;; control functions

(defun http-request-abort (client)
  (unless (http-request-completed-p client)
    (let ((ctx (winhttp:request-context (http-client-request client))))
      (setf (context-aborted-p ctx) t))
    (winhttp:close-handle (http-client-request client))
    ))

(defun http-response-wait (client &key nowait no-redraw sleep (timeout 30) (interval 0.1)
                                  (ready-state :complete)
                                  (signal-error t))
  (let ((start (get-internal-real-time)))
    (while (and (http-client-waiting-p client ready-state)
                (not nowait)
                (or (not timeout)
                    (< (/ (- (get-internal-real-time) start) 1000.0)
                       timeout)))
      (if sleep
          (sleep-for interval)
        (sit-for interval no-redraw)))
    (unless (http-client-waiting-p client ready-state)
      (when signal-error
        (when-let (err (http-client-error client))
          (error err))))))


;;; request predicates

(defun http-request-aborted-p (client)
  (let ((ctx (winhttp:request-context (http-client-request client))))
    (context-aborted-p ctx)))

(defun http-request-waiting-p (client)
  (http-client-waiting-p client :complete))

(defun http-request-completed-p (client)
  (or (not (http-request-waiting-p client))
      (http-request-aborted-p client)))


;;; request accessors

(defun http-request-uri (client)
  (http-response-wait client :ready-state :loading)
  (http-client-uri client))

(defun http-request-header (client header)
  (cdr (assoc header (http-request-header-alist client) :test #'equalp)))

(defun http-request-header-alist (client)
  (http-response-wait client :ready-state :loading)
  (http-client-request-header-alist client))


;;; response accessors

(defun http-response-result (client)
  (http-response-wait client)
  (http-client-result client))

(defun http-response-status (client)
  (http-response-wait client :ready-state :loading)
  (http-client-status-code client))

(defun http-response-status-text (client)
  (http-response-wait client :ready-state :loading)
  (http-client-status-text client))

(defun http-response-header (client header)
  (cdr (assoc header (http-response-header-alist client) :test #'equalp)))

(defun http-response-header-alist (client)
  (http-response-wait client :ready-state :loading)
  (http-client-response-header-alist client))

(defun http-response-values (client)
  (values (http-response-result client)
          (http-response-status client)
          (http-response-header-alist client)
          (http-request-uri client)
          ))


;;; utilities

(defun http-compose-query (path params &optional encoding)
  (add-query-params path params :encoding encoding))

(defun http-compose-form-data (params port &optional encoding)
  (check-type port (or output-stream null))
  (let* ((boundary (make-boundary))
         (form-data (build-multipart/form-data params encoding boundary))
         (input-stream (make-form-data-input-stream form-data)))
    (values (cond (port
                   (copy-stream input-stream port)
                   port)
                  (t
                   (with-output-to-string (s)
                     (copy-stream input-stream s))))
            boundary)))


(provide "http-client/api/api")

;;; End
