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
  (require "http-client/api/conditions")
  (require "http-client/api/types")
  (require "http-client/api/receiver")
  (require "http-client/api/misc")
  )

(in-package :http-client.api)

(export '(*http-user-agent*
          *http-accept-type*
          *http-proxy*
          *http-redirect-policy*
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
          http-response-status
          http-response-status-text
          http-response-header
          http-response-header-alist
          http-response-result
          http-response-values
          ;; utilities
          http-compose-query
          http-compose-form-data
          http-date-from-universal-time
          http-date-to-universal-time
          ))


(defvar *http-user-agent* (format nil "xyzzy/~A" (software-version)))
(defvar *http-accept-type* "*/*")
(defvar *http-proxy* :winhttp)
(defvar *http-redirect-policy* :disallow-https-to-http)

;;; request functions

(progn
  (defun http-get #1=(uri &rest args &key
                          headers query encoding
                          auth proxy-auth proxy no-redirect
                          receiver
                          wait
                          onprogress oncomplete onabort onerror)
    (apply 'http-request "GET" uri nil args))

  (defun http-post #2=(uri body &rest args &key
                           headers query encoding
                           auth proxy-auth proxy no-redirect
                           receiver
                           wait
                           onprogress oncomplete onabort onerror)
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
                            receiver
                            wait
                            onprogress oncomplete onabort onerror)
  (let ((client (make-http-client))
        ok)
    (with-translate-condition
     (unwind-protect
         (prog1
             (apply 'http-request-impl client method uri body args)
           (setf ok t))
       (when (and (not ok)
                  (http-client-request client))
         (http-request-abort client))
       ))))


(defun http-request-impl (client method uri body &rest args &key
                                 headers query encoding
                                 auth proxy-auth
                                 (proxy *http-proxy*)
                                 no-redirect
                                 (receiver (http-string-receiver))
                                 wait
                                 onprogress oncomplete onabort onerror)
  (multiple-value-bind (content-type content-length body-stream)
      (parse-form-data body encoding)
    (let ((buf (make-buffer))
          request ondata
          (request-progress (make-http-progress :response-p nil :total content-length))
          (response-progress (make-http-progress :response-p t))
          )
      (labels ((emit (handler &rest args)
                 (when handler
                   (apply handler args)))
               (emit-error (err)
                 (setf err (translate-condition err))
                 (set-on-error client err)
                 (when err
                   (let ((handler (if (typep err 'http-abort-error)
                                      onabort onerror)))
                     (emit handler err))))
               (emit-progress (progress n)
                 (when n
                   (incf (http-progress-current progress) n)
                   (emit onprogress (copy-http-progress progress)))))
        (setf request
              (make-http-request
               method
               (http-compose-query uri query encoding)
               :headers headers
               :default-headers `(,(when content-type `(:Content-Type ,content-type))
                                  ,(when content-length `(:Content-Length ,content-length))
                                  ,(when *http-user-agent* `(:User-Agent ,*http-user-agent*))
                                  ,(when *http-accept-type* `(:Accept ,*http-accept-type*))
                                  )
               :options `((:redirect-policy ,(if no-redirect :never *http-redirect-policy*))
                          ;; proxy ÇÕÇ†Ç∆Ç≈ê›íË
                          )
               :credentials `(,(when auth `(:server ,@auth))
                              ,(when proxy-auth `(:proxy ,@proxy-auth))
                              )
               :handlers
               `(:response ,#'(lambda ()
                                (set-on-response client)
                                (let ((len (winhttp:query-response-header request :content-length :type :number)))
                                  (setf (http-progress-total response-progress) len)
                                  (setf ondata
                                        (emit receiver
                                              (http-response-status client)
                                              (http-response-header-alist client)
                                              len
                                              ))))
                 :send ,#'(lambda (n)
                            (emit-progress request-progress n)
                            (if (listen body-stream)
                                (winhttp:write-data request (read-into buf body-stream nil nil))
                              (winhttp:receive-response request)))
                 :data ,#'(lambda (chunk n)
                            (emit-progress response-progress n)
                            (emit ondata chunk))
                 :end ,#'(lambda ()
                           (set-on-end client (emit ondata nil))
                           (apply #'emit oncomplete
                                  (multiple-value-list (http-response-values client))))
                 :close ,#'emit-error
                 :error ,#'emit-error
                 )
               ))
        (setf (http-client-request client) request)
        (set-proxy request uri proxy)
        (winhttp:send-request request :total-length content-length)
        (when wait
          (http-response-wait client))
        client
        ))))


;;; control functions

(defun http-request-abort (client)
  (unless (http-request-completed-p client)
    (let ((ctx (winhttp:request-context (http-client-request client))))
      (setf (context-aborted-p ctx) t))
    (with-translate-condition
     (winhttp:close-handle (http-client-request client)))
    ))

(defun http-response-wait (client &key nowait no-redraw sleep (timeout 30) (interval 0.1)
                                  (ready-state :complete)
                                  (abort-on-quit t)
                                  (signal-error t))
  (handler-case
      (progn
        (let ((start (get-internal-real-time)))
          (while (and (http-client-waiting-p client ready-state)
                      (not nowait)
                      (or (not timeout)
                          (< (/ (- (get-internal-real-time) start) 1000.0)
                             timeout)))
            (if sleep
                (sleep-for interval)
              (sit-for interval no-redraw))))
        (unless (http-client-waiting-p client ready-state)
          (when signal-error
            (when-let (err (http-client-error client))
              (error err))))
        (http-request-completed-p client))
    (quit (c)
      (when abort-on-quit
        (http-request-abort client))
      (error c))
    ))


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
  (http-response-wait client :ready-state :loaded)
  (http-client-uri client))

(defun http-request-header (client header)
  (check-type header string-designator)
  (cdr (assoc header (http-request-header-alist client) :test #'string-equal)))

(defun http-request-header-alist (client)
  (http-response-wait client :ready-state :loaded)
  (http-client-request-header-alist client))


;;; response accessors

(defun http-response-status (client)
  (http-response-wait client :ready-state :loaded)
  (http-client-status-code client))

(defun http-response-status-text (client)
  (http-response-wait client :ready-state :loaded)
  (http-client-status-text client))

(defun http-response-header (client header)
  (check-type header string-designator)
  (cdr (assoc header (http-response-header-alist client) :test #'string-equal)))

(defun http-response-header-alist (client)
  (http-response-wait client :ready-state :loaded)
  (http-client-response-header-alist client))

(defun http-response-result (client)
  (http-response-wait client)
  (when-let (r (http-client-result client))
    (if (functionp r)
        (funcall r)
      r)))

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

(defun http-date-from-universal-time (universal-time)
  (with-translate-condition
   (winhttp:time-from-universal-time universal-time)))

(defun http-date-to-universal-time (http-date)
  (with-translate-condition
   (winhttp:time-to-universal-time http-date)))


(provide "http-client/api/api")

;;; End
