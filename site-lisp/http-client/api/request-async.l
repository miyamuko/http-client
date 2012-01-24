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

(defun http-request-async (method server request-uri body
                                  &rest headers
                                  &key request-encoding proxy
                                  (sink (make-default-sink))
                                  (flusher (make-default-flusher))
                                  no-redirect secure async
                                  (oncomplete #'(lambda (status-code response-header response-text)
                                                  (message "~A" status-code)))
                                  (onerror #'(lambda (err)
                                               (errbox "Error: ~A" err)))
                                  &allow-other-keys)
  (check-type server string)
  (check-type request-uri (or string list))
  (check-type method (or string symbol))
  (check-type sink (or output-stream null))
  (remove-from-plistf headers :request-encoding :proxy :sink :flusher :no-redirect :secure :async :oncomplete :onerror)
  (let ((opts (http-client.async:parse-url (create-url server request-uri
                                                       :encoding request-encoding
                                                       :secure secure))))
    (multiple-value-bind (content-type form-data multipart-p)
        (build-form-data body request-encoding)
      (when content-type
        (setf headers (append headers `(:content-type ,content-type
                                        :content-length ,(calc-multipart/form-data-size form-data)
                                        ))))
      (setf opts (append opts `(:method ,method
                                :headers ,headers)))
      (let ((req (http-client.async:http-request opts)))
        (on :response req
            #'(lambda (res)
                (on :data res
                    #'(lambda (chunk)
                        (princ chunk sink)))
                (on :end res
                    #'(lambda ()
                        (multiple-value-call oncomplete
                          (finish-response req sink flusher))))
                (on :close res
                    #'(lambda (err)
                        (when err
                          (funcall onerror err))))
                ))
        (on :send req
            #'(lambda (n)
                (cond ((and multipart-p form-data)
                       (setf form-data
                             (write-multipart/form-data-async
                              #'(lambda (chunk)
                                  (http-client.async:http-request-write req chunk nil))
                              form-data)))
                      (t
                       (http-client.async:http-request-end req form-data nil)))))
        (on :error req
            #'(lambda (err)
                (funcall onerror err)))
        req))))

(defun build-form-data (body request-encoding)
  (check-type body (or string list))
  (cond ((null body)
         (values nil nil nil))
        ((multipart-form-data-p body)
         (let* ((boundary (make-boundary))
                (form-data (build-multipart/form-data body request-encoding boundary))
                (total-length (calc-multipart/form-data-size form-data)))
           (values (content-type-header-value :form-data t :boundary boundary)
                   form-data
                   t)
           ))
        (t
         (values (content-type-header-value :urlencoded t)
                 (url-encode-params body request-encoding)
                 nil)
         )))


(provide "http-client/api/request-async")

;;; End