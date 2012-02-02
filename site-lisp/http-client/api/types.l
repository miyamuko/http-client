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

(export '(http-progress
          http-progress-p
          http-progress-direction
          http-progress-current
          http-progress-total
          http-progress-percent
          ))

(defconstant +http-client-ready-state+
  '(:uninitialized              ; 初期状態
    :loading                    ; ヘッダ受信完了 & ボディ受信中
    :complete                   ; ボディ受信完了
    ))

(deftype output-stream ()
  `(satisfies (lambda (v)
                (and v
                     (streamp v)
                     (output-stream-p v)))))


(defstruct (http-progress
            (:print-function
             (lambda (o s p)
               (if-let (percent (http-progress-percent o))
                   (format s "~:[Send~;Receive~] ~:D/~:D bytes (~D%)"
                           (http-progress-response-p o)
                           (http-progress-current o)
                           (http-progress-total o)
                           percent)
                 (format s "~:[Send~;Receive~] ~:D bytes"
                         (http-progress-response-p o)
                         (http-progress-current o))))))
  (response-p nil :type symbol :read-only t)
  (current 0 :type integer :read-only nil)
  (total 0 :type integer :read-only nil)
  )

(defun http-progress-percent (x)
  (check-type x http-progress)
  (when (and (http-progress-total x)
             (< 0 (http-progress-total x)))
    (floor (* 100 (/ (http-progress-current x)
                     (http-progress-total x))))))


(defstruct http-client
  (ready-state :uninitialized)
  uri
  status-code
  status-text
  response-header-alist
  request-header-alist
  result
  error
  request
  )

(defun ready-state-order (ready-state)
  (or (position ready-state +http-client-ready-state+)
      (error "Undefined ready state: ~A" ready-state)))

(defun http-client-waiting-p (client ready-state)
  (< (ready-state-order (http-client-ready-state client))
     (ready-state-order ready-state)
     ))

(defun set-on-response (client &optional (request (http-client-request client)))
  (setf (http-client-ready-state client)
        :loading

        (http-client-uri client)
        (winhttp:query-option request :url)

        (http-client-status-code client)
        (winhttp:query-response-header request :status-code :type :number)

        (http-client-status-text client)
        (winhttp:query-response-header request :status-text)

        (http-client-response-header-alist client)
        (raw-headers->alist
         (winhttp:query-response-header request :raw-headers))

        (http-client-request-header-alist client)
        (raw-headers->alist
         (winhttp:query-request-header request :raw-headers))
        ))

(defun set-on-end (client result &optional (request (http-client-request client)))
  (setf (http-client-ready-state client)
        :complete

        (http-client-result client)
        result
        ))

(defun set-on-error (client err)
  (setf (http-client-ready-state client)
        :complete

        (http-client-error client)
        err
        ))


(provide "http-client/api/types")

;;; End
