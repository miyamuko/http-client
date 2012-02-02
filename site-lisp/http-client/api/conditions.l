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

(export '(http-error
          http-error-keyword
          http-error-cause
          http-timeout-error
          http-connect-error
          http-request-error
          http-abort-error
          ))

(progn
  (define-condition http-error (simple-error)
    (keyword cause)
    #1=(:report (lambda (c s)
                  (apply 'format s
                         (http-error-format-string c)
                         (http-error-format-arguments c))))
    )

  (define-condition http-timeout-error (http-error)
    ()
    #1#)

  (define-condition http-connect-error (http-error)
    ()
    #1#)

  (define-condition http-request-error (http-error)
    ()
    #1#)

  (define-condition http-abort-error (http-error)
    ()
    #1#)
  )


(defmacro with-translate-condition (&body body)
  `(handler-case
       (progn ,@body)
     (winhttp:winhttp-condition (c)
       (error (translate-condition c)))))

(defun translate-condition (c)
  (typecase c
    (winhttp:winhttp-condition
     (let ((keyword (winhttp:winhttp-condition-error-keyword c)))
       (multiple-value-bind (type msg)
           (translate-winhttp-condition c)
         (make-condition type
                         :keyword keyword
                         :cause c
                         :format-string "~A (~S)~%~A"
                         :format-arguments (list msg keyword c)))))
    (t
     c)))

(defun translate-winhttp-condition (c)
  (case (winhttp:winhttp-condition-error-keyword c)
    (:timeout
     #1=(values 'http-timeout-error "タイムアウトしました。"))
    (:name-not-resolved
     #2=(values 'http-connect-error "接続できません。"))
    (:cannot-connect #2#)
    (:connection-error #2#)
    (:operation-cancelled
     #3=(values 'http-abort-error "中断しました"))
    (:invalid-server-response
     #4=(values 'http-request-error "リクエストが失敗しました。"))
    (:redirect-failed #4#)
    (t
     (values 'http-error "HTTP エラー"))
    ))


(provide "http-client/api/conditions")

;;; End
