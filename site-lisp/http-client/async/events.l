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
  )

(in-package :http-client.async)

(export '(on
          ))

(defun job-queue (request proc)
  (let ((ctx (winhttp:request-context request)))
    (if (context-waiting-p ctx)
        (put-waiting-jobs ctx proc)
      (progn
        (funcall proc)
        (setf (context-waiting-p ctx) t))
      )))

(defmacro with-job-queue (request &rest body)
  `(job-queue request #'(lambda () ,@body)))
(setf (lisp:get 'with-job-queue 'ed:lisp-indent-hook) 1)

(defun emit (ctx event &rest args)
  (when-let (cb (getf (context-callbacks ctx) event))
    (apply cb args)))

(defun on (event request callback)
  (check-type event symbol)
  (check-type request winhttp:request)
  (check-type callback winhttp::callable)
  (let* ((ctx (winhttp:request-context request))
         (callbacks (context-callbacks ctx)))
    (if (getf callbacks event)
        (setf (getf callbacks event) callback)
      (setf (context-callbacks ctx)
            (append callbacks (list event callback))))
    request))

(defmacro define-event (event)
  (let ((func (symbolicate "on-" event)))
    `(progn
       (defun ,func (request callback)
         (on request ,event callback))
       (export ',func)
       )))

(define-event :send)
(define-event :response)
(define-event :upgrade)
(define-event :continue)
(define-event :error)

(define-event :data)
(define-event :end)
(define-event :close)


(provide "http-client/async/events")

;;; End