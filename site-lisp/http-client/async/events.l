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

(export '(http-set-callback
          http-request-abort
          on
          ))

(defun http-set-callback (request)
  (setf (winhttp:request-context request) (make-context))
  (winhttp:set-status-callback request 'request-callback))

(defun request-callback (event req &rest args)
  (let ((ctx (winhttp:request-context req)))
    (setf (context-last-event ctx) event)
    ;(msgbox "~S ~S~%~%~S" event args req)
    (request-callback1 event req args)))

(defun request-callback1 (event req args)
  (destructuring-case `(,event ,req ,@args)
    ;; ���N�G�X�g�w�b�_���M����
    ((:send-request-complete req)
     (emit-send req nil))
    ;; ���N�G�X�g�{�f�B���M
    ((:write-complete req n)
     (emit-send req n))
    ;; ���X�|���X�w�b�_��M����
    ((:headers-available req)
     (emit-response req)
     (winhttp:query-data-available req))
    ;; ���X�|���X�{�f�B��M�`�F�b�N
    ((:data-available req n)
     (cond ((<= n 0)
            ;; EOF reached
            (emit-end req)
            (winhttp:close-handle req))
           (t
            ;; Data available
            (winhttp:read-data req n))))
    ;; ���X�|���X�{�f�B��M
    ((:read-complete req data n)
     (emit-data req data)
     (winhttp:query-data-available req))
    ;; ���M�G���[
    ((:request-error req type error)
     (winhttp:close-handle req)
     (emit-error req error))
    ;; SSL �G���[
    ((:secure-failure failure)
     ;; TODO
     )
    ;; �R�[���o�b�N���ŃG���[
    ((:callback-error req error)
     (winhttp:close-handle req)
     (emit-error req error))
    ;; Ctrl-g �������ꂽ
    ((:callback-quit req error)
     (winhttp:close-handle req)
     (emit-error req error))
    ;((:resolving-name server)
    ; )
    ;((:name-resolved addr)
    ; )
    ;((:connecting-to-server addr)
    ; )
    ;((:connected-to-server addr)
    ; )
    ;((:headers-available)
    ; )
    ;((:intermediate-response status-code)
    ; )
    ;((:redirect url)
    ; )
    ;((:sending-request)
    ; )
    ;((:request-sent n)
    ; )
    ;((:response-received n)
    ; )
    ;((:closing-connection)
    ; )
    ;((:connection-closed)
    ; )
    ;((:handle-created hinternet)
    ; )
    ;((:handle-closing hinternet)
    ; )
    ))

(defun emit (event request &rest args)
  (check-type event symbol)
  (check-type request winhttp:request)
  (let* ((ctx (winhttp:request-context request))
         (callbacks (context-callbacks ctx)))
    (when-let (cb (getf callbacks event))
      (apply cb args))))

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
  (let ((on-func (symbolicate "on-" event))
        (emit-func (symbolicate "emit-" event)))
    `(progn
       (defun ,on-func (request callback)
         (on ,event request callback))
       (defun ,emit-func (request &rest args)
         (apply 'emit ,event request args))
       (export ',on-func)
       (export ',emit-func)
       (setf (get ',on-func 'ed:lisp-indent-hook) 1)
       )))

(define-event :send)
(define-event :response)
(define-event :error)

(define-event :data)
(define-event :end)
(define-event :close)


(provide "http-client/async/events")

;;; End