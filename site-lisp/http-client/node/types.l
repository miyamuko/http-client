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
  )

(in-package :http-client.node)

(defstruct context
  (aborted-p nil :type bool)
  (waiting-p nil :type bool)
  (paused-p nil :type bool)
  (waiting-jobs nil)
  (paused-events nil)
  (last-event nil)
  (max-read-length nil)
  (total-read-length 0)
  (callbacks nil)
  )

(defun take-waiting-jobs (context)
  (pop (context-waiting-jobs context)))

(defun put-waiting-jobs (context job)
  (appendf (context-waiting-jobs context) (list job)))

(defun put-paused-events (context job)
  (appendf (context-paused-events context) (list job)))


(provide "http-client/node/types")

;;; End
