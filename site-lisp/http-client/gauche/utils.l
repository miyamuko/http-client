; -*- mode: lisp; package: http-client.gauche; encoding: shift_jis -*-

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
  (require "http-client/gauche/package")
  )

(in-package :http-client.gauche)

(defun parse-server (server &key secure)
  (multiple-value-bind (scheme user pass host port path extra)
      (winhttp:crack-url (format nil "~:[http~;https~]://~A" secure server))
    (values host port)))

(defun merge-header-alist (alist)
  (loop with r
    for (k . v) in alist
    for k = (string-downcase k)
    do (if (assoc k r :test #'equal)
           (appendf (cdr (assoc k r :test #'equal)) (list v))
         (push (list k v) r))
    finally (return (nreverse r))))

(defun make-default-sink ()
  (make-string-output-stream))

(defun make-default-flusher ()
  #'(lambda (sink headers)
      (unwind-protect
          (get-output-stream-string sink)
        (close sink))))


(provide "http-client/gauche/utils")

;;; End
