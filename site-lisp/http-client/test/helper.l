; -*- mode: lisp; package: http-client.test.helper; encoding: shift_jis -*-

(require "lisp-unit")

(defpackage :http-client.test.helper
  (:use :lisp :editor :lisp-unit
   )
  )

(in-package :http-client.test.helper)

(export '($
          utf8
          euc
          sjis
          bin
          ))

(defun $ (obj &rest keys)
  (reduce #'(lambda (o k)
              (cdr (assoc k o :test #'string-equal)))
          keys
          :initial-value obj))

(defun utf8 (str)
  (when str
    (convert-encoding-from-internal *encoding-utf8n* str)))

(defun euc (str)
  (when str
    (convert-encoding-from-internal *encoding-euc-jp* str)))

(defun sjis (str)
  (when str
    (convert-encoding-from-internal *encoding-sjis* str)))

(defun bin (str)
  (when str
    (map 'string 'code-char
         (mapcan #'(lambda (code)
                     (let (r)
                       (loop
                         (push (logand #xff code) r)
                         (setf code (ash code -8))
                         (when (zerop code)
                           (return r)))))
                 (map 'list 'char-code str)))))


(provide "http-client/test/helper")

;;; End
