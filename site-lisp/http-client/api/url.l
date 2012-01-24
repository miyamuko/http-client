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

(defparameter *default-literal-chars* "0-9A-Za-z._~-")


(defun create-url (server request-uri &key encoding secure)
  (let (scheme path params)
    (etypecase request-uri
      (null nil)
      (string
       (setf path request-uri))
      (list
       (setf path (car request-uri)
             params (cdr request-uri))))
    ;; server Ç… http://localhost Ç»Ç«ÇéwíËÇµÇΩèÍçá
    (when (string-match "\\`\\(?:\\([a-z]+\\)://\\)?\\([^/]+\\)\\(.*\\)\\'" server)
      (setf scheme (match-string 1)
            server (match-string 2)
            path (concat (match-string 3) path)
            ))
    (winhttp:create-url scheme nil nil server nil
                        (add-query-params path params :encoding encoding)
                        nil secure)))

(defun compose-query (path-and-params encoding)
  (etypecase path-and-params
    (null nil)
    (string path-and-params)
    (list
     (add-query-params (car path-and-params) (cdr path-and-params)
                       :encoding encoding))))

;; from xml-http-request/util.l

(defun add-query-params (uri query &key encoding literal-chars)
  (add-query-string uri (url-encode-params query encoding literal-chars)))

(defun add-query-string (uri query-string)
  (if (emptyp query-string)
      uri
    (format nil "~A~A~A"
            (or uri "")
            (cond ((emptyp uri) "?")
                  ((member (char uri (1- (length uri))) '(#\? #\&)) "")
                  ((find #\? uri) "&")
                  (t "?"))
            query-string)))

(defun url-encode-params (data &optional encoding literal-chars)
  (etypecase data
    (null nil)
    (string data)
    (list
     (format nil "~{~A~^&~}"
             (mapcar #'(lambda (e)
                         (url-encode-key-value e encoding literal-chars))
                     (remove-if #'null data))))
    ))

(defun url-encode-key-value (data &optional encoding literal-chars)
  (etypecase data
    (null nil)
    (string data)
    (list
     (format nil "~A~^=~A"
             (url-encode-value (car data) encoding literal-chars)
             (url-encode-value (cadr data) encoding literal-chars)
             ))))

(defun url-encode-value (v &optional encoding literal-chars)
  (let ((v (typecase v
             (string v)
             (null "")
             (t (format nil "~A" v))))
        (literal-chars (or literal-chars *default-literal-chars*)))
    (si:www-url-encode (encode v encoding)
                       nil literal-chars)))


(provide "http-client/api/url")

;;; End
