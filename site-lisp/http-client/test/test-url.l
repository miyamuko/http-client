; -*- mode: lisp; package: http-client.test.url; encoding: shift_jis -*-

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

(require "lisp-unit")

(require "http-client")
(require "http-client/test/helper")

(defpackage :http-client.test.url
  (:use :lisp :editor
   :lisp-unit
   :http-client
   )
  )

(in-package :http-client.test.url)

(remove-all-tests :http-client.test.url)

(define-test http-client/url/create-url
  (assert-equal "http://www.google.co.jp"
      (http-client::create-url "www.google.co.jp" nil))
  (assert-equal "http://www.google.co.jp:81"
      (http-client::create-url "www.google.co.jp:81" nil))
  (assert-equal "http://www.google.co.jp:81"
      (http-client::create-url "http://www.google.co.jp:81" nil))

  (assert-equal "https://www.google.co.jp"
      (http-client::create-url "www.google.co.jp" nil :secure t))
  (assert-equal "https://www.google.co.jp:81"
      (http-client::create-url "www.google.co.jp:81" nil :secure t))
  (assert-equal "https://www.google.co.jp:81"
      (http-client::create-url "https://www.google.co.jp:81" nil :secure t))

  (assert-equal "https://www.google.co.jp:81"
      (http-client::create-url "https://www.google.co.jp:81" nil :secure nil))
  (assert-equal "http://www.google.co.jp:81"
      (http-client::create-url "http://www.google.co.jp:81" nil :secure t))

  (assert-equal "http://www.google.co.jp/search"
      (http-client::create-url "www.google.co.jp" "search"))
  (assert-equal "http://www.google.co.jp/search"
      (http-client::create-url "www.google.co.jp" "/search"))
  (assert-equal "http://www.google.co.jp/search"
      (http-client::create-url "www.google.co.jp" '("/search")))
  (assert-equal "http://www.google.co.jp/search?q=xyzzy&num=30"
      (http-client::create-url "www.google.co.jp" '("/search" (q "xyzzy") (num 30))))
  (assert-equal "http://www.google.co.jp/search/fuga?q=xyzzy&num=30"
      (http-client::create-url "www.google.co.jp/search" '("/fuga" (q "xyzzy") (num 30))))
  (assert-equal "http://www.google.co.jp/search/fuga?q=xyzzy&num=30"
      (http-client::create-url "www.google.co.jp/search/" '("fuga" (q "xyzzy") (num 30))))
  (assert-equal "http://www.google.co.jp/search/?fuga=hoge&q=xyzzy&num=30"
      (http-client::create-url "www.google.co.jp/search/" '("?fuga=hoge" (q "xyzzy") (num 30))))

  (assert-equal "http://www.google.co.jp/search?q=%82%A0&num=30"
      (http-client::create-url "www.google.co.jp" '("/search" (q "‚ ") (num 30))))
  (assert-equal "http://www.google.co.jp/search?q=%E3%81%82&num=30"
      (http-client::create-url "www.google.co.jp" '("/search" (q "‚ ") (num 30))
                               :encoding *encoding-utf8n*))

  (assert-equal "http://www.google.co.jp/search?fuga=hoge&q=xyzzy&num=30"
      (http-client::create-url "http://www.google.co.jp/search" '("?fuga=hoge" (q "xyzzy") (num 30))))

  (assert-equal "http://www.google.co.jp/search?q=xyzzy&num=30"
      (http-client::create-url "http://www.google.co.jp/sea" '("rch" (q "xyzzy") (num 30))))
  )
