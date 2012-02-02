# http-client - HTTP クライアント

* Home URL: http://miyamuko.s56.xrea.com/xyzzy/http-client/intro.htm
* Version: 0.0.1


## SYNOPSIS

```lisp
;;; xyzzy lisp REPL
user> (require "http-client")
t

user> (in-package :http-client)
#<package: http-client.api>

;; 基本的に非同期で future オブジェクトを返します
http-client.api> (setf r (http-get "http://www.google.com/search"
                                   :query '(:hl "ja" :lr "lang_ja" :ie "UTF-8" :oe "Shift_JIS" :num 50
                                            :q "xyzzy 読み方")
                                   :encoding *encoding-utf8n*))
#S(http-client ready-state :uninitialized uri nil status-code nil status-text nil ...)

;; 結果を取得しようとした時点でレスポンスを待ちます
http-client.api> (http-request-uri r)
"http://www.google.com/search?hl=ja&lr=lang_ja&ie=UTF-8&oe=Shift_JIS&num=50&q=xyzzy%20%E8%AA%AD%E3%81%BF%E6%96%B9"

http-client.api> (http-request-header-alist r)
(("User-Agent" . "xyzzy/0.2.2.235") ("Host" . "www.google.com") ...)

http-client.api> (http-response-status r)
200

http-client.api> (http-response-status-text r)
"OK"

http-client.api> (http-response-header-alist r)
(("Content-Type" . "text/html; charset=Shift_JIS") ("Date" . "Wed, 01 Feb 2012 07:09:23 GMT") ...)

http-client.api> (http-response-result r)
"<!doctype html> ..."

;; receiver に http-file-receiver を指定することでレスポンス・ボディをファイルに
;; 出力することができます。
http-client.api> (defun http-download (uri localfile)
                   (http-get uri
                             :receiver (http-file-receiver localfile)
                             :oncomplete #'(lambda (fullpath status headers uri)
                                             (msgbox "Download OK~%URL: ~A~%File: ~A"
                                                     uri fullpath))
                             :onerror #'(lambda (err) (msgbox "Error: ~A" err))
                             :onabort #'(lambda (err) (msgbox "Abort: ~A" err))
                             ))
http-download

http-client.api> (http-download "http://www.jsdlab.co.jp/~kamei/cgi-bin/download.cgi"
                                "xyzzy-0.2.2.235.lzh")
#S(http-client ready-state :uninitialized uri nil status-code nil status-text nil ...)

;; receiver に http-buffer-receiver を指定することでレスポンス・ボディをバッファに
;; 出力することができます。
http-client.api> (defun find-uri (uri)
                   (interactive "sURL: ")
                   (http-get uri
                             :receiver (http-buffer-receiver uri)
                             :oncomplete #'(lambda (buffer status headers uri)
                                             (pop-to-buffer buffer)
                                             (refresh-screen))
                             :onerror #'(lambda (err) (msgbox "Error: ~A" err))
                             :onabort #'(lambda (err) (msgbox "Abort: ~A" err))
                             ))
find-uri

http-client.api> (find-uri "http://goo.gl/bgggL")
```


## DESCRIPTION

http-client は [xl-winhttp](http://miyamuko.s56.xrea.com/xyzzy/xl-winhttp/intro.htm)
を利用した HTTP クライアントライブラリです。

xl-winhttp は WinHTTP の API をそのまま提供するという方針であるため、
利用するにはある程度の WinHTTP の知識を必要とし利用が難しいライブラリです。

http-client は xl-winhttp をラップし利用しやすい API を提供します。


## INSTALL

1. [NetInstaller] で http-client, xl-winhttp, xl-alexandria, ansi-loop, ansify, setf-values
   をインストールします。

2. http-client はライブラリであるため自動的にロードはされません。
   必要な時点で require してください。

  [NetInstaller]: http://www7a.biglobe.ne.jp/~hat/xyzzy/ni.html


## REFERENCE

* references/ 配下を見てください。


## TODO

* :accept */* は Accept ヘッダが指定されていない場合のみ設定する
  - `*default-accept-header*`
* http-error
  - winhttp-condition をラップする
* content-transfer-encoding で binary 以外の対応
* メモリ使用量削減
  - 受信バッファ用 chunk の再利用
* chunked アップロードに対応


## KNOWN BUGS

なし。

要望やバグは [GitHub Issues] か [@miyamuko] まで。

  [GitHub Issues]: http://github.com/miyamuko/http-client/issues
  [@miyamuko]: http://twitter.com/home?status=%40miyamuko%20%23xyzzy%20http-client%3a%20


## AUTHOR

みやむこ かつゆき (<mailto:miyamuko@gmail.com>)


## COPYRIGHT

http-client は MIT/X ライセンスに従って本ソフトウェアを使用、再頒布することができます。

    Copyright (c) 2012 MIYAMUKO Katsuyuki.

    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
