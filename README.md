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

http-client は [xl-winhttp] を利用した HTTP クライアント・ライブラリです。
Proxy、Basic/Digest 認証、SSL、Cookie、非同期通信などをサポートしています。

[xml-http-request] と比較したメリット・デメリットはそれぞれ以下のとおりです。

  * メリット
    * ストリーミングに対応している (Transfer-Encoding: chunked)
    * Content-Type の charset が不明な場合でも文字化けしない
    * バイナリ・データの送受信に対応している
    * ファイル・ダウンロードに対応している
    * ファイル・アップロードに対応している (multipart/form-data)
    * IE とセッションが分離されているので、IE のキャッシュやクッキーの影響を受けない

  * デメリット
    * IE とは別にプロキシを WinHTTP で設定しておく必要がある
    * IE とセッションが分離されているので、自前でログイン処理などを実装する必要がある
    * クッキーは xyzzy を再起動するとすべて消える
    * 認証情報は毎回指定する必要がある
      - 自動的に認証ダイアログがでない
      - 認証情報のキャッシュは行わない
    * 圧縮転送に対応していない (Content-Encoding: gzip)
    * 受信処理を xyzzy Lisp で行うため若干遅い

  [xl-winhttp]: http://miyamuko.s56.xrea.com/xyzzy/xl-winhttp/intro.htm
  [xml-http-request]: http://miyamuko.s56.xrea.com/xyzzy/xml-http-request/intro.htm


## INSTALL

1. [NetInstaller] で http-client, xl-winhttp, xl-alexandria, ansi-loop, ansify, setf-values
   をインストールします。

2. http-client はライブラリであるため自動的にロードはされません。
   必要な時点で require してください。

  [NetInstaller]: http://www7a.biglobe.ne.jp/~hat/xyzzy/ni.html


## REFERENCE

* references/ 配下を見てください。


## TODO

* content-transfer-encoding で binary 以外の対応
* メモリ使用量削減
  - 受信バッファ用 chunk の再利用
* chunked アップロードに対応


## KNOWN BUGS

  * http-general-receiver: 取得したレスポンスが UTF-8 で、
    :encoding 引数を指定しない場合にうまく行単位に分割できない場合がある

要望やバグは [GitHub Issues] か [@miyamuko] まで。

  [GitHub Issues]: http://github.com/miyamuko/http-client/issues
  [@miyamuko]: http://twitter.com/home?status=%40miyamuko%20%23xyzzy%20http-client%3a%20


## AUTHOR

みやむこ かつゆき (<mailto:miyamuko@gmail.com>)


## ACKNOWLEDGEMENT

本ライブラリの仕様を検討するにあたり、Gauche の rfc.http モジュール
および Clojure の http.async.client モジュールの仕様を参考にさせてもらいました。

  * [Gauche ユーザリファレンス: 11.23 rfc.http - HTTP](http://practical-scheme.net/gauche/man/gauche-refj_146.html)
  * [rfc.http に関する議論](https://www.google.com/search?q=rfc.http+inurl:http://chaton.practical-scheme.net/gauche/&filter=0&qscrl=1)
  * [Asynchronous HTTP Client - Clojure - Documentation](http://neotyk.github.com/http.async.client/docs.html#sec-2)


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
