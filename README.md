# http-client - HTTP クライアント

* Home URL: http://miyamuko.s56.xrea.com/xyzzy/http-client/intro.htm
* Version: 0.0.1


## SYNOPSIS

### Node.js ほぼ互換 API

```lisp
(require "http-client")

(defpackage :your-app
  (:use
   :lisp :editor
   :http-client.node))

(in-package :your-app)

(defun http-download-async (url localfile)
  (let* ((opts (parse-url url))
         (req (http-get opts))
         (out (open localfile :direction :output :encoding :binary)))
    (http-request-on-response
     req #'(lambda (res)
             (http-response-on-data
              res #'(lambda (chunk)
                      (princ chunk out)))
             (http-response-on-end
              res #'(lambda ()
                      (close out)
                      (setf out nil)
                      (msgbox "ダウンロードが完了しました。~%~A~%~A" url localfile)))
             (http-response-on-close
              res #'(lambda ()
                      (when out
                        (close out)
                        (msgbox "ダウンロードが失敗しました。~%~A~%~A" url localfile))))
             ))
    (http-request-on-error
     req #'(lambda (err)
             (when out
               (close out))
             (msgbox "エラーが発生しました。~%~A" err)))
    (http-request-end req)))

(http-download-async "http://www.google.co.jp/"
                     "index.html")
;=> t
```

### Gauche (rfc.http) ほぼ互換 API

TODO


## DESCRIPTION

http-client は [xl-winhttp](http://miyamuko.s56.xrea.com/xyzzy/xl-winhttp/intro.htm)
を利用した HTTP クライアントライブラリです。

xl-winhttp は WinHTTP の API をそのまま提供するという方針であるため、
利用するにはある程度の WinHTTP の知識を必要とし利用が難しいライブラリです。

http-client は xl-winhttp をラップし利用しやすい API を提供します。

また、http-client では API の学習コストが最小になるように、以下の HTTP ライブラリの
仕様を参考にして実装しています。

  * [Node.js ほぼ互換 API](http://nodejs.org/docs/latest/api/http.html)
    - 非同期 API のみ提供
  * [Gauche (rfc.http) ほぼ互換 API](http://practical-scheme.net/gauche/man/gauche-refj_146.html)
    - 同期・非同期 API の両方を提供


## INSTALL

1. [NetInstaller](http://www7a.biglobe.ne.jp/~hat/xyzzy/ni.html)
   で http-client, xl-winhttp, xl-alexandria, ansi-loop, ansify, setf-values をインストールします。

2. http-client はライブラリであるため自動的にロードはされません。
   必要な時点で require してください。


## TODO

* リファレンス
* Gauche (rfc.http) ほぼ互換 API の実装


## KNOWN BUGS

なし。

要望やバグは
[GitHub Issues](http://github.com/miyamuko/http-client/issues) か
[@miyamuko](http://twitter.com/home?status=%40miyamuko%20%23xyzzy%20http-client%3a%20)
まで。


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
