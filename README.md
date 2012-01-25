# http-client - HTTP クライアント

* Home URL: http://miyamuko.s56.xrea.com/xyzzy/http-client/intro.htm
* Version: 0.0.1


## SYNOPSIS

```lisp
(require "http-client")

(defpackage :your-app
  (:use
   :lisp :editor
   :http-client
   ))

(in-package :your-app)

(defun http-download-async (url localfile callback)
  (let ((out (open localfile :direction :output :encoding :binary))
        (total 0))
    (http-get url nil
              :sink (make-general-output-stream
                     #'(lambda (chunk)
                         (incf total (length chunk))
                         (message "Download ~:D bytes" total)
                         (princ chunk out)))
              :flusher #'(lambda (sink headers)
                           (close out))
              :async t
              :oncomplete #'(lambda (status headers _)
                              (funcall callback url localfile nil))
              :onerror #'(lambda (err)
                           (funcall callback url localfile err))
              )))

(http-download-async "http://www.jsdlab.co.jp/~kamei/cgi-bin/download.cgi"
                     "xyzzy-0.2.2.235.lzh"
                     #'(lambda (url localfile err)
                         (if err
                             (msgbox "ダウンロードが失敗しました。~%URL: ~A~%File: ~A~%Error: ~A"
                                     url localfile err)
                           (msgbox "ダウンロードが完了しました。~%URL: ~A~%File: ~A~%MD5: ~A"
                                   url localfile
                                   (with-open-file (s localfile :encoding :binary)
                                     (si:md5 s))))))
```


## DESCRIPTION

http-client は [xl-winhttp](http://miyamuko.s56.xrea.com/xyzzy/xl-winhttp/intro.htm)
を利用した HTTP クライアントライブラリです。

xl-winhttp は WinHTTP の API をそのまま提供するという方針であるため、
利用するにはある程度の WinHTTP の知識を必要とし利用が難しいライブラリです。

http-client は xl-winhttp をラップし利用しやすい API を提供します。

また、http-client では API の学習コストが最小になるように、
[Gauche の rfc.http モジュール](http://practical-scheme.net/gauche/man/gauche-refj_146.html)
の仕様を参考にして実装しています。


## INSTALL

1. [NetInstaller](http://www7a.biglobe.ne.jp/~hat/xyzzy/ni.html)
   で http-client, xl-winhttp, xl-alexandria, ansi-loop, ansify, setf-values をインストールします。

2. http-client はライブラリであるため自動的にロードはされません。
   必要な時点で require してください。


## REFERENCE

* references/ 配下を見てください。


## TODO

* :accept */* は Accept ヘッダが指定されていない場合のみ設定する
  - `*default-accept-header*`
* 非同期処理完了時に request と connection の close
* http-error
  - winhttp-condition をラップする
* API 追加
  - http-abort
    - 非同期処理を中断
  - http-request-aborted-p
    - 非同期処理が中断したかどうか
  - http-request-completed-p
    - 非同期処理が完了したかどうか
* イベント追加?
  - onabort
  - onresponse
    - レスポンスヘッダを受信した時点で呼び出す
  - onprogress (type send-or-write-length content-length)
    - 進捗状況のコールバック
* content-transfer-encoding で binary 以外の対応
* メモリ使用量削減
  - 受信バッファ用 chunk の再利用
* chunked アップロードに対応


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
