# http-client.gauche API Reference

  * [PACKAGES](#packages)
    * [http-client.gauche](#http-client.gauche)
  * [VARIABLES](#variables)
    * [\*http-user-agent\*](#*http-user-agent*)
  * [FUNCTIONS](#functions)
    * [http-get](#http-get)
    * [http-head](#http-head)
    * [http-post](#http-post)
    * [http-put](#http-put)
    * [http-delete](#http-delete)
    * [http-request](#http-request)
    * [http-compose-query](#http-compose-query)
    * [http-compose-form-data](#http-compose-form-data)
    * [http-secure-connection-available?](#http-secure-connection-available?)

----

## <a name="packages">PACKAGES</a>

### Package: <a name="http-client.gauche"><em>http-client.gauche</em></a>

このパッケージは、[RFC2616 "Hypertext Transfer Protocol - HTTP/1.1"](http://tools.ietf.org/html/rfc2616)
で定義されている HTTP/1.1 に対する簡単なクライアント API を提供します。

このパッケージは [Gauche の rfc.http モジュール](http://practical-scheme.net/gauche/man/gauche-refj_146.html)
とほぼ互換性があります。

Gauche の rfc.http モジュールとの非互換は以下のとおりです。

  * 任意の HTTP メソッドを送信できるように [http-request](#http-request) が利用可能です
  * `SERVER` に完全な URL を指定可能です
  * `:proxy` を指定しない場合は、WinHTTP のデフォルト設定が利用されます
  * `:content-type` でパラメータごとに文字エンコーディングを指定できません
  * `:content-type` を指定しない場合は、Content-Type ヘッダは送信されません
  * `:content-transfer-encoding` は binary のみしか指定できません
  * `:content-transfer-encoding` を指定しない場合は、Content-Transfer-Encoding ヘッダは送信されません
  * 非同期モードをサポートします (`:async`, `:oncomplete`, `:onerror`)

パッケージのニックネームは以下のとおりです。

  * `rfc.http`


----

## <a name="variables">VARIABLES</a>

### Variable: <a name="*http-user-agent*"><em>\*http-user-agent\*</em></a>

user-agent ヘッダに渡される値のデフォルト値を指定するスペシャル変数です。
デフォルトの値は xyzzy.http/* (*部分は xyzzy のバージョン) になっています。

各アプリケーションは適切な値を設定するようにしてください。

----

## <a name="functions">FUNCTIONS</a>

### Function: <a name="http-get"><em>http-get</em></a> <i>`SERVER` `REQUEST-URI` &rest `HEADERS` &key `:request-encoding` `:proxy` `:sink` `:flusher` `:no-redirect` `:secure` `:async` `:oncomplete` `:onerror` &allow-other-keys</i>

HTTP GET リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。

### Function: <a name="http-head"><em>http-head</em></a> <i>`SERVER` `REQUEST-URI` &rest `HEADERS` &key `:request-encoding` `:proxy` `:no-redirect` `:secure` `:async` `:oncomplete` `:onerror` &allow-other-keys</i>

HTTP HEAD リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。

### Function: <a name="http-post"><em>http-post</em></a> <i>`SERVER` `REQUEST-URI` `BODY` &rest `HEADERS` &key `:request-encoding` `:proxy` `:sink` `:flusher` `:no-redirect` `:secure` `:async` `:oncomplete` `:onerror` &allow-other-keys</i>

HTTP POST リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。

### Function: <a name="http-put"><em>http-put</em></a> <i>`SERVER` `REQUEST-URI` `BODY` &rest `HEADERS` &key `:request-encoding` `:proxy` `:sink` `:flusher` `:secure` `:async` `:oncomplete` `:onerror` &allow-other-keys</i>

HTTP PUT リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。

### Function: <a name="http-delete"><em>http-delete</em></a> <i>`SERVER` `REQUEST-URI` &rest `HEADERS` &key `:request-encoding` `:proxy` `:sink` `:flusher` `:secure` `:async` `:oncomplete` `:onerror` &allow-other-keys</i>

HTTP DELETE リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。

### Function: <a name="http-request"><em>http-request</em></a> <i>`METHOD` `SERVER` `REQUEST-URI` `BODY` &rest `HEADERS` &key `:request-encoding` `:proxy` `:sink` `:flusher` `:no-redirect` `:secure` `:async` `:oncomplete` `:onerror` &allow-other-keys</i>

`SERVER` に `METHOD` と `REQUEST-URI` で指定されたリクエストを送りサーバの応答を返します。

#### 引数

  * `SERVER` 引数は文字列で HTTP サーバ名を指定します。

    サーバ名にはスキーム、ポート番号、およびパスを付加できます。

    ```
    "w3c.org"
    "mycompany.com:8080"
    "http://www.google.co.jp/search"
    ```

  * `REQUEST-URI` 引数は文字列かリストです。

    文字列の場合、RFC2616 で規定されているリクエスト URI と解釈されます。
    通常これは HTTP URL のパス部分です。文字列はそのままサーバに渡されるので、
    呼び出し側で必要な文字コード変換や url エンコーディングを行う必要があります。

    `REQUEST-URI` がリストの場合は、次の形式でなければなりません。

    ```lisp
    (path (name value) ...)
    ```

    ここで `path` はリクエスト URI のパスコンポーネントまでを指定する文字列です。
    与えられた `name` と `value` の alist から、http リクエスト手続きは HTML4 で定められた
    application/x-www-form-urlencoded 形式のクエリ文字列を構成し、`path` にアペンドします。

    例えば次のリクエストは同じ効果を持ちます。
    二番目以降の呼び出しでは url エスケープが自動的に行われることに注目してください。

    ```lisp
    (http-get "https://www.google.co.jp/search?q=xyzzy%20%93%C7%82%DD%95%FB" nil)
    (http-get "https://www.google.co.jp/search" '(nil (q "xyzzy 読み方")))
    (http-get "https://www.google.co.jp" '("/search" (q "xyzzy 読み方")))
    (http-get "www.google.co.jp" '("/search" (q "xyzzy 読み方")) :secure t)
    ```

    `:request-encoding` キーワード引数が与えられた場合、
    `name` と `value` はまずその文字エンコーディングに変換されたのちに url エスケープされます。

  * `BODY` は文字列かリストです。

    文字列の場合はそのまま送られます。
    リストの場合は multipart/form-data 形式にエンコードされて送られます。

    `BODY` がリストの場合、それはパラメータ指定のリストです。
    各パラメータ指定は、`("submit" "OK")` のような名前と値のリスト、
    もしくは `("upload" :file "logo.png" :content-type "image/png")` のように
    名前の後にキーワード-値リストを付加したものです。 

    最初の形式は使うのが簡単で、また request-uri のクエリパラメータリストと同じ形式なので
    GET と POST でルーチンを共有したい場合にも便利でしょう。

    `:request-encoding` キーワード引数が与えられた場合、
    `name` と `value` はまずその文字エンコーディングに変換されたのちに url エスケープされます。

    二番目の形式では、MIME パートの属性についてより細かな指定を行うことができます。
    以下のキーワードが特別に扱われます。

    * `:value`:
      パラメータの値を指定します。簡潔な `(name val)` 形式は `(name :value val)` の省略形です。

    * `:file`:
      指定された名前のファイルの中身をパラメータの値として挿入します。
      ファイルのアップロードに便利です。
      このオプションは `:value` より優先されます。

    * `:content-type`:
      MIME タイプを指定します。
      指定が無ければ Content-Type ヘッダは送信されません。

    * `:content-transfer-encoding`:
      送信時のエンコーディングを指定します (binary のみをサポート)。
      指定が無ければ Content-Transfer-Encoding ヘッダは送信されません。

  * `:request-encoding`

    `REQUEST-URI` や `BODY` がリストで与えられた場合、パラメータの名前や値は
    まずこの引数で指定される文字エンコーディングへと変換され、
    その後、application/x-www-form-urlencoded や multipart/form-data MIME 形式に
    したがったエンコーディングが行われます。

    `REQUEST-URI` や `BODY` に文字列を与えた場合は、文字エンコーディング変換は行われません。
    呼び出し側で望みの文字コードにあらかじめ変換しておいてください。

  * `:proxy`

    http プロキシサーバを、hostname または hostname:port 形式の文字列で指定します。
    指定しない場合は、WinHTTP のデフォルト設定のプロキシが利用されます。

  * `:no-redirect`

    真の値が与えられた場合、リダイレクションには従わなくなります。
    すなわち、手続きは"3xx"のメッセージをそのまま返します。

  * `:secure`

    真の値が与えられた場合、セキュアな接続 (https) を使います。
    `SERVER` にスキーマが指定された場合はスキーマの指定が優先されます。

    ```lisp
    http-client.gauche> (http-get "https://www.google.co.jp"
                                  '("/search" (q "xyzzy"))
                                  :secure nil :async t)
    #S(xl-winhttp.api:request description "GET /search?q=xyzzy with secure" ...)
    ```

  * `:sink`, `:flusher`

    これらのキーワード引数によりリプライメッセージ・ボディがどのように扱われるかをカスタマイズできます。
    `sink` には出力ストリームを、`flusher` には 2 引数を取る手続きを渡さなければなりません。

    `sink` に nil を指定した場合は受信したメッセージ・ボディは捨てられます。
    `flusher` に nil を指定した場合は何も呼び出されません。

    手続きがメッセージ・ボディを受信し始めると、`sink` へ受け取ったデータ片を書き込みます。
    手続きがメッセージ・ボディを受信し終わると、 `flusher` に与えられた手続きが、
    `sink` と(手続きからの 2 つ目の戻り値と同じフォーマットの)メッセージ・ヘッダ・フィールドの
    リストとともに呼び出されます。
    `flusher` の戻り値が、手続きからの 3 つ目の戻り値となります。

    したがって、`sink` のデフォルト値は、新しく開かれた string-output ストリームで、
    flusher のデフォルト値は `#'(lambda (sink headers) (get-output-stream-string sink))` とも言えます。

    以下のサンプルは、(とても大きい可能性のある) 文字列バッファを作らずに、
    メッセージ・ボディを直接ファイルに保存します。

    ```lisp
    http-client.gauche> (with-open-file (out "page.html" :direction :output :encoding :binary)
                          (http-get "www.schemers.org" "/"
                                    :sink out
                                    :flusher #'(lambda (sink headers)
                                                 (file-length sink))))
    "200" ;
    (("date" "Mon, 23 Jan 2012 07:10:21 GMT")
     ("server" "Apache/2.2.9 (Debian) mod_ssl/2.2.9 OpenSSL/0.9.8g")
     ...)
    5331
    ```

    メッセージ・ボディを受信するたびに何か処理を行いたい場合は、
    `sink` に general-output-stream を指定します。

    ```lisp
    http-client.gauche> (http-get "www.schemers.org"
                                  "/"
                                  :sink (make-general-output-stream
                                         #'(lambda (chunk)
                                             (msgbox "~A" chunk)))
                                  :flusher nil)
    "200" ;
    (("date" "Mon, 23 Jan 2012 07:10:21 GMT")
     ("server" "Apache/2.2.9 (Debian) mod_ssl/2.2.9 OpenSSL/0.9.8g")
     ...)
    nil
    ```

  * `:async`, `:oncomplete`, `:onerror`

    `async` に真の値が与えられた場合、非同期モードで動作します。

    `oncomplete` には 3 引数を取る手続きを、`onerror` には 1 引数を取る手続きを渡さなければなりません。
    nil を指定した場合は何も呼び出されません。

    * `oncomplete`: リクエストが完了した場合に呼び出されます。
      引数は同期呼び出し時の戻り値が指定されます。
      `flusher` より後に呼び出されます。

    * `onerror`: エラー発生時に呼び出されます。
      引数はコンディションです。

    ```lisp
    http-client.gauche> (http-get "www.schemers.org" "/"
                                  :sink (make-buffer-stream
                                         (get-buffer-create "*http-get*"))
                                  :flusher nil
                                  :async t
                                  :oncomplete #'(lambda (status-code headers result)
                                                  (pop-to-buffer "*http-get*")
                                                  (html+-mode)
                                                  (refresh-screen))
                                  :onerror #'(lambda (err)
                                               (msgbox "Error: ~A" err))
                                  )
    #S(xl-winhttp.api:request description "GET /" ...)
    ```

  * 残りのキーワードは MIME パートのヘッダにそのまま使われます。

    デフォルトで、これらの手続きはリクエストメッセージに "Host" ヘッダ・フィールドを追加するだけです。
    他のヘッダ・フィールドを追加するためにキーワード引数を与えることができます。

    ```lisp
    http-client.gauche> (http-get "twitter.com" "/"
                                  :accept-language "en")
    ```

#### 戻り値

  手続きは 3 つの値を返します。

  * 1 つ目は、RFC2616 で定義されているステータスコードの文字列値(例えば、成功時の "200" など)です。

  * 2 つ目は、パーズされたヘッダのリストで、リストの要素は(header-name value …)です。

    `header-name` はヘッダの文字列名 (例えば、 "content-type" や "location" など) で、
    `value` は対応する値の文字列値です。

    ヘッダ名は小文字に変換されます。
    サーバが同じ名前のヘッダを 1 つ以上返した場合は、 1 つのリストに統合されます。
    それ以外では、2 つ目の戻り値におけるヘッダのリストの順番は、サーバの応答での順番と同じです。

  * 3 つ目の戻り値は、サーバの応答におけるメッセージボディです。

    デフォルトでは、文字列で表現されたメッセージボディそのものです。
    サーバの応答がボディを持たない場合、3 つ目の戻り値は nil です。

    キーワード引数によって、メッセージボディがどのように扱われるかを制御できます。
    例えば、中間的な文字列を作らずに、返されたメッセージボディを直接ファイルに格納することが出来ます。


### Function: <a name="http-compose-query"><em>http-compose-query</em></a> <i>`PATH` `PARAMS` &optional `ENCODING`</i>

`PATH` と `PARAMS` からリクエスト URI を作成します。

  * `ENCODING` を指定した場合は、`PARAMS` のエンコーディングを変換した上で url エンコードを行います。
  * `PARAMS` が文字列の場合はそのままサーバに渡されるので、呼び出し側で必要な文字コード変換や
    url エンコーディングを行う必要があります。

```lisp
http-client.gauche> (http-compose-query "/search"
                                        '((q "xyzzy 読み方") (num 30)))
"/search?q=xyzzy%20%93%C7%82%DD%95%FB&num=30"

http-client.gauche> (http-compose-query "/search"
                                        '((q "xyzzy 読み方") (num 30))
                                        *encoding-utf8n*)
"/search?q=xyzzy%20%E8%AA%AD%E3%81%BF%E6%96%B9&num=30"

http-client.gauche> (http-compose-query "/search"
                                        "q=xyzzy%20%93%C7%82%DD%95%FB&num=30")
"/search?q=xyzzy%20%93%C7%82%DD%95%FB&num=30"
```

### Function: <a name="http-compose-form-data"><em>http-compose-form-data</em></a> <i>`PARAMS` `PORT` &optional `ENCODING`</i>

`PARAMS` から multipart/form-data 形式のリクエストを作成します。

  * `PORT` に output-stream を指定した場合はリクエストを `PORT` に書き込み、
    `PORT` と boundary 文字列を多値で返します。
  * `PORT` に nil を指定した場合はリクエストと boundary 文字列を多値で返します。

```lisp
http-client.gauche> (setf body '((q "xyzzy 読み方") (num 50)))
((q "xyzzy 読み方") (num 50))

http-client.gauche> (http-compose-form-data
                     body nil)
"--boundary-7hcecyuy0h20wigl8hrrn0jior89i24vtfrifjebd
Content-Disposition: form-data; name=\"q\"

xyzzy 読み方
--boundary-7hcecyuy0h20wigl8hrrn0jior89i24vtfrifjebd
Content-Disposition: form-data; name=\"num\"

nil
--boundary-7hcecyuy0h20wigl8hrrn0jior89i24vtfrifjebd--
" ;
"boundary-7hcecyuy0h20wigl8hrrn0jior89i24vtfrifjebd"

http-client.gauche> (http-compose-form-data
                     body
                     (make-buffer-stream
                      (get-buffer-create "*form*")))
#<buffer stream 79566604> ;
"boundary-23vt3uiri0nrp4ftwbnwnuiajo4bvl6r2jcixdyv0"
```

### Function: <a name="http-secure-connection-available?"><em>http-secure-connection-available?</em></a>

この関数は常に t を返します。
Gauche との互換性のために用意されています。
