# http-client API Reference

  * [PACKAGES](#packages)
    * [http-client.api](#http-client.api)
  * [VARIABLES](#variables)
    * [\*http-user-agent\*](#*http-user-agent*)
    * [\*http-accept-type\*](#*http-accept-type*)
    * [\*http-redirect-policy\*](#*http-redirect-policy*)
  * [CONDITIONS](#conditions)
    * [http-error](#http-error)
      * [http-error-cause](#http-error-cause)
      * [http-error-keyword](#http-error-keyword)
    * [http-abort-error](#http-abort-error)
    * [http-connect-error](#http-connect-error)
    * [http-request-error](#http-request-error)
    * [http-timeout-error](#http-timeout-error)
  * [STRUCTS](#structs)
    * [http-progress](#http-progress)
      * [http-progress-p](#http-progress-p)
      * [http-progress-response-p](#http-progress-response-p)
      * [http-progress-current](#http-progress-current)
      * [http-progress-total](#http-progress-total)
      * [http-progress-percent](#http-progress-percent)
  * [FUNCTIONS/MACROS](#function-macros)
    * request API
      * [http-get](#http-get)
      * [http-head](#http-head)
      * [http-post](#http-post)
      * [http-put](#http-put)
      * [http-delete](#http-delete)
      * [http-request](#http-request)
    * receiver API
      * [http-string-receiver](#http-string-receiver)
      * [http-file-receiver](#http-file-receiver)
      * [http-buffer-receiver](#http-buffer-receiver)
      * [http-oport-receiver](#http-oport-receiver)
      * [http-null-receiver](#http-null-receiver)
      * [http-general-receiver](#http-general-receiver)
      * [http-cond-receiver](#http-cond-receiver)
    * request control
      * [http-request-abort](#http-request-abort)
      * [http-response-wait](#http-response-wait)
      * [http-request-aborted-p](#http-request-aborted-p)
      * [http-request-completed-p](#http-request-completed-p)
      * [http-request-waiting-p](#http-request-waiting-p)
    * accessors
      * [http-request-uri](#http-request-uri)
      * [http-request-header](#http-request-header)
      * [http-request-header-alist](#http-request-header-alist)
      * [http-response-status](#http-response-status)
      * [http-response-status-text](#http-response-status-text)
      * [http-response-header](#http-response-header)
      * [http-response-header-alist](#http-response-header-alist)
      * [http-response-result](#http-response-result)
      * [http-response-values](#http-response-values)
    * utilities
      * [http-compose-query](#http-compose-query)
      * [http-compose-form-data](#http-compose-form-data)
      * [http-date-from-universal-time](#http-date-from-universal-time)
      * [http-date-to-universal-time](#http-date-to-universal-time)
      * [http-client-version](#http-client-version)


----

## <a name="packages">PACKAGES</a>


### Package: <a name="http-client.api"><em>http-client.api</em></a>

このパッケージは、[RFC2616 "Hypertext Transfer Protocol - HTTP/1.1"]
で定義されている HTTP/1.1 に対する簡単なクライアント API を提供します。

パッケージのニックネームは以下のとおりです。

  * `http-client`

  [RFC2616 "Hypertext Transfer Protocol - HTTP/1.1"]: http://tools.ietf.org/html/rfc2616

----

## <a name="variables">VARIABLES</a>


### Variable: <a name="*http-user-agent*"><em>\*http-user-agent\*</em></a>

User-Agent ヘッダに渡される値のデフォルト値を指定するスペシャル変数です。
デフォルトの値は `"xyzzy/(xyzzy のバージョン)"` です。

各アプリケーションは適切な値を設定するようにしてください。


### Variable: <a name="*http-accept-type*"><em>\*http-accept-type\*</em></a>

Accept ヘッダに渡される値のデフォルト値を指定するスペシャル変数です。
デフォルトの値は `"*/*"` です。


### Variable: <a name="*http-redirect-policy*"><em>\*http-redirect-policy\*</em></a>

リダイレクトが発生した場合のデフォルトの処理を指定スペシャル変数です。
以下の値を設定可能です。

  * `:never`: リダイレクトしない
  * `:always`: 常にリダイレクトする
  * `:disallow-https-to-http`: HTTPS から HTTP へのリダイレクトのみしない

デフォルトは `:disallow-https-to-http` です。

__See Also:__

  * [http-request](#http-request)

----

## <a name="conditions">CONDITIONS</a>

### Condition: <a name="http-error"><em>http-error</em></a>

HTTP 通信でエラーが発生した場合に通知されるコンディションです。

継承関係は以下のとおりです。

  * simple-error
    * http-error
      * [http-abort-error](#http-abort-error)
      * [http-connect-error](#http-connect-error)
      * [http-request-error](#http-request-error)
      * [http-timeout-error](#http-timeout-error)

以下のスロットが定義されています。

  * `cause`

    元となったエラーが設定されます。

  * `keyword`

    発生したエラーをあらわすキーワードが設定されます。
    キーワードは [WinHTTP Error Messages] のエラー名に対応します。

    例): `ERROR_WINHTTP_NAME_NOT_RESOLVED` なら `:name-not-resolved`

  [WinHTTP Error Messages]: http://msdn.microsoft.com/en-us/library/windows/desktop/aa383770.aspx

__See Also:__

  * [http-error-cause](#http-error-cause)
  * [http-error-keyword](#http-error-keyword)


### Condition: <a name="http-abort-error"><em>http-abort-error</em></a>

通信を中断した場合に通知されるコンディションです。

__See Also:__

  * [http-error](#http-error)
  * [http-request-abort](#http-request-abort)


### Condition: <a name="http-connect-error"><em>http-connect-error</em></a>

サーバに接続できない場合に通知されるコンディションです。

__See Also:__

  * [http-error](#http-error)


### Condition: <a name="http-request-error"><em>http-request-error</em></a>

サーバとの通信に失敗した場合に通知されるコンディションです。

__See Also:__

  * [http-error](#http-error)


### Condition: <a name="http-timeout-error"><em>http-timeout-error</em></a>

通信中にタイムアウトが発生した場合に通知されるコンディションです。

__See Also:__

  * [http-error](#http-error)


### Accessor: <a name="http-error-cause"><em>http-error-cause</em></a> <i>`X`</i>

[http-error](#http-error) コンディションの `cause` スロットを取得します。

__See Also:__

  * [http-error](#http-error)


### Accessor: <a name="http-error-keyword"><em>http-error-keyword</em></a> <i>`X`</i>

[http-error](#http-error) コンディションの `keyword` スロットを取得します。

__See Also:__

  * [http-error](#http-error)


----

## <a name="structs">STRUCTS</a>


### Struct: <a name="http-progress"><em>http-progress</em></a>

リクエスト・ボディの送信またはレスポンス・ボディの受信処理の進捗状況を表す構造体です。

以下のスロットが定義されています。

  * `response-p`

    レスポンス・ボディの受信処理の場合 t が設定されます。

  * `current`

    現在までに送受信したバイトサイズが設定されます。

  * `total`

    Content-Length ヘッダの値が設定されます。
    送受信するデータが惣菜しない場合や Content-Length が不明な場合は nil が設定されます。

__See Also:__

  * [http-progress-p](#http-progress-p)
  * [http-progress-response-p](#http-progress-response-p)
  * [http-progress-current](#http-progress-current)
  * [http-progress-total](#http-progress-total)
  * [http-progress-percent]](#http-progress-percent)


### Accessor: <a name="http-progress-p"><em>http-progress-p</em></a> <i>`X`</i>

`X` が [http-progress](#http-progress) 構造体のインスタンスなら t を返します。

__See Also:__

  * [http-progress](#http-progress)


### Accessor: <a name="http-progress-response-p"><em>http-progress-response-p</em></a> <i>`X`</i>

[http-progress](#http-progress) 構造体の `response-p` スロットを取得します。

__See Also:__

  * [http-progress](#http-progress)


### Accessor: <a name="http-progress-current"><em>http-progress-current</em></a> <i>`X`</i>

[http-progress](#http-progress) 構造体の `current` スロットを取得します。

__See Also:__

  * [http-progress](#http-progress)


### Accessor: <a name="http-progress-total"><em>http-progress-total</em></a> <i>`X`</i>

[http-progress](#http-progress) 構造体の `total` スロットを取得します。

__See Also:__

  * [http-progress](#http-progress)


### Function: <a name="http-progress-percent"><em>http-progress-percent</em></a> <i>`X`</i>

[http-progress](#http-progress) 構造体の進捗率 (%) を整数で取得します。

`total` スロットが `nil` または 0 以下の場合は `nil` を返します。


----

## <a name="function-macros">FUNCTIONS/MACROS</a>


### Function: <a name="http-get"><em>http-get</em></a> <i>`URI` &key `:headers` `:query` `:encoding` `:auth` `:proxy-auth` `:proxy` `:no-redirect` `:receiver` `:wait` `:onprogress` `:oncomplete` `:onabort` `:onerror`</i>

HTTP GET リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。


### Function: <a name="http-head"><em>http-head</em></a> <i>`URI` &key `:headers` `:query` `:encoding` `:auth` `:proxy-auth` `:proxy` `:no-redirect` `:receiver` `:wait` `:onprogress` `:oncomplete` `:onabort` `:onerror`</i>

HTTP HEAD リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。


### Function: <a name="http-post"><em>http-post</em></a> <i>`URI` `BODY` &key `:headers` `:query` `:encoding` `:auth` `:proxy-auth` `:proxy` `:no-redirect` `:receiver` `:wait` `:onprogress` `:oncomplete` `:onabort` `:onerror`</i>

HTTP POST リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。


### Function: <a name="http-put"><em>http-put</em></a> <i>`URI` `BODY` &key `:headers` `:query` `:encoding` `:auth` `:proxy-auth` `:proxy` `:no-redirect` `:receiver` `:wait` `:onprogress` `:oncomplete` `:onabort` `:onerror`</i>

HTTP PUT リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。


### Function: <a name="http-delete"><em>http-delete</em></a> <i>`URI` &key `:headers` `:query` `:encoding` `:auth` `:proxy-auth` `:proxy` `:no-redirect` `:receiver` `:wait` `:onprogress` `:oncomplete` `:onabort` `:onerror`</i>

HTTP DELETE リクエストを送信します。

詳細は [http-request](#http-request) を参照してください。


### Function: <a name="http-request"><em>http-request</em></a> <i>`METHOD` `URI` `BODY` &key `:headers` `:query` `:encoding` `:auth` `:proxy-auth` `:proxy` `:no-redirect` (`:receiver` (http-string-receiver)) `:wait` `:onprogress` `:oncomplete` `:onabort` `:onerror`</i>

`URI` に `METHOD` で指定されたリクエストを送信して Future オブジェクトを返します。

#### 引数

  * `METHOD`: 送信する HTTP メソッドを指定します。

  * `URI`: URL を指定します。

  * `BODY`: 送信する HTTP レスポンス・ボディを文字列かリストで指定します。

    文字列の場合はそのまま送信します。
    リストの場合は multipart/form-data 形式にエンコードして送信します。

    リストの要素は `(name value)` のような名前と値のリスト、
    または `(name :file filename :content-type type)` のように
    名前の後に plist を付加したものです。

    例:

    ```lisp
    '(("submit" "OK"
      ("filename" :file "icon.png" :content-type "image/png" :content-transfer-encoding "binary"))
    ```

    plist には以下のキーワードを指定可能です。

    * `:value`:
      パラメータの値を指定します。`(name val)` 形式は `(name :value value)` の省略形です。

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

    `:encoding` キーワード引数が与えられた場合、
    `name` と `value` はまずその文字エンコーディングに変換されたのちに url エスケープされます。

  * `:headers`

    送信時の HTTP ヘッダを alist または plist で指定します。

    ```lisp
    http-client.api> (http-get "https://twitter.com/"
                               :headers '(:Accept-Language "en"))
    ```

  * `:query`: URL パラメータを文字列かリストで指定します。

    文字列の場合そのまま `URL` に追加されて送信されます。
    呼び出し側で必要な文字コード変換や url エンコーディングを行う必要があります。

    リストの場合は、`name` と `value` を alist または plist で指定します。

    `name` と `value` の alist/plist から、application/x-www-form-urlencoded 形式の
    クエリ文字列を構成し `URL` に追加します。

    例えば次のリクエストは同じ効果を持ちます。
    二番目以降の呼び出しでは url エスケープが自動的に行われることに注目してください。

    ```lisp
    (http-get "https://www.google.co.jp/search?q=xyzzy%20%93%C7%82%DD%95%FB")
    (http-get "https://www.google.co.jp/search" :query "q=xyzzy%20%93%C7%82%DD%95%FB")
    (http-get "https://www.google.co.jp/search" :query '(:q "xyzzy 読み方"))
    (http-get "https://www.google.co.jp/search" :query '(("q" . "xyzzy 読み方")))
    ```

    `:encoding` キーワード引数が与えられた場合、
    `name` と `value` はまずその文字エンコーディングに変換されたのちに url エスケープされます。

  * `:encoding`

    `:query` や `BODY` がリストで与えられた場合、パラメータの名前や値は
    まずこの引数で指定される文字エンコーディングへと変換され、
    その後、application/x-www-form-urlencoded や multipart/form-data MIME 形式に
    したがったエンコーディングが行われます。

    `:query` や `BODY` に文字列を与えた場合は、文字エンコーディング変換は行われません。
    呼び出し側で望みの文字コードにあらかじめ変換しておいてください。

  * `:auth`

    サーバに対する認証情報をリストで指定します。

    ```lisp
   :auth '(:basic "user" "pass")
   :auth '(:digest "user" "pass")
    ```

  * `:proxy-auth`

    プロキシに対する認証情報をリストで指定します。

    ```lisp
   :proxy-auth '(:basic "user" "pass")
   :proxy-auth '(:digest "user" "pass")
    ```

  * `:proxy`

    http プロキシサーバを文字列、リスト、キーワードで指定します。

    `nil` を指定した場合はプロキシサーバを利用しません。
    何も指定しなかった場合は、WinHTTP のデフォルト設定のプロキシが利用されます。

    `:ie` を指定すると IE のプロキシ設定を利用します。
    この場合、プロキシの解決には WPAD (Web Proxy Auto-Discovery) による自動検出
    および自動構成スクリプトが利用可能になりますが、
    Ctrl-g が効かない同期処理になるので注意してください。

     ```lisp
    ;; プロキシ名を指定
    :proxy "my.proxy.com"
    :proxy "my.proxy.com:8080"

    ;; プロキシ名とバイパスを指定
    :proxy '(:proxy-name "my.proxy.com:8080" :proxy-bypass "192.168.*.*" :access-type :named-proxy)

    ;; IE のプロキシ設定を利用する
    :proxy :ie

    ;; WinHTTP のプロキシ設定を利用する
    :proxy :default

    ;; proxy を利用しない
    :proxy :no
    :proxy nil
    ```

  * `:no-redirect`

    `non-nil` を指定した場合、リダイレクションには従わなくなります。

    `nil` を指定した場合は [\*http-redirect-policy\*](#*http-redirect-policy*) の設定に従います。

  * `:receiver`

    レスポンス・ボディがどのように扱われるかをカスタマイズできます。
    `:receiver` には「1 引数を受け取る関数」を返す、3 引数を受け取る関数を指定します。

    レスポンス・ヘッダを受信し終わると、`:receiver` を
    ステータスコード、レスポンス・ヘッダ、Content-Length を指定して呼び出します。

    レスポンス・ボディを受信すると、`:receiver` が返した関数を受信したメッセージ？ボディを
    指定して呼び出します。

    レスポンス・ボディの終端に到達すると、`:receiver` が返した関数に `nil` を指定して呼び出します。
    その時の戻り値が [http-response-result](#http-response-result) の戻り値となります。

    `:receiver` のデフォルト値は [http-string-receiver](#http-string-receiver) です。

    receiver には以下の値を指定可能です。

      * [http-string-receiver](#http-string-receiver)
      * [http-file-receiver](#http-file-receiver)
      * [http-buffer-receiver](#http-buffer-receiver)
      * [http-oport-receiver](#http-oport-receiver)
      * [http-null-receiver](#http-null-receiver)
      * [http-general-receiver](#http-general-receiver)
      * [http-cond-receiver](#http-cond-receiver)

  * `:wait`

    `wait` に `non-nil` を指定した場合、同期モードで動作します。
    リクエストが完了するまで関数は処理を返しません。

  * `:onprogress`, `:oncomplete`, `:onabort`, `:onerror`

    `:oncomplete` には 4 引数を取る手続きを、`:onprogress`、`:onabort` および `:onerror`
    には 1 引数を取る関数を指定してください。

    `nil` を指定した場合は何も呼び出されません。

    * `:onprogress`: リクエスト・ボディの送信中またはレスポンス・ボディの受信中に
      呼び出されます。引数は [http-progress](#http-progress) オブジェクトです。

      ```lisp
      http-client.api> (http-get "http://www.google.co.jp/"
                                 :onprogress #'(lambda (progress)
                                                 (message "~A" progress)))
      ```

    * `:oncomplete`: リクエストが完了した場合に呼び出されます。
      引数は レスポンス・ボディ、ステータスコード、レスポンス・ヘッダ、URL です。
      URL はリダイレクト後の URL であるため、`URI` で指定した URL と違う場合があります。

    * `:onabort`: リクエスト中断時に呼び出されます。
      引数はコンディションです。

    * `:onerror`: エラー発生時に呼び出されます。
      引数はコンディションです。


#### 戻り値

戻り値は同期・非同期にかかわらず Future オブジェクトです。

`:wait` に `non-nil` を指定しない場合、API を呼び出すとすぐに制御を返します。

  * [http-response-values](#http-response-values) などで、Future オブジェクトから値を
    取得しようとした時点で、まだリクエストが完了していない場合はブロックします。
  * [http-response-wait](#http-response-wait) で明示的にリクエストの完了を待つことが可能です。
  * リクエストが完了したかどうかは [http-request-completed-p](#http-request-completed-p)
    で判断できます。
  * リクエストを停止したい場合は Future オブジェクトを
     [http-request-abort](#http-request-abort) に指定します。

__See Also:__

  * [\*http-user-agent\*](#*http-user-agent*)
  * [\*http-accept-type\*](#*http-accept-type*)
  * [\*http-redirect-policy\*](#*http-redirect-policy*)
  * [http-request-uri](#http-request-uri)
  * [http-request-header](#http-request-header)
  * [http-request-header-alist](#http-request-header-alist)
  * [http-response-status](#http-response-status)
  * [http-response-status-text](#http-response-status-text)
  * [http-response-header](#http-response-header)
  * [http-response-header-alist](#http-response-header-alist)
  * [http-response-result](#http-response-result)
  * [http-response-values](#http-response-values)
  * [http-request-completed-p](#http-request-completed-p)
  * [http-request-waiting-p](#http-request-waiting-p)
  * [http-request-abort](#http-request-abort)
  * [http-response-wait](#http-response-wait)


### Function: <a name="http-string-receiver"><em>http-string-receiver</em></a> <i>&key (`:encoding` t)</i>

レスポンス・ボディを文字列で受信するための receiver です。

  * デフォルトの receiver です。
  * `:encoding` にエンコーディングを指定した場合は、指定されたエンコーディングで
    内部エンコーディングに変換します。
  * `:encoding` に `non-nil` を指定した場合は、Content-Type の charaset パラメータで
    指定されたエンコーディングで内部エンコーディングに変換します。
    charaset からエンコーディングを特定できない場合、
    内部エンコーディングへの変換は行いません。
  * `:encoding` に `nil` または `:binary` を指定した場合は、
    内部エンコーディングへの変換は行いません。
  * `:encoding` を指定した場合、改行コードも内部コードに変換します。
  * [http-response-result](#http-response-result) はレスポンス・ボディを文字列で返します。

```lisp
http-client.api> (http-response-result
                  (http-get "http://zip.ricollab.jp/1120002.json"))
"{
  \"zipcode\": \"1120002\",
  \"address\": {
    \"prefecture\": \"東京都\",
    \"city\": \"文京区\",
    \"town\": \"小石川\"
  },
  \"yomi\": {
    \"prefecture\": \"トウキョウト\",
    \"city\": \"ブンキョウク\",
    \"town\": \"コイシカワ\"
  }
}
"

http-client.api> (http-response-result
                  (http-get "http://zip.ricollab.jp/1120002.json"
                            :receiver (http-string-receiver :encoding *encoding-utf8n*)))
"{
  \"zipcode\": \"1120002\",
  \"address\": {
    \"prefecture\": \"東京都\",
    \"city\": \"文京区\",
    \"town\": \"小石川\"
  },
  \"yomi\": {
    \"prefecture\": \"トウキョウト\",
    \"city\": \"ブンキョウク\",
    \"town\": \"コイシカワ\"
  }
}
"

http-client.api> (http-response-result
                  (http-get "http://zip.ricollab.jp/1120002.json"
                            :receiver (http-string-receiver :encoding nil)))
"{
  \"zipcode\": \"1120002\",
  \"address\": {
    \"prefecture\": \"譚ｱ莠ｬ驛ｽ\",
    \"city\": \"譁?ｺｬ蛹ｺ\",
    \"town\": \"蟆冗浹蟾?
  },
  \"yomi\": {
    \"prefecture\": \"繝医え繧ｭ繝ｧ繧ｦ繝?,
    \"city\": \"繝悶Φ繧ｭ繝ｧ繧ｦ繧ｯ\",
    \"town\": \"繧ｳ繧､繧ｷ繧ｫ繝ｯ\"
  }
}
"
```

__See Also:__

  * [http-request](#http-request)


### Function: <a name="http-file-receiver"><em>http-file-receiver</em></a> <i>`FILENAME` &key (`:encoding` :binary) (`:if-exists` :new-version) (`:share` nil)</i>

レスポンス・ボディを `FILENAME` で指定したファイルに保存するための receiver です。
受信しながらファイルに書き込むため、巨大なファイルをダウンロードする場合でもメモリを圧迫しません。

  * `:encoding`, `:if-exists`, `:share` は open 関数にそのまま指定されます。
  * `:encoding` のデフォルト値は `:binary` です。
  * `:if-exists` のデフォルト値は `:new-version` です。
  * `:share` のデフォルト値は `nil` です。
  * [http-response-result](#http-response-result) はファイルのフルパスを文字列で返します。

```lisp
(defun http-download (uri localfile)
  (http-get uri
            :receiver (http-file-receiver localfile)
            :oncomplete #'(lambda (fullpath status headers uri)
                            (msgbox "Download OK~%URL: ~A~%File: ~A"
                                    uri fullpath))
            :onerror #'(lambda (err) (msgbox "Error: ~A" err))
            :onabort #'(lambda (err) (msgbox "Abort: ~A" err))
            ))
```

__See Also:__

  * [http-request](#http-request)


### Function: <a name="http-buffer-receiver"><em>http-buffer-receiver</em></a> <i>`BUFFER` &key (`:encoding` t)</i>

レスポンス・ボディを `BUFFER` で指定したバッファに書きこむための receiver です。

  * `BUFFER` にバッファ名を指定した場合、同名のバッファがあった場合でも新規にバッファを作成します。
  * `BUFFER` にバッファを指定した場合、指定したバッファの末尾に書き込みます。
  * `:encoding` にエンコーディングを指定した場合は、指定されたエンコーディングで
    内部エンコーディングに変換します。
  * `:encoding` に `non-nil` を指定した場合は、Content-Type の charaset パラメータで
    指定されたエンコーディングで内部エンコーディングに変換します。
    charaset からエンコーディングを特定できない場合、
    内部エンコーディングへの変換は行いません。
  * `:encoding` に `nil` または `:binary` を指定した場合は、
    内部エンコーディングへの変換は行いません。
  * `:encoding` を指定した場合、改行コードも内部コードに変換し、
    buffer-eol-code を適切に設定します。
  * [http-response-result](#http-response-result) はバッファを返します。

```lisp
;; www.baidu.com  は Content-Type: text/html;charset=gb2312 なので自動判別可能だが、
;; news.baidu.com は Content-Type: text/html なので encoding を明示する必要がある
http-client.api> (http-response-values
                  (http-get "http://news.baidu.com/"
                            :receiver (http-buffer-receiver "baidu"
                                                            :encoding *encoding-euc-gb*)
                            :oncomplete #'(lambda (buffer status headers uri)
                                            (pop-to-buffer buffer)
                                            (refresh-screen))
                            :onerror #'(lambda (err) (msgbox "Error: ~A" err))
                            :onabort #'(lambda (err) (msgbox "Abort: ~A" err))
                            ))
#<buffer: baidu>
200
(... ("Content-Type" . "text/html") ...)
"http://news.baidu.com/"
```

__See Also:__

  * [http-request](#http-request)


### Function: <a name="http-oport-receiver"><em>http-oport-receiver</em></a> <i>`SINK` `FLUSHER` &key (`:encoding` t) `:close` `:finish-output`</i>

レスポンス・ボディを `SINK` で指定したストリームに書きこむための receiver です。

  * `SINK` には出力ストリームを、`FLUSHER` には `SINK` を引数に取る関数を指定します。
  * `:encoding` にエンコーディングを指定した場合は、指定されたエンコーディングで
    内部エンコーディングに変換します。
  * `:encoding` に `non-nil` を指定した場合は、Content-Type の charaset パラメータで
    指定されたエンコーディングで内部エンコーディングに変換します。
    charaset からエンコーディングを特定できない場合、
    内部エンコーディングへの変換は行いません。
  * `:encoding` に `nil` または `:binary` を指定した場合は、
    内部エンコーディングへの変換は行いません。
  * `:encoding` を指定した場合、改行コードも内部コードに変換します。
  * `:finish-output` に `non-nil` を指定すると、レスポンス・ボディ終端に達した時点で
    `SINK` を finish-output します。
    デフォルトは `nil` です。
  * `:close` に `non-nil` を指定すると、レスポンス・ボディ終端に達した時点で
    `SINK` を close します。
    デフォルトは `nil` です。
  * [http-response-result](#http-response-result) は `FLUSHER` の戻り値を返します。
    `FLUSHER` に `nil` を指定すると `SINK` を返します。

```lisp
http-client.api> (http-response-result
                  (http-get "http://www.google.co.jp/"
                            :receiver (http-oport-receiver *standard-output*
                                                           #'buffer-stream-buffer)))
<!doctype html>
  :
#<buffer: *REPL*>
```

__See Also:__

  * [http-request](#http-request)


### Function: <a name="http-null-receiver"><em>http-null-receiver</em></a>

レスポンス・ボディを読み捨てるための receiver です。

__See Also:__

  * [http-request](#http-request)


### Function: <a name="http-general-receiver"><em>http-general-receiver</em></a> <i>`CALLBACK` &key (`:encoding` t) `:line`</i>

レスポンス・ボディを受信するたびに `CALLBACK` で指定した任意の処理を行うための receiver です。

  * `:line` に `nil` を指定した場合:
    * 受信したレスポンスをそのまま `CALLBACK` に指定して呼び出します。
  * `:line` に `non-nil` を指定した場合:
    * 1 行ごとに分割して `CALLBACK` を呼び出します。
    * 受信したレスポンス・ボディに改行文字が含まれない場合、`CALLBACK` は呼び出さずに次のレスポンス・ボディを待ちます。
      次に改行文字を受信した時点でレスポンス・ボディを結合して `CALLBACK` を呼び出します。
    * レスポンス・ボディの終端に達した場合は、改行文字がなくてもそれまで受信していたレスポンス・ボディを
      結合して `CALLBACK` を呼び出します。
      このときにアプリケーションレベルでは不完全なデータが `CALLBACK` に指定される可能性があります。
      その後 `CALLBACK` に `nil` を指定して呼び出します。
      その時の戻り値が [http-response-result](#http-response-result) の戻り値となります。
  * `:encoding` にエンコーディングを指定した場合は、指定されたエンコーディングで
    内部エンコーディングに変換します。
  * `:encoding` に `non-nil` を指定した場合は、Content-Type の charaset パラメータで
    指定されたエンコーディングで内部エンコーディングに変換します。
    charaset からエンコーディングを特定できない場合、
    内部エンコーディングへの変換は行いません。
  * `:encoding` に `nil` または `:binary` を指定した場合は、
    内部エンコーディングへの変換は行いません。
  * `:encoding` を指定した場合、または `:line` を指定した場合は、改行コードも内部コードに変換します。
  * レスポンス・ボディの終端に達した場合は、`CALLBACK` に `nil` を指定して呼び出します。
    その時の戻り値が [http-response-result](#http-response-result) の戻り値となります。

```lisp
http-client.api> (defun chunk-size (url &key line)
                   (let* ((chunk-size)
                          (r (http-get url
                                       :receiver (http-general-receiver
                                                  #'(lambda (chunk)
                                                      (if chunk
                                                          (push (length chunk) chunk-size)
                                                        chunk-size))
                                                  :line line))))
                     (http-response-result r)))

http-client.api> (setf a (chunk-size "http://www.jsdlab.co.jp/~kamei/" :line t))
(9 10 81 8 9 23 85 21 69 19 102 19 54 ...)

http-client.api> (setf b (chunk-size "http://www.jsdlab.co.jp/~kamei/"))
(2502 579)

http-client.api> (values (apply #'+ a) (apply #'+ b))
3081 ;
3081
```

__See Also:__

  * [http-request](#http-request)


### Macro: <a name="http-cond-receiver"><em>http-cond-receiver</em></a> <i>(`STATUS` `HEADERS` `CONTENT-LENGTH`) `&BODY` `FORMS`</i>

レスポンスで条件分岐を行い receiver を選択するためのマクロです。

200 OK の場合は [http-file-receiver](#http-file-receiver) を利用してファイルをダウンロードし、
それ以外の場合は [http-string-receiver](#http-string-receiver) を利用してエラーメッセージを
文字列で受信するということができます。

  * `STATUS`, `HEADERS`, `CONTENT-LENGTH` は receiver が受け取る引数名を指定します。
  * `FORMS` はそのまま cond に展開されます。
  * `FORMS` は receiver を返すように実装してください。

```lisp
http-client.api> (defun http-download-sync (uri localfile)
                   (http-response-values
                    (http-get uri
                              :receiver (http-cond-receiver (status headers content-length)
                                          ((= status 200)
                                           (http-file-receiver localfile))
                                          (t
                                           (http-string-receiver)))
                              )))
http-download-sync

http-client.api> (http-download-sync "http://www.jsdlab.co.jp/~kamei/cgi-bin/download.cgi"
                                     "c:/xyzzy.lzh")
"c:/xyzzy.lzh"
200
(("Date" . "Wed, 01 Feb 2012 01:55:17 GMT") ("Server" . "Apache/2.0") ...)
"http://www.mars.dti.ne.jp/~t-kamei/xyzzy/xyzzy-0.2.2.235.lzh"

http-client.api> (http-download-sync "http://www.jsdlab.co.jp/~kamei/cgi-bin/download"
                                     "c:/xyzzy.lzh")
"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<HTML><HEAD>
<TITLE>404 Not Found</TITLE>
</HEAD><BODY>
<H1>Not Found</H1>
The requested URL /~kamei/cgi-bin/download was not found on this server.<P>
<HR>
<ADDRESS>Apache/1.3.41 Server at www.jsdlab.co.jp Port 80</ADDRESS>
</BODY></HTML>
"
404
(("DeleGate-Ver" . "7.9.6 (delay=1)") ("Date" . "Thu, 02 Feb 2012 03:25:23 GMT") ...)
"http://www.jsdlab.co.jp/~kamei/cgi-bin/download"
```

__See Also:__

  * [http-request](#http-request)


### Function: <a name="http-request-abort"><em>http-request-abort</em></a> <i>`CLIENT`</i>

指定した `CLIENT` を中断します。

引数には以下の関数の戻り値を指定可能です。

  * [http-get](#http-get)
  * [http-head](#http-head)
  * [http-post](#http-post)
  * [http-put](#http-put)
  * [http-delete](#http-delete)
  * [http-request](#http-request)

通信を中断したなら t を返します。
既に通信が終了していたら何もせず `nil` を返します。

```lisp
http-client.api> (let ((req (http-get "www.google.co.jp" "/" :async t)))
                   (values
                    (list (http-request-aborted-p req)
                          (http-request-waiting-p req)
                          (http-request-completed-p req))
                    (http-request-abort req)
                    (list (http-request-aborted-p req)
                          (http-request-waiting-p req)
                          (http-request-completed-p req))))
(nil t nil) ;
t ;
(t nil t)
```

__See Also:__

  * [http-response-wait](#http-response-wait)
  * [http-request-aborted-p](#http-request-aborted-p)
  * [http-request-completed-p](#http-request-completed-p)
  * [http-request-waiting-p](#http-request-waiting-p)


### Function: <a name="http-response-wait"><em>http-response-wait</em></a> <i>`CLIENT` &key `:nowait` `:no-redraw` `:sleep` (`:timeout` 30) (`:interval` 0.1) (`:ready-state` :complete) (`:signal-error` t)</i>

`CLIENT` で指定したリクエストが完了するのを待ちます。
リクエストが完了した場合は t を返します。

  * `:nowait` に `non-nil` を指定するとリクエストが完了していない場合は
    待ち合わせをせずにすぐに `nil` を返します。

    デフォルトは `nil` です。

  * `:no-redraw` に `non-nil` を指定するとリクエストの完了待ち中に
    画面の再描画を行いません。

    デフォルトは `nil` です。

  * `:sleep` に `non-nil` を指定するとリクエストの完了待ち中に
    キー入力があっても中断しません。

    `:sleep` が `nil` の場合キー入力があったら待ち合わせを中断します。
    中断時点でリクエスト完了していない場合は `nil` を返します。

    `:sleep` を指定した場合は画面の再描画を行いません。

    デフォルトは `nil` です。

  * `:timeout` を指定すると指定した秒数以内にリクエストが完了しない場合、
    `nil` を返します。

    `:timeout` に `nil` を指定するとタイムアウトせずに無限に待ち合わせます。

    デフォルトは 30 秒です。

  * `:interval` は監視間隔です。

    デフォルトは 0.1 秒です。

  * `:ready-state` は待ち合わせる状態を指定します。

    * :loading を指定するとレスポンス・ヘッダの受信完了を待ち合わせます。
    * :complete を指定するとレスポンス・ボディの受信完了を待ち合わせます。

    デフォルトは :complete です。

  * `:signal-error` に `non-nil` を指定すると、非同期処理で発生したエラーを再送します。

    デフォルトは t です。

__See Also:__

  * [http-request-abort](#http-request-abort)
  * [http-request-aborted-p](#http-request-aborted-p)
  * [http-request-completed-p](#http-request-completed-p)
  * [http-request-waiting-p](#http-request-waiting-p)


### Function: <a name="http-request-aborted-p"><em>http-request-aborted-p</em></a> <i>`CLIENT`</i>

`CLIENT` を [abort した場合](#http-request-abort) t を返します。

引数には以下の関数の戻り値を指定可能です。

  * [http-get](#http-get)
  * [http-head](#http-head)
  * [http-post](#http-post)
  * [http-put](#http-put)
  * [http-delete](#http-delete)
  * [http-request](#http-request)

__See Also:__

  * [http-request-abort](#http-request-abort)
  * [http-response-wait](#http-response-wait)
  * [http-request-completed-p](#http-request-completed-p)
  * [http-request-waiting-p](#http-request-waiting-p)


### Function: <a name="http-request-completed-p"><em>http-request-completed-p</em></a> <i>`CLIENT`</i>

指定した `CLIENT` が完了したなら t を返します。

  * リクエストを [abort した場合](#http-request-abort) でも t を返します。
  * リクエストが完了したか abort したかは、[http-request-aborted-p](#http-request-aborted-p) で区別します。

引数には以下の関数の戻り値を指定可能です。

  * [http-get](#http-get)
  * [http-head](#http-head)
  * [http-post](#http-post)
  * [http-put](#http-put)
  * [http-delete](#http-delete)
  * [http-request](#http-request)

__See Also:__

  * [http-request-abort](#http-request-abort)
  * [http-response-wait](#http-response-wait)
  * [http-request-aborted-p](#http-request-aborted-p)
  * [http-request-waiting-p](#http-request-waiting-p)


### Function: <a name="http-request-waiting-p"><em>http-request-waiting-p</em></a> <i>`CLIENT`</i>

指定した `CLIENT` がまだ処理中なら t を返します。

引数には以下の関数の戻り値を指定可能です。

  * [http-get](#http-get)
  * [http-head](#http-head)
  * [http-post](#http-post)
  * [http-put](#http-put)
  * [http-delete](#http-delete)
  * [http-request](#http-request)

__See Also:__

  * [http-request-abort](#http-request-abort)
  * [http-response-wait](#http-response-wait)
  * [http-request-aborted-p](#http-request-aborted-p)
  * [http-request-completed-p](#http-request-completed-p)


### Function: <a name="http-request-uri"><em>http-request-uri</em></a> <i>`CLIENT`</i>

リクエストした URL を取得します。
リダイレクトした場合はリダイレクト後の URL になります。

```lisp
http-client.api> (let ((r (http-get "http://goo.gl/bgggL")))
                   (http-request-uri r))
"http://www.jsdlab.co.jp/~kamei/"
```


### Function: <a name="http-request-header"><em>http-request-header</em></a> <i>`CLIENT` `HEADER`</i>

指定したリクエスト・ヘッダを取得します。
ヘッダ名は文字列またはシンボルで指定します (case-sensitive)。

```lisp
http-client.api> (let ((r (http-get "http://www.google.co.jp/")))
                   (http-request-header r "User-Agent"))
"xyzzy/0.2.2.235"

http-client.api> (let ((r (http-get "http://www.google.co.jp/")))
                   (http-request-header r :user-agent))
"xyzzy/0.2.2.235"
```

### Function: <a name="http-request-header-alist"><em>http-request-header-alist</em></a> <i>`CLIENT`</i>

すべてのリクエスト・ヘッダを alist で取得します。

```lisp
http-client.api> (let ((r (http-get "http://www.google.co.jp/"
                                    :headers `(:X-Foo 1))))
                   (http-request-header-alist r))
(("X-Foo" . "1")
 ("User-Agent" . "xyzzy/0.2.2.235")
 ("Connection" . "Keep-Alive"))
```

### Function: <a name="http-response-status"><em>http-response-status</em></a> <i>`CLIENT`</i>

ステータスコードを数値で取得します。


### Function: <a name="http-response-status-text"><em>http-response-status-text</em></a> <i>`CLIENT`</i>

ステータスコードを文字列で取得します。

```lisp
http-client.api> (let ((r (http-delete "http://www.google.co.jp/")))
                   (values (http-response-status r)
                           (http-response-status-text r)))
405 ;
"Method Not Allowed"
```


### Function: <a name="http-response-header"><em>http-response-header</em></a> <i>`CLIENT` `HEADER`</i>

指定したレスポンス・ヘッダを取得します。
ヘッダ名は文字列またはシンボルで指定します (case-sensitive)。

```lisp
http-client.api> (let ((r (http-get "http://www.jsdlab.co.jp/~kamei/")))
                   (http-response-header r "Last-Modified"))
"Wed, 07 Dec 2005 14:45:00 GMT"

http-client.api> (let ((r (http-get "http://www.jsdlab.co.jp/~kamei/")))
                   (http-response-header r :last-modified))
"Wed, 07 Dec 2005 14:45:00 GMT"
```


### Function: <a name="http-response-header-alist"><em>http-response-header-alist</em></a> <i>`CLIENT`</i>

すべてのレスポンス・ヘッダを alist で取得します。

```lisp
http-client.api> (let ((r (http-get "http://www.google.co.jp/")))
                   (http-response-header-alist r))
(("Cache-Control" . "private, max-age=0")
 ("Date" . "Wed, 01 Feb 2012 12:40:48 GMT")
 ("Transfer-Encoding" . "chunked")
 ...)
```


### Function: <a name="http-response-result"><em>http-response-result</em></a> <i>`CLIENT`</i>

receiver が返した結果を取得します。

__See Also:__

  * [http-string-receiver](#http-string-receiver)
  * [http-file-receiver](#http-file-receiver)
  * [http-buffer-receiver](#http-buffer-receiver)
  * [http-stream-chunk-receiver](#http-stream-chunk-receiver)
  * [http-stream-line-receiver](#http-stream-line-receiver)
  * [http-null-receiver](#http-null-receiver)


### Function: <a name="http-response-values"><em>http-response-values</em></a> <i>`CLIENT`</i>

以下の値を多値で返します。

  1. [http-response-result](#http-response-result)
  2. [http-response-status](#http-response-status)
  3. [http-response-header-alist](#http-response-header-alist)
  4. [http-request-uri](#http-request-uri)

```lisp
http-client.api> (http-response-values
                  (http-get "http://www.google.co.jp/"))
"<!doctype html>
</script>"
200
(("Cache-Control" . "private, max-age=0") ("Date" . "Thu, 02 Feb 2012 12:44:32 GMT") ...)
"http://www.google.co.jp/"
```


### Function: <a name="http-compose-query"><em>http-compose-query</em></a> <i>`PATH` `PARAMS` &optional `ENCODING`</i>

`PATH` と `PARAMS` からリクエスト URI を作成します。

  * `ENCODING` を指定した場合は、`PARAMS` のエンコーディングを変換した上で url エンコードを行います。
  * `PARAMS` が文字列の場合はそのままサーバに渡されるので、呼び出し側で必要な文字コード変換や
    url エンコーディングを行う必要があります。

```lisp
http-client.api> (http-compose-query "/search"
                                     '((q "xyzzy 読み方") (num 30)))
"/search?q=xyzzy%20%93%C7%82%DD%95%FB&num=30"

http-client.api> (http-compose-query "/search"
                                     '((q "xyzzy 読み方") (num 30))
                                     *encoding-utf8n*)
"/search?q=xyzzy%20%E8%AA%AD%E3%81%BF%E6%96%B9&num=30"

http-client.api> (http-compose-query "/search"
                                     "q=xyzzy%20%93%C7%82%DD%95%FB&num=30")
"/search?q=xyzzy%20%93%C7%82%DD%95%FB&num=30"
```

__See Also:__

  * [http-compose-form-data](#http-compose-form-data)


### Function: <a name="http-compose-form-data"><em>http-compose-form-data</em></a> <i>`PARAMS` `PORT` &optional `ENCODING`</i>

`PARAMS` から multipart/form-data 形式のリクエストを作成します。

  * `PORT` に output-stream を指定した場合はリクエストを `PORT` に書き込み、
    `PORT` と boundary 文字列を多値で返します。
  * `PORT` に `nil` を指定した場合はリクエストと boundary 文字列を多値で返します。

```lisp
http-client.api> (setf body '((q "xyzzy 読み方") (num 50)))
((q "xyzzy 読み方") (num 50))

http-client.api> (http-compose-form-data
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

http-client.api> (http-compose-form-data
                  body
                  (make-buffer-stream
                   (get-buffer-create "*form*")))
#<buffer stream 79566604> ;
"boundary-23vt3uiri0nrp4ftwbnwnuiajo4bvl6r2jcixdyv0"
```

__See Also:__

  * [http-compose-query](#http-compose-query)


### Function: <a name="http-date-from-universal-time"><em>http-date-from-universal-time</em></a> <i>`UNIVERSAL-TIME`</i>

universal-time を RFC 1123 で定義されている日付形式に変換します。

```lisp
http-client.api> (http-date-from-universal-time (encode-universal-time 1 2 3 4 5 2012 0))
"Fri, 04 May 2012 03:02:01 GMT"
```

__See Also:__

  * [http-date-to-universal-time](#http-date-to-universal-time)


### Function: <a name="http-date-to-universal-time"><em>http-date-to-universal-time</em></a> <i>`HTTP-DATE`</i>

RFC 1123 で定義されている日付形式を universal-time に変換します。

```lisp
http-client.api> (http-date-to-universal-time "Fri, 04 May 2012 03:02:01 GMT")
3545089321

http-client.api> (decode-universal-time 3545089321 0)
1 ;
2 ;
3 ;
4 ;
5 ;
2012 ;
4 ;
nil ;
0

http-client.api> (let* ((client (http-head "http://www.jsdlab.co.jp/~kamei/"))
                        (last-mod (http-response-header client :last-modified)))
                   (values last-mod
                           (http-date-to-universal-time last-mod)))
"Wed, 07 Dec 2005 14:45:00 GMT"
3342955500
```

__See Also:__

  * [http-date-from-universal-time](#http-date-from-universal-time)


### Function: <a name="http-client-version"><em>http-client-version</em></a>

本ライブラリのバージョンを文字列で返します。

```lisp
http-client.api> (http-client-version)
"0.0.1"
```
