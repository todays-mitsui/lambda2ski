# lambda2ski

関数抽象(無名関数) を SKI コンビネータとして展開するトランスレータです。  
展開した式は [Lazy_K](http://legacy.e.tir.jp/wiliki?%CB%DD%CC%F5%3A%A5%D7%A5%ED%A5%B0%A5%E9%A5%DF%A5%F3%A5%B0%B8%C0%B8%ECLazy_K) のコードとして実行可能な形式になります。

## インストール

事前に [stack](https://docs.haskellstack.org/en/stable/README/) のセットアップが完了している必要があります。

```bash
$ git clone https://github.com/todays-mitsui/lambda2ski.git
$ cd lambda2ski
$ stack setup
$ stack install
```

コンパイル後のバイナリを PATH の通ったディレクトリに配置してください。

## 使い方

インストールが完了すると ski というコマンドが使えるようになります。  
ski コマンドに関数抽象を含む式を与えて実行すると、関数抽象を含まない SKI コンビネータに変換されます。

```bash
$ ski "^xy.`x`yy"
``s``s`ksk`k``sii
```

## オプション

`--load` オプションにファイルを指定すると、あらかじめ定義した変数を参照しながら式を展開することができます。

```bash
$ ski --load=sample.context "ISZERO"
``s``si`k`k`ki`kk
```

このファイルを便宜的に Context ファイルと呼ぶことにします。

## Context ファイル

Context ファイルの例を以下に示します。  
イコール `=` を使って変数を定義しています。手続き型のプログラミング言語に触れたことのある人であれば見慣れた構文でしょう。

```bash
# sample.context

ISZERO = ^n.``n^_.FALSE TRUE

TRUE  = ^xy.x  # 真
FALSE = ^xy.y # 偽
```

変数名には英数字とアンダースコア `_` が使えます。  
`HOGE`, `FUNCTION`, `HOGE_FUNCTION`, `42`, `_` などはどれも正しい変数名です。  
ただし、小文字は1文字でのみ正しい変数名と認識されます。そのため `x` は正しい変数名ですが、`xy` は2つの変数 `x y` が連なった物として扱われます。

また、`42` などのように整数リテラルのように見えるものであっても特別な意味は持ちません。
