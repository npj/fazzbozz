fazzbozz
========

A platform for exploring haskell, mostly by vastly overengineering fizzbuzz.

Usage:
```ShellSession
$ stack build
$ stack exec fazzbozz
1
2
fazz
4
bozz
...
fazz
19
bozz
$ fazzbozz -n 100
1
2
fazz
4
bozz
...
98
fazz
bozz
$ fazzbozz -p 2:foo -p 3:bar
1
foo
bar
foo
5
foobar
...
19
foo
$ fazzbozz -p fib:foo -p 5:bar
foo
foo
foo
4
foobar
6
...
19
bar
```
