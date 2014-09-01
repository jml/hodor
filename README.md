# hodor

*A simple-minded todo list system*

## What does it do?

It's a todo list.

You can *add items*:

```
$ hodor add Buy cereal
01 2014-09-01 Buy cereal
HODOR: 1 added.
```

View them:

```
$ hodor ls
01 2014-09-01 Buy cereal
--
HODOR: 1 of 1 items shown
```

Mark them as *done*:

```
$ hodor do 1
01 x 2014-09-01 2014-09-01 Buy cereal
HODOR: 1 marked as done.
```

Run `hodor --help` to get the full range of commands.

## Why use this?

Probably you don't want to. Hodor was written as a clone of
[todotxt](http://todotxt.com/), which is used much more widely and is much
better maintained.

## Why does this exist?

So I could try to write a command-line utility in Haskell. It's been fun.


Released under the Apache 2.0 license, copyright Jonathan M. Lange.
