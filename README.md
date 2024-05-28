# s-java
A Java parser implemented in Haskell, built on top of the [`s`](https://github.com/thyeem/s) parser.    
This is primarily a parser library, but it also includes an _executable_ for testing `Java` files in a playground environment.

## Install

```bash
$ cd ~
$ git clone https://github.com/thyeem/s

$ cd ~
$ git clone https://github.com/thyeem/s-java

# build: this creates `sj` executables in `app` directory
$ make build

# test
$ make test
$ make doctest
```

## Usage

```bash
$ app/sj -h
Usage: sj [-V|--version] [-v|--verbose] [-t|--test] FILE

Available options:
  -h,--help                Show this help text
  -V,--version             Print version of the program
  -v,--verbose             Increase verbosity
  -t,--test                Test parsing Java code
  FILE                     File or directory of code
  
$ app/sj -tv FILE.java  

$ app/sj -tv DIR/\*  
```
