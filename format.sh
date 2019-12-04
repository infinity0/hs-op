#!/bin/bash
shopt -s globstar
shopt -s nullglob

brittany "$@" --write-mode=inplace {src,test}/**/*.hs
stylish-haskell -i {src,test}/**/*.hs
