#!/bin/fish

stack build
set ORIGIN_BIN_NAME todo-hs-exe
set STACK_BINS (stack path --local-install-root)

mkdir -p build
cp "$STACK_BINS/bin/$ORIGIN_BIN_NAME" ./build/todo

echo $STACK_BINS
