#!/bin/bash

num=$1

for i in $(seq 1 $num);
do
  cp README.md "testfile$i.md"
done
