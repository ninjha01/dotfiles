#!/bin/bash
# -*-sh-*- # Convert list of decimal numbers into hex


for arg in "$@" ; do
  printf "%d\t= 0x%x\n" $arg $arg;
done
