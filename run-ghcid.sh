#! /bin/bash

sudo sysctl -w kern.maxfiles=20480
sudo sysctl -w kern.maxfilesperproc=30000
ulimit -n 20000
exec ghcid \
  --test 'DevelMain.update' -W --color=always \
  -c 'stack ghci --extra-include-dirs=/usr/local/opt/icu4c/include/ --extra-lib-dirs=/usr/local/opt/icu4c/lib/ src/DevelMain.hs' \
  --reverse-errors --no-height-limit \
  --restart stack.yaml \
  --restart package.yaml \
  --allow-eval \
  --clear
