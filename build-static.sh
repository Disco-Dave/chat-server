#!/bin/bash

set -ex

docker build -t chat-server-build .

mkdir -p ./bin
stack --docker install --flag chat-server:static --local-bin-path ./bin


