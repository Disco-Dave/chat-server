#!/bin/bash

(cd ../; stack run) &
api=$!

(npm run serve) &
client=$!

docker run --rm --name shortest-url-dev-nginx --net="host" -v $PWD/nginx.conf:/etc/nginx/nginx.conf:ro nginx:alpine &
proxy=$!

wait $api $client $project
