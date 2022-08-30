#!/bin/bash
docker container run -it --publish 8080:8080 -v /home/fabien/code/warp:/warp:rw warp:dev
