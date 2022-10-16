#!/bin/bash
docker container run -it --publish 8080:8080 -v $(pwd):/warp:rw warp:dev
