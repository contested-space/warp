# syntax=docker/dockerfile:1
FROM erlang:25.0.4

LABEL author="Fabien Lamarche-Filion"

EXPOSE 8080

RUN useradd -ms /bin/bash warp
WORKDIR /warp
USER warp
CMD bash
