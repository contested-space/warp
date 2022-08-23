warp
=====

`warp` is a real time economic simulator that represents an arbitrary large 3d galaxy.

Very WIP.

Dependencies: erlang 25.0.4 or docker.

Build
-----
    Erlang:
    $ rebar3 compile

    Docker:
    Build docker image:
    $ docker image build --tag warp:dev .

    Run docker with a bind on your local directory:
    $ docker container run -it -v $(pwd):/warp:rw warp:dev

    Compile from within the container:
    $ rebar3 compile
