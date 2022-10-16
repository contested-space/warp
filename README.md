warp
=====

`warp` (working title) is a real time economic simulator that represents an arbitrary large 3d galaxy.

Very WIP.

Dependencies: erlang 25.0.4 or docker.

Build
-----
    Erlang:
    $ rebar3 as dev compile

    Docker:
    Build docker image:
    $ docker image build --tag warp:dev .

    Run docker with a bind on your local directory:
    $ docker container run -it -v $(pwd):/warp:rw warp:dev
    (or run `./dev.sh)

    Compile from within the container:
    $ rebar3 compile

Run
-----
   running `./dev.sh` will start the development Docker image
   and expose port 8080.

   Start the application with
   $ rebar3 shell
