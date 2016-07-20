# dnsserver
An modular DNS(SEC) server in Erlang.

[![Build Status](https://travis-ci.org/rikribbers/dnsserver.svg?branch=master)](https://travis-ci.org/rikribbers/dnsserver)

More or less based on:

- https://github.com/aetrion/erl-dns
- https://github.com/miekg/dns


# Building and testing

Using my dev image from hub.docker.io

    | => docker network create dns.dev
    
    | => docker run --rm -it -v ~/git/dnsserver/:/workspace  \ 
         -h dnsserver.dns.dev --net dns.dev rikribbers/erldev /bin/bash
         

# Running the image in cluster modes

Start a cluster contact node

    | => erl -name cluster_contact -setcookie dns -detached
    
Start the application (in console mode)

    | => _build/default/rel/dnsserver/bin/dnsserver console

I accept pull requests

