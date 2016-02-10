#!/usr/bin/env bash

# this script assumes your prompt is in the root of the project
# so it is called with bin/start.sh

export ERL_INETRC=`pwd`/config/erl_inetrc
export ERL_CRASH_DUMP=/var/log/dnsserver

`pwd`_build/default/rel/dnsserver/bin/dnsserver console
