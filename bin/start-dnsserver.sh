#!/usr/bin/env bash

# this script assumes your prompt is in the root of the project
# so it is called with bin/start-dnsserver.sh

export ERL_INETRC=`pwd`/config/erl_inetrc
export ERL_CRASH_DUMP=/var/log/dnsserver

_build/default/rel/dnsserver/bin/dnsserver console
