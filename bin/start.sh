#!/usr/bin/env bash

export ERL_INETRC=config/erl_inetrc
export ERL_CRASH_DUMP=/var/log/dnsserver

_build/default/rel/dnsserver/bin/dnsserver console
