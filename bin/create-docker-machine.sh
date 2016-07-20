#!/usr/bin/env bash

# Docker Machine Setup
docker-machine create \
        -d virtualbox \
            dnsdev

# Create the network with overlay driver
docker $(docker-machine config dnsdev) network create dns.dev

