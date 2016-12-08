#!/bin/sh

openssl rand -base64 22 | sed 's/^\(.\{6\}\)\(.*\)$/\1.\2/'
