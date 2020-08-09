#!/bin/sh
exec haproxy -W -db "$@"
