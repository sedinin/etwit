#!/usr/bin/env sh

rebar get-deps && rebar clean && rebar compile && rebar generate
