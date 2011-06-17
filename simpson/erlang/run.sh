#!/bin/bash
erlc ./simpsonrule.erl
erl -sname master -run simpsonrule start 2, 10000000, 0.0, 1.1 -noinput
