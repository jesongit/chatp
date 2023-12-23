#!/bin/bash

ebin="./_build/default/lib/*/ebin"
config_file="./config/sys.config"
node_name="chatp_1@127.0.0.1"
cookie="123456"

start() {
    erl -pz $ebin -config $config_file -name $node_name -setcookie $cookie \
    -hidden -boot start_sasl \
    -detached -run chatp_app
}

stop() {
    erl_call -c $cookie -name $node_name -a 'chatp_app stop'
}
reload() {
    erl_call -c $cookie -name $node_name -a 'reloader reload_all'
}

remsh() {
    erl -name remsh@127.0.0.1 -setcookie $cookie -remsh $node_name
}

robot() {
    echo "start robot $1"
}

case "$1" in
    "compile")
        rebar3 compile;;
    "reload")
        rebar3 compile;;
    "start")
        start;;
    "stop")
        stop;;
    "remsh")
        remsh;;
    "robot")
        robot $2;;
esac