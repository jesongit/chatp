#!/bin/bash

config_file_str=$(cat ./config/sys.config)
server_id=${config_file_str##*server_id, }
server_id=${server_id%%\}*}

ebin="./_build/default/lib/*/ebin"
config_file="./config/sys.config"
node_name="chatp_${server_id}@127.0.0.1"
cookie="123456"

start() {
    erl -pz $ebin -config $config_file -name $node_name -setcookie $cookie \
    -hidden -boot start_sasl -detached -run lager -run chatp
}

debug() {
    erl -pz $ebin -config $config_file -name $node_name -setcookie $cookie \
    -hidden -boot start_sasl -run lager -run chatp
}

stop() {
    erl_call -c $cookie -name $node_name -a 'init stop'
}
reload() {
    erl_call -c $cookie -name $node_name -a 'reloader reload_all'
}

remsh() {
    erl -name remsh@127.0.0.1 -setcookie $cookie -remsh $node_name
}

robot() {
    echo "start robot $1"
    robot_name="robot_$1@127.0.0.1"
    erl -pz $ebin -config $config_file -name $robot_name -setcookie $cookie \
    -remsh $node_name
}

case "$1" in
    "compile")
        rebar3 compile;;
    "reload")
        rebar3 compile;;
    "debug")
        debug;;
    "start")
        start;;
    "stop")
        stop;;
    "remsh")
        remsh;;
    "robot")
        robot $2;;
    *)
        echo $1;;
esac