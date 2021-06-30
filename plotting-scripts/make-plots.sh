#!/usr/bin/env bash

TD="$(pwd)"
PD="$(dirname "$TD")"
BD="$(dirname "$PD")"

while [[ "$#" -gt 0 ]]; do
    case $1 in
        -t|--time) time="$2"; shift;;
        -n|--num) num="$2"; shift;;
	-b|--bmark) bmark="$2"; shift;;
	-d|--dir) dir="$2"; shift;;
        --help|*) echo "Usage : ./make-plots.sh -b (benchmark folder) -t (realtime|totalruntime|usertime|systime) -n (number of runs per tool) -d (output directory)"; exit 1 ;;
    esac
    shift
done

mkdir $dir
cd $dir

bname=$(basename "$bmark")
rm $(echo "$bname-avg-*")

$PD/eval_tool_exec "seqsolve" $bmark $num $time
$PD/eval_tool_exec "cvc4" $bmark $num $time
$PD/eval_tool_exec "z3str3" $bmark $num $time
$PD/eval_tool_exec "z3str2" $bmark $num $time
$PD/eval_tool_exec "norn" $bmark $num $time
$PD/eval_tool_exec "seqsolve-srv" $bmark $num $time

python3 newplot.py
