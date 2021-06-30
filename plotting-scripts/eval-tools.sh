#!/usr/bin/env bash

TD="$(pwd)"
PD="$(dirname "$TD")"
BD="$(dirname "$PD")"

while [[ "$#" -gt 0 ]]; do
    case $1 in
        -n|--num) num="$2"; shift;;
	-b|--bmark) bmark="$2"; shift;;
	-d|--dir) dir="$2"; shift;;
        --help|*) echo "Usage : ./eval-tools.sh -b (benchmark folder) -n (number of runs per tool) -d (output directory)"; exit 1 ;;
    esac
    shift
done

mkdir $dir
cd $dir

#../eval_tool_exec seqsolve ~/dev/monoid-solver/evaluation/stringfuzz-gen 3 realtime

#$PD/eval_tool_exec "seqsolve" $bmark $num 
#$PD/eval_tool_exec "cvc4" $bmark $num 
#$PD/eval_tool_exec "z3str3" $bmark $num 
#$PD/eval_tool_exec "z3str2" $bmark $num 
#$PD/eval_tool_exec "norn" $bmark $num
#$PD/eval_seqsolve_servm_exec $bmark $num
#$PD/eval_tool_exec "sloth" $bmark $num
$PD/eval_tool_exec "trau" $bmark $num

