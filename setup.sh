git submodule init
git submodule update

export BASE=$(pwd)
export SRC=$(pwd)/src
export MD=$(pwd)/model-analyses/lisp-z3/examples/acl2s

cd $MD

ln -f -s $SRC/seq_raw_code.lsp  seq_raw_code.lsp
ln -f -s $SRC/seqsolve.lisp     seqsolve.lisp
ln -f -s $SRC/seqsolve.acl2     seqsolve.acl2
ln -f -s $SRC/higher-order.lisp higher-order.lisp
ln -f -s $SRC/metering.cl       metering.cl
ln -f -s $SRC/eval.lisp         eval.lisp  
ln -f -s $SRC/build.lisp        build.lisp
ln -f -s $SRC/cert.acl2         cert.acl2

rm ast-introspection.lisp
ln -f -s $SRC/ast-introspection.lisp ast-introspection.lisp

acl2s < $SRC/build.lisp
acl2s < $SRC/eval.lisp

cd $BASE
mkdir build
cd build
ln -f -s $MD/eval_tool_exec eval_tool_exec
ln -f -s $MD/seqsolve_exec seqsolve_exec

