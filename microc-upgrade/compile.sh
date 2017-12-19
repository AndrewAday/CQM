LLI="lli"
LLC="llc"
CC="cc"
MICROC="./microc.native"

Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Compile() {
  basename=`echo $1 | sed 's/.*\\///
                           s/.mc//'`
  echo ${basename}
  Run "cat" "lib/*.mc" "$1" "|" "$MICROC" ">" "${basename}.ll" &&
  Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
  Run "$CC" "-o" "${basename}.exe" "${basename}.s" "printbig.o" "eigen_mnist.o" "-Llib/src" "-leigentest" "-lm"
}

Compile $1
