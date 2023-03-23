for compiler in $(find $(echo "$PATH" | tr ':' '\n') -name 'ghc-*' | sort | uniq | grep --extended-regexp '/ghc-[[:digit:]]+.[[:digit:]]+$')
do
    cabal run --with-compiler "$compiler" checks
done
