. base.sh

run . codeworld-compiler codeworld-base/dist/doc/html/codeworld-base/codeworld-base.txt \
                         -o test.base.js \
                         -e test.base.err \
                         -m codeworld \
                         -s test.base.syms \
                         --gen-base \
                         --ignore-in-base fromCWText \
                         --ignore-in-base toCWText \
                         --ignore-in-base randomsFrom

run . codeworld-compiler data/codeworld/user/PhF/PhFFj32Bx0FcpQvvoVJW0xw.hs \
                         -o test.js \
                         -e test.err \
                         -m codeworld \
                         -s test.base.syms
