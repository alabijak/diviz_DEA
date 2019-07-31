#! /bin/bash

CMD="./HierarchicalDEAValueADDExtremeRanks.sh"

if [ $# != 1 ]; then
  echo "Usage: ${0} [--v2|--v3]" >&2
  exit 1
elif [ ${1} != "--v2" -a ${1} != "--v3" ]; then
  echo "Usage: ${0} [--v2|--v3]" >&2
  exit 1
fi

version=${1#*v}

NB_TESTS=$(find tests -maxdepth 1 -type d -regex '.*/in[0-9]*\.v'"${version}"'$' | wc -l)

mkdir -p tests_tmp
for i in $(seq 1 ${NB_TESTS}); do
    IN="tests/in${i}.v${version}"
    REFERENCE_OUT="tests/out${i}.v${version}"
    OUT=$(mktemp --tmpdir=. -d tests_tmp/out.XXX)
    echo "${IN}"
    ${CMD} "--v${version}" "${IN}" "${OUT}"
    diff -x README -ruBw "${REFERENCE_OUT}" "${OUT}"
    ret_diff=$?
    if [ $ret_diff -ne 0 ]; then
        echo "FAILED: ${IN}"
    else
        rm -r ${OUT}
    fi
done
