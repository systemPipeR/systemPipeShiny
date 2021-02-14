#!/bin/bash
set -e

man_folder="./man"
hash_file=".github/reference_hash/man_hash.txt"

rm -rf /tmp/man_update
mkdir -p .github/reference_hash

# check hash
if [[ -f "${hash_file}" ]]; then
    echo "${hash_file} exists, continue"
    current_hash=$(cat ${hash_file})
    new_hash=`find ${man_folder} -type f -print0 | \
        sort -z | \
        xargs -0 md5sum | \
        md5sum | \
        awk '{ print $1 }'`
    echo "compare hash"
    echo "Current hash: ${current_hash}"
    echo "New hash:     ${new_hash}"
    if [[ ! "${current_hash}" == "${new_hash}" ]]; then
        echo "hash differs, update needed"
        touch ./differ
    else
        echo "skip pkgdown"
    fi
else
    echo "${hash_file} is not there, update needed"
    touch ./differ
fi

