#!/bin/bash

VDM_BIN=`stack exec -- which vdm-ty`

pkill -9 vdm-ty

# template, target-dir
create_and_start_bot () {
    echo "creating bot"
    echo "template=$1, target=$2"

    cp -r "$1" "$2"

    pushd "$2"
    $VDM_BIN arena >log 2>err &
    popd
}

create_and_start_bot template-simple simple-1
create_and_start_bot template-simple simple-2
create_and_start_bot template-qr qr-1
create_and_start_bot template-qr qr-2
