#!/bin/bash

rm -f /tmp/hansible*

test -d venv || ./testsetup.sh || exit 1

source venv/bin/activate || exit 1

stack run
