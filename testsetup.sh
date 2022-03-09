#!/bin/bash

# Install Ansible and hansible specific complementary
# packages

PYTHON='/usr/bin/env python3'

REPO_GLUE='ssh://git@giteaprod-01.my.cum.re/ffpp/hansible-glue.git#egg=hansible-glue'
REPO_GALAXY='git@giteaprod-01.my.cum.re:ffpp/hansible-modules.git'

$PYTHON -m virtualenv venv

source venv/bin/activate || exit 1

pip install ansible || exit 1
pip install -e "git+$REPO_GLUE" || exit 1

ansible-galaxy collection install --force "git+$REPO_GALAXY" || exit 1

printf "\e[0;31m"
cat << EOF

============================
============================

test VENV ready.
activate with 'source venv/bin/activate'
EOF
printf "\e[39m"
