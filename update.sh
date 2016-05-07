#!/usr/bin/env sh

ANSIBLE_KEEP_REMOTE_FILES=1 ansible-playbook -i "localhost," -c local ansible/update.yml --ask-sudo-pass
