#!/usr/bin/env sh

ansible-playbook -i "localhost," -c local ansible/update.yml --ask-sudo-pass
