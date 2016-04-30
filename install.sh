#!/usr/bin/env sh

ansible-playbook -i "localhost," -c local ansible/apply.yml --ask-sudo-pass
