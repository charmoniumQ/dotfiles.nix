#!/usr/bin/env sh

ansible-playbook -i "localhost," -c local ansible/install.yml --ask-sudo-pass
