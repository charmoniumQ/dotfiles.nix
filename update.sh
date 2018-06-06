#!/bin/sh
ansible-playbook ansible/main-playbook.yaml \
     --extra-vars "{\"dotfiles_root\": \"${PWD}\", \"become_works\": false}"
