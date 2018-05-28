#!/bin/sh
ansible-playbook ansible/main-playbook.yaml \
     --user=`whoami` \
     --extra-vars "{\"dotfiles_root\": \"${PWD}\", \"install_packages\": false}"
