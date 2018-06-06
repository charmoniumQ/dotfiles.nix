#!/bin/sh
# run as root

# informational variables
user=sam

# packages
if ! which sudo > /dev/null
then
	apt-get update
	apt-get install -y sudo ansible
fi

# sudo configuration
usermod -aG sudo "${user}"

# run main playbook
sudo -u sam ansible-playbook ansible/main-playbook.yaml \
     --user=`whoami` \
     --ask-become-pass \
     --extra-vars "{\"dotfiles_root\": \"${PWD}\", \"become_works\": true}"
