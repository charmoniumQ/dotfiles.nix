#!/bin/sh
set -e

# run as root
if [ "$(whoami)" != "root" ]
then
	echo "run as root"
	exit 1
fi

# informational variables
user=sam

# packages
apt-get update
apt-get install -y sudo ansible

# sudo configuration
usermod -aG sudo "${user}"
