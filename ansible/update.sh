#!/bin/sh

if [ -n "${p}${packages}" ]
then
	# run as "p=1 ./update.sh" or "packages=1 ./update.sh" to install packages (time-consuming)
	extra_args="--extra-vars install_packages=True"
fi

if [ -n "${v}${verbose}" ]
then
	extra_args="${extra_args} -vvv"
fi

ansible-playbook ansible/main-playbook.yaml --ask-become-pass ${extra_args}
