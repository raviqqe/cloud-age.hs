#!/bin/sh

global_master_ip=$1
local_master_ip=$2
ip=$3

if [ $local_master_ip = $ip ]
then
  mkdir -p etc &&
  scp -i $HOME/.ssh/id_rsa -o StrictHostKeyChecking=No root@$global_master_ip:/etc/kubernetes/admin.conf etc/admin.conf
fi
