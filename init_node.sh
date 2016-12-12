#!/bin/sh

global_master_ip=$1
local_master_ip=$2
token=$3
ip=$4

curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - &&
echo 'deb http://apt.kubernetes.io/ kubernetes-xenial main' \
     >> /etc/apt/sources.list.d/kubernetes.list &&

apt-get update --fix-missing &&
apt-get install -y docker.io kubelet kubeadm kubectl kubernetes-cni &&

if [ $ip = $local_master_ip ]
then
  # TODO: --api-advertise-addresses=$global_master_ip,$local_master_ip
  kubeadm init --api-advertise-addresses=$global_master_ip --pod-network-cidr=10.244.0.0/16 --token "$token" &&
  curl -sSL https://raw.github.com/coreos/flannel/master/Documentation/kube-flannel.yml |
  kubectl apply -f - &&
  kubectl get pods --all-namespaces
else
  kubeadm join --token "$token" $global_master_ip # TODO: use $local_master_ip
fi
