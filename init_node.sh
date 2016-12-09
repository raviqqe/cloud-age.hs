#!/bin/sh

master_ip=$1
token=$2
ip=$3

curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - &&
echo 'deb http://apt.kubernetes.io/ kubernetes-xenial main' \
     >> /etc/apt/sources.list.d/kubernetes.list &&

apt-get update &&
apt-get install -y docker.io kubelet kubeadm kubectl kubernetes-cni &&

if [ $ip = $master_ip ]
then
  kubeadm init --pod-network-cidr=10.244.0.0/16 --token "$token" &&
  curl -sSL https://raw.github.com/coreos/flannel/master/Documentation/kube-flannel.yml |
  kubectl apply -f - &&
  kubectl get pods --all-namespaces
else
  kubeadm join --token "$token" $master_ip
fi
