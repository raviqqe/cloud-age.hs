variable "project" {
  default = "navium-151723"
}

variable "num_instances" {
  default = 2
}

variable "token" {
  default = ""
}

provider "google" {
  project     = "${var.project}"
  region      = "us-central1"
  credentials = "${file("~/.google/${var.project}.json")}"
}

resource "google_compute_instance" "navium" {
  count = "${var.num_instances}"

  name         = "navis-${count.index}"
  machine_type = "f1-micro"
  zone         = "us-central1-a"

  disk {
    image = "ubuntu-os-cloud/ubuntu-1604-lts"
  }

  network_interface {
    subnetwork = "${google_compute_subnetwork.default.name}"
    address    = "10.0.0.${count.index + 2}"
    access_config {}
  }

  metadata {
    ssh-keys = "root:${file("~/.ssh/id_rsa.pub")}"
  }

  provisioner "remote-exec" {
    connection {
      type        = "ssh"
      user        = "root"
      private_key = "${file("~/.ssh/id_rsa")}"
      agent       = "false"
    }

    inline = [
      "curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -",
      "echo 'deb http://apt.kubernetes.io/ kubernetes-xenial main' >> /etc/apt/sources.list.d/kubernetes.list",
      "apt-get update",
      "apt-get install -y docker.io kubelet kubeadm kubectl kubernetes-cni",
      <<EOF
if [ ${count.index} -eq 0 ]
then
  kubeadm init --pod-network-cidr=10.244.0.0/16 --token '${var.token}' &&
  curl -sSL https://raw.github.com/coreos/flannel/master/Documentation/kube-flannel.yml |
  kubectl apply -f -
  kubectl get pods --all-namespaces
fi
EOF
    ]
  }
}

resource "google_compute_network" "default" {
  name = "mare"
}

resource "google_compute_subnetwork" "default" {
  name          = "caribbean"
  ip_cidr_range = "10.0.0.0/8"
  network       = "${google_compute_network.default.self_link}"
}

resource "google_compute_firewall" "default" {
  name = "fireball"
  network = "${google_compute_network.default.name}"

  allow {
    protocol = "icmp"
  }

  allow {
    protocol = "tcp"
    ports = [22, 80, 443]
  }
}
