variable "project" {
  default = "navium-151723"
}

variable "num_instances" {
  default = 2
}

variable "token" {
  default = ""
}

variable "first_ip" {
  default = 2
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
    address    = "10.0.0.${var.first_ip + count.index}"
    access_config {}
  }

  metadata {
    ssh-keys = "root:${file("~/.ssh/id_rsa.pub")}"
  }

  provisioner "file" {
    connection {
      private_key = "${file("~/.ssh/id_rsa")}"
      agent       = "false"
    }

    source = "init_node.sh"
    destination = "/tmp/init_node.sh"
  }

  provisioner "remote-exec" {
    connection {
      private_key = "${file("~/.ssh/id_rsa")}"
      agent       = "false"
    }

    inline = [
      "sh /tmp/init_node.sh 10.0.0.${var.first_ip} '${var.token}' ${self.network_interface.0.address}",
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
    ports = ["0-65535"]
  }

  allow {
    protocol = "udp"
    ports = ["0-65535"]
  }
}
