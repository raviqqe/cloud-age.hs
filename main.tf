provider "google" {
  project = "navium-151723"
  region = "us-central1"
  credentials = "${file("~/.google/navium.json")}"
}

resource "google_compute_instance" "navium" {
  count = 2

  name         = "navis-${count.index}"
  machine_type = "f1-micro"
  zone         = "us-central1-a"

  disk {
    image = "ubuntu-os-cloud/ubuntu-1604-lts"
  }

  network_interface {
    network = "default"
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
    }

    inline = [
      "sudo apt -y install docker",
    ]
  }
}
