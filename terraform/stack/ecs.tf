resource "aws_ecs_cluster" "cluster" {
  name = "${var.namespace}"
}

resource "aws_service_discovery_private_dns_namespace" "namespace" {
  name = "${var.namespace}"
  vpc  = "${var.vpc_id}"
}

module "cluster_hosts" {
  source = "git::https://github.com/wellcometrust/terraform.git//ecs/modules/ec2/prebuilt/nvm?ref=v17.1.0"

  image_id = "ami-0de29b072b458b107"

  vpc_id   = "${var.vpc_id}"
  key_name = "${var.ssh_key_name}"
  asg_name = "${var.namespace}"

  subnets = "${var.private_subnets}"

  cluster_name = "${aws_ecs_cluster.cluster.name}"

  asg_min     = "1"
  asg_desired = "2"
  asg_max     = "2"

  controlled_access_cidr_ingress = ["${var.vpc_cidr}"]
}

module "bastion" {
  source = "git::https://github.com/wellcometrust/terraform.git//ec2/prebuilt/bastion?ref=v17.1.0"

  vpc_id = "${var.vpc_id}"

  name = "${var.namespace}-bastion"

  controlled_access_cidr_ingress = "${var.controlled_access_cidr_ingress}"

  key_name    = "${var.ssh_key_name}"
  subnet_list = "${var.public_subnets}"
}
