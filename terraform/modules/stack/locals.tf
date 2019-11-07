locals {
  bags_api_service_name    = "${var.namespace}-bags-api"
  ingests_service_name     = "${var.namespace}-ingests"
  ingests_api_service_name = "${var.namespace}-ingests-api"
  notifier_service_name    = "${var.namespace}-notifier"

  bag_unpacker_service_name           = "${var.namespace}-bag-unpacker"
  bag_root_finder_service_name        = "${var.namespace}-bag-root-finder"
  bag_versioner_service_name          = "${var.namespace}-bag-versioner"
  bag_replicator_service_name         = "${var.namespace}-bag-replicator"
  bag_register_service_name           = "${var.namespace}-bag-register"
  bag_verifier_post_repl_service_name = "${var.namespace}-bag-verifier-post-replication"
  bag_verifier_pre_repl_service_name  = "${var.namespace}-bag-verifier-pre-replication"
  replica_aggregator_service_name     = "${var.namespace}-replica_aggregator"

  bag_versioner_image      = module.images.services["bag_versioner"]
  bag_register_image       = module.images.services["bag_register"]
  bag_root_finder_image    = module.images.services["bag_root_finder"]
  bags_api_image           = module.images.services["bags_api"]
  ingests_image            = module.images.services["ingests"]
  ingests_api_image        = module.images.services["ingests_api"]
  notifier_image           = module.images.services["notifier"]
  bag_replicator_image     = module.images.services["bag_replicator"]
  bag_verifier_image       = module.images.services["bag_verifier"]
  bag_unpacker_image       = module.images.services["bag_unpacker"]
  replica_aggregator_image = module.images.services["replica_aggregator"]

  logstash_transit_service_name = "${var.namespace}_logstash_transit"
  logstash_transit_image        = "wellcome/logstash_transit:edgelord"
  logstash_host                 = "${local.logstash_transit_service_name}.${var.namespace}"
}

