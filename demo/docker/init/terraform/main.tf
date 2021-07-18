provider "aws" {
  region = "eu-west-1"

  endpoints {
    dynamodb = "http://localhost:4566"
    iam      = "http://localhost:4566"
    s3       = "http://localhost:4566"
    sns      = "http://localhost:4566"
    sqs      = "http://localhost:4566"
    sts      = "http://localhost:4566"
  }

  access_key = "test"
  secret_key = "test"

  skip_credentials_validation = true
  skip_metadata_api_check     = true
  skip_requesting_account_id  = true
  s3_force_path_style         = true
}

locals {
  namespace = "demo"
}

module "metadata_stores" {
  source = "github.com/wellcomecollection/storage-service.git//terraform/modules/critical/metadata_stores?ref=b24ea38"

  namespace = local.namespace

  vhs_bucket_name = "${local.namespace}-manifests"
  vhs_table_name  = "${local.namespace}-manifests"
}

output "ingests_table_name" {
  value = module.metadata_stores.ingests_table_name
}

module "working_storage" {
  source = "github.com/wellcomecollection/storage-service.git//terraform/modules/stack/working_storage?ref=b89f88d"

  namespace          = local.namespace
  bucket_name_prefix = "${local.namespace}-"

  azure_replicator_enabled = false
}

locals {
  queue_names = [
    "ingests_worker_input"
  ]

  topic_names = [
    "callback_notifications",
    "updated_ingests",
  ]
}

resource "aws_sqs_queue" "q" {
  for_each = toset(local.queue_names)

  name = each.key
}

output "queue_urls" {
  value = {
    for k, queue in aws_sqs_queue.q : k => queue.id
  }
}

resource "aws_sns_topic" "t" {
  for_each = toset(local.topic_names)

  name = each.key
}

output "topic_arns" {
  value = {
    for k, topic in aws_sns_topic.t : k => topic.arn
  }
}
