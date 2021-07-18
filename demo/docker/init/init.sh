#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o verbose

apk add --update git terraform

pushd /docker-entrypoint-initaws.d/terraform
  terraform init
  terraform apply -auto-approve
popd
