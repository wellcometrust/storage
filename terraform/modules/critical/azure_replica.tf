data "azurerm_resource_group" "rg" {
  name = var.azure_resource_group_name
}

resource "azurerm_storage_account" "wellcome" {
  resource_group_name = data.azurerm_resource_group.rg.name

  name         = var.azure_storage_account_name
  account_kind = "StorageV2"
  account_tier = "Standard"

  # Our primary and Glacier replicas are in Amazon's eu-west-1 region, which
  # is in Dublin.  We want our Azure replica to have geographical separation
  # from our other replicas -- the westeurope region is in Amsterdam.
  location = "westeurope"

  # We only use Locally Redundant storage for the Azure replica because
  # this is a backup of last resort.
  account_replication_type = "LRS"

  # New blobs are written to the Cool tier by default, rather than Hot.
  # We expect most blobs will only be accessed once -- when the verifier
  # checks a blob was written correctly.  After 30 days, blobs will be
  # cycled out of Cool into the Archive tier.
  access_tier = "Cool"
}

resource "azurerm_storage_container" "container" {
  name                  = "wellcomecollection-${var.namespace}-replica-netherlands"
  storage_account_name  = azurerm_storage_account.wellcome.name
  container_access_type = "private"

  lifecycle {
    prevent_destroy = true
  }
}

resource "azurerm_storage_management_policy" "tier_to_archive" {
  storage_account_id = azurerm_storage_account.wellcome.id

  rule {
    name    = "TierToArchive"
    enabled = true

    filters {
      blob_types = ["blockBlob"]
    }

    actions {
      base_blob {
        tier_to_archive_after_days_since_modification_greater_than = 30
      }
    }
  }
}
