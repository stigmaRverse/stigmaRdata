# 01_master.R
# Purpose: Join all cleaned source datasets → produce release datasets
# Outputs:
#   release_data/composite.rda  → consumed by stigmaR package via GitHub Release
#   release_data/items.rda      → consumed by stigmaR package via GitHub Release
#
# After running, publish these files as assets on a GitHub Release at:
#   https://github.com/follhim/stigmaRdata/releases

pacman::p_load(tidyverse, here)

# ── Load cleaned source datasets ─────────────────────────────────────────────
iat_sex_items   <- readRDS(here("data/clean_data/iat_sexuality_items.Rds"))
iat_sex_indices <- readRDS(here("data/clean_data/iat_sexuality_indices.Rds"))
# iat_black_items   <- readRDS(here("clean_data/iat_race_items.Rds"))
# iat_black_indices <- readRDS(here("clean_data/iat_race_indices.Rds"))
# map_items         <- readRDS(here("clean_data/map_items.Rds"))
# map_indices       <- readRDS(here("clean_data/map_indices.Rds"))

# ── Merge all item-level sources ──────────────────────────────────────────────
items <- iat_sex_items
# |> full_join(iat_black_items, by = c("state", "year"))
# |> full_join(map_items,       by = c("state", "year"))

# ── Merge all composite-level sources ────────────────────────────────────────
composite <- iat_sex_indices
# |> full_join(iat_black_indices, by = c("state", "year"))
# |> full_join(map_indices,       by = c("state", "year"))

# ── Validate ──────────────────────────────────────────────────────────────────
stopifnot(
  "state must be two-letter code" =
    all(nchar(items$state) == 2, na.rm = TRUE),
  "items and composite must have same state-year rows" =
    nrow(items) == nrow(composite)
)

# ── Save to release_data/ ─────────────────────────────────────────────────────
dir.create(here("data/release_data"), showWarnings = FALSE, recursive = TRUE)
save(composite, file = here("data/release_data/composite.rda"))
save(items,     file = here("data/release_data/items.rda"))

message("01_master.R complete.",
        "\n  composite : ", nrow(composite), " rows, ", ncol(composite), " cols",
        "\n  items     : ", nrow(items),     " rows, ", ncol(items),     " cols",
        "\n\nNext: publish release_data/ as a GitHub Release on follhim/stigmaRdata.")
