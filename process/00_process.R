# 00_process.R
# Master run script — execute this to fully rebuild all release datasets.
# Run from the stigmaRdata root directory (where stigmaRdata.Rproj lives).
#
# Order:
#   1. Source-specific scripts clean raw data → .Rds in clean_data/
#   2. 01_master.R joins all sources → saves composite.rda + items.rda to release_data/
#
# After running, publish release_data/ files as a GitHub Release on
# https://github.com/follhim/stigmaRdata so stigmaR can pull them downstream.

pacman::p_load(here)

# ── Step 1: Source-specific cleaning scripts ──────────────────────────────────
source(here("process/iat/02_iat_sexuality.R"))   # ~2-3 min
# source(here("process/iat/03_iat_race.R"))      # placeholder
# source(here("process/map/map_master.R"))        # placeholder

# ── Step 2: Join all sources → release_data/ ─────────────────────────────────
source(here("process/01_master.R"))

message("Full pipeline complete. Check release_data/ and publish a GitHub Release.")
