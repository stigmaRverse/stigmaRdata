# 02_iat_sexuality.R
# Purpose: Raw IAT sexuality files (2016–2025) → item means → composite indices
# Outputs:
#   data/clean/iat_sexuality_items.Rds    (state × year, all iat_sex_ items + iat_sex_n_ counts)
#   data/clean/iat_sexuality_indices.Rds  (state × year, composite scores only — no n_ columns)
#
# NOTE: 2015 is excluded — it uses a different variable set (no adoptchild,
#   serverights, transgender, marriagerights_3num, relationslegal_3num).
#
# NOTE: iat_sex_explicit_bel (belief that sexuality is environmental) is not
#   included — the source variable (sexualityorigin) does not appear in any
#   Project Implicit sexuality IAT public dataset file from 2016 onward.

pacman::p_load(tidyverse, here, readr, haven)

folder_path <- here("raw_data/iat/sexuality")

# ── Valid US states + DC only ─────────────────────────────────────────────────
us_states_dc <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY"
)

# ── Read all raw files (exclude 2015) ────────────────────────────────────────
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
sav_files <- list.files(folder_path, pattern = "\\.sav$", full.names = TRUE, recursive = TRUE)

# Drop 2015 — variable structure is incompatible with 2016+
csv_files <- csv_files[!grepl("2015", csv_files)]

all_files <- c(
  set_names(map(csv_files, read_csv, show_col_types = FALSE),
            tools::file_path_sans_ext(basename(csv_files))),
  set_names(map(sav_files, read_sav),
            tools::file_path_sans_ext(basename(sav_files)))
)

# ── Helper: safely grab a column or return NAs ───────────────────────────────
grab <- function(df, col) {
  if (col %in% names(df)) df[[col]] else rep(NA_real_, nrow(df))
}

# ── Clean one year-file to state-year item means ─────────────────────────────
clean_one_year <- function(df) {
  # Normalize all column names to lowercase to handle casing differences
  # across years (.csv vs .sav) and Project Implicit naming conventions
  names(df) <- tolower(names(df))

  df |>
    filter(!is.na(state)) |>
    mutate(
      # Implicit -------------------------------------------------------------------
      # D-score: higher = stronger pro-straight (anti-gay) implicit bias
      iat_sex_imp_d         = grab(pick(everything()), "d_biep.straight_good_all"),

      # Explicit: Attitude ---------------------------------------------------------
      # 7-point scale; higher = more positive toward straight (more stigma)
      iat_sex_exp_att       = grab(pick(everything()), "att_7"),

      # Explicit: Thermometers (reversed: higher = more stigma) ------------------
      # tgayleswomen used 2017+; tgaywomen present in 2016 as fallback
      iat_sex_exp_therm_gm  = 10 - grab(pick(everything()), "tgaymen"),
      iat_sex_exp_therm_gw  = 10 - coalesce(
        grab(pick(everything()), "tgayleswomen"),
        grab(pick(everything()), "tgaywomen")
      ),

      # Explicit: Policy (0 = low stigma, 1 = high stigma) ----------------------
      iat_sex_exp_pol_marr  = case_when(
        grab(pick(everything()), "marriagerights_3num") == 1 ~ 0,   # support
        grab(pick(everything()), "marriagerights_3num") == 2 ~ 1,   # oppose
        grab(pick(everything()), "marriagerights_3num") == 3 ~ NA_real_  # no opinion
      ),

      iat_sex_exp_pol_legal = case_when(
        grab(pick(everything()), "relationslegal_3num") == 1 ~ 0,
        grab(pick(everything()), "relationslegal_3num") == 2 ~ 1,
        grab(pick(everything()), "relationslegal_3num") == 3 ~ NA_real_
      ),

      iat_sex_exp_pol_adopt = case_when(
        grab(pick(everything()), "adoptchild") == 1 ~ 0,
        grab(pick(everything()), "adoptchild") == 2 ~ 1,
        grab(pick(everything()), "adoptchild") == 3 ~ NA_real_
      ),

      iat_sex_exp_pol_serv  = case_when(
        grab(pick(everything()), "serverights") == 1 ~ 1,   # support refusal
        grab(pick(everything()), "serverights") == 2 ~ 0,
        grab(pick(everything()), "serverights") == 3 ~ NA_real_
      ),

      iat_sex_exp_pol_trans = case_when(
        grab(pick(everything()), "transgender") == 1 ~ 1,   # oppose trans bathroom
        grab(pick(everything()), "transgender") == 2 ~ 0
      )
    ) |>
    group_by(year, state) |>
    summarise(
      # Counts FIRST — named iat_sex_n_{item} to follow AAA_BBB_CCC convention
      across(
        c(iat_sex_imp_d,
          iat_sex_exp_att,
          iat_sex_exp_therm_gm, iat_sex_exp_therm_gw,
          iat_sex_exp_pol_marr, iat_sex_exp_pol_legal,
          iat_sex_exp_pol_adopt, iat_sex_exp_pol_serv,
          iat_sex_exp_pol_trans),
        ~ sum(!is.na(.x)),
        .names = "iat_sex_n_{sub('iat_sex_', '', .col)}"
      ),
      # Means SECOND — safe to overwrite column names
      across(
        c(iat_sex_imp_d,
          iat_sex_exp_att,
          iat_sex_exp_therm_gm, iat_sex_exp_therm_gw,
          iat_sex_exp_pol_marr, iat_sex_exp_pol_legal,
          iat_sex_exp_pol_adopt, iat_sex_exp_pol_serv,
          iat_sex_exp_pol_trans),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}"
      ),
      .groups = "drop"
    ) |>
    # Strip <labelled> class from .sav imports, normalize, keep 50 states + DC
    mutate(state = toupper(trimws(as.character(haven::zap_labels(state))))) |>
    filter(state %in% us_states_dc)
}

# ── Build item-level dataset ──────────────────────────────────────────────────
# Contains individual item means AND their iat_sex_n_ counts
iat_sexuality_items <- all_files |>
  map_dfr(clean_one_year)

# ── Build composite-level dataset ─────────────────────────────────────────────
# Composite scores only — no respondent counts (those live in items)
iat_sexuality_indices <- iat_sexuality_items |>
  mutate(
    # Implicit -------------------------------------------------------------------
    iat_sex_implicit       = iat_sex_imp_d,

    # Explicit: Thermometer ------------------------------------------------------
    iat_sex_explicit_therm = rowMeans(
      pick(iat_sex_exp_therm_gm, iat_sex_exp_therm_gw),
      na.rm = TRUE
    ),

    # Explicit: Policy -----------------------------------------------------------
    iat_sex_explicit_pol   = rowMeans(
      pick(iat_sex_exp_pol_marr, iat_sex_exp_pol_legal,
           iat_sex_exp_pol_adopt, iat_sex_exp_pol_serv,
           iat_sex_exp_pol_trans),
      na.rm = TRUE
    ),

    # Explicit: Omnibus (therm + pol) -------------------------------------------
    iat_sex_explicit       = rowMeans(
      pick(iat_sex_exp_therm_gm,  iat_sex_exp_therm_gw,
           iat_sex_exp_pol_marr,  iat_sex_exp_pol_legal,
           iat_sex_exp_pol_adopt, iat_sex_exp_pol_serv,
           iat_sex_exp_pol_trans),
      na.rm = TRUE
    )
  ) |>
  select(
    state, year,
    iat_sex_implicit,
    iat_sex_explicit_therm,
    iat_sex_explicit_pol,
    iat_sex_explicit
  )

# ── Save ──────────────────────────────────────────────────────────────────────
saveRDS(iat_sexuality_items,   here("clean_data/iat_sexuality_items.Rds"))
saveRDS(iat_sexuality_indices, here("clean_data/iat_sexuality_indices.Rds"))

message("02_iat_sexuality.R complete: ",
        nrow(iat_sexuality_items), " state-year rows written (2016–2025).")
