pacman::p_load(here, purrr)

input_folder <- here("data", "clean_data", "map", "2020")
output_file <- here("data", "clean_data", "map", "2020", "combined.txt")

# Create output directory
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

# Get all files and combine
txt_files <- list.files(input_folder, pattern = "_clean\\.txt$", full.names = TRUE)
txt_files <- sort(txt_files)

cat("Combining", length(txt_files), "files...\n")

all_content <- map_chr(txt_files, ~paste(readLines(.x, warn = FALSE), collapse = "\n"))
combined <- paste(all_content, collapse = "\n\n\n")

# Add header
header <- paste0(
  "COMBINED STATE LGBTQ POLICY DATA - ", length(txt_files), " States\n",
  "Created: ", Sys.time(), "\n",
  paste(rep("=", 80), collapse = ""), "\n\n\n"
)

writeLines(paste0(header, combined), output_file)

cat("✓ Combined file saved to:", output_file, "\n")
cat("✓ File size:", round(file.size(output_file) / 1024^2, 2), "MB\n")
