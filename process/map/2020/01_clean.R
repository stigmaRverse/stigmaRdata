pacman::p_load(here, rvest, xml2, purrr, dplyr, stringr)

# AGGRESSIVE cleaning function - extracts only pure data (with better error handling)
clean_single_html_aggressive <- function(input_path, output_folder) {
  
  tryCatch({
    html <- read_html(input_path)
    
    # Extract state name
    state_name <- html %>%
      html_node("h1") %>%
      html_text(trim = TRUE) %>%
      str_replace("'s Equality Profile", "")
    
    cat("Processing:", basename(input_path), "-", state_name, "\n")
    
    # Initialize clean output
    output <- paste0(
      "========================================\n",
      "STATE: ", state_name, "\n",
      "========================================\n\n"
    )
    
    # 1. EXTRACT QUICK FACTS
    output <- paste0(output, "QUICK FACTS\n", strrep("-", 40), "\n")
    
    quick_facts <- html %>%
      html_nodes(".quickfactbox") %>%
      map_chr(function(box) {
        label <- box %>% html_node("p") %>% html_text(trim = TRUE)
        value <- box %>% html_node("span") %>% html_text(trim = TRUE)
        paste0(label, " ", value)
      }) %>%
      paste(collapse = "\n")
    
    output <- paste0(output, quick_facts, "\n\n")
    
    # 2. EXTRACT POLICY TALLY
    output <- paste0(output, "POLICY TALLY\n", strrep("-", 40), "\n")
    
    so_tally <- html %>%
      html_node(".so-box .tally") %>%
      html_text(trim = TRUE)
    
    gi_tally <- html %>%
      html_node(".gi-box .tally") %>%
      html_text(trim = TRUE)
    
    overall_tally <- html %>%
      html_node(".overalltally .tally") %>%
      html_text(trim = TRUE)
    
    output <- paste0(output,
                     "Sexual Orientation Policy Tally: ", so_tally, "\n",
                     "Gender Identity Policy Tally: ", gi_tally, "\n",
                     "Overall Tally: ", overall_tally, "\n\n"
    )
    
    # 3. EXTRACT POLICY TABLES
    output <- paste0(output, "LGBTQ LAWS AND POLICIES\n", strrep("=", 40), "\n\n")
    
    policy_tables <- html %>%
      html_nodes(".policy-table")
    
    for (table_section in policy_tables) {
      
      # Get section title
      section_title <- table_section %>%
        html_node(".title-cell") %>%
        html_text(trim = TRUE)
      
      if (is.na(section_title) || length(section_title) == 0) next
      
      output <- paste0(output, toupper(section_title), "\n", strrep("-", 40), "\n")
      
      # Extract table data with error handling
      table_node <- table_section %>% html_node("table")
      
      if (length(table_node) == 0 || inherits(table_node, "xml_missing")) {
        output <- paste0(output, "  [No table data available]\n\n")
        next
      }
      
      table <- html_table(table_node, fill = TRUE)
      
      # Clean and format table
      if (!is.null(table) && nrow(table) > 2) {
        # Skip header rows and process data rows
        for (i in 3:nrow(table)) {
          row <- table[i, ]
          
          # Skip if it's a subtotal or total row
          if (grepl("Subtotal|Total", row[1, 1], ignore.case = TRUE)) {
            output <- paste0(output, "\n", row[1, 1], "\n")
            if (ncol(row) >= 3) {
              output <- paste0(output, "  ", paste(row[1, 2:ncol(row)], collapse = " | "), "\n")
            }
          } else {
            # Regular policy row
            policy_name <- row[1, 1]
            # Remove HTML artifacts and clean
            policy_name <- gsub("\\s+", " ", policy_name)
            policy_name <- str_trim(policy_name)
            
            output <- paste0(output, "  • ", policy_name, "\n")
            
            # Add SO and GI status if available
            if (ncol(row) >= 5) {
              so_status <- paste(row[1, 2:3], collapse = " ")
              gi_status <- paste(row[1, 4:5], collapse = " ")
              output <- paste0(output, 
                               "    SO: ", so_status, " | GI: ", gi_status, "\n")
            }
          }
        }
      }
      
      output <- paste0(output, "\n")
    }
    
    # 4. EXTRACT LOCAL ORDINANCES
    output <- paste0(output, "LOCAL NONDISCRIMINATION ORDINANCES\n", strrep("=", 40), "\n\n")
    
    # Sexual Orientation coverage
    so_boxes <- html %>%
      html_nodes(".box")
    
    if (length(so_boxes) > 0) {
      # Check if it has the "no-city-county" class (means no local protections)
      has_no_local <- html %>%
        html_nodes(".box.no-city-county") %>%
        length() > 0
      
      if (has_no_local) {
        output <- paste0(output, "No local nondiscrimination ordinances\n\n")
      } else {
        so_coverage <- so_boxes[[1]] %>%
          html_nodes(".round") %>%
          map_chr(~html_text(.x, trim = TRUE)) %>%
          paste(collapse = " | ")
        
        output <- paste0(output, "Sexual Orientation Coverage: ", so_coverage, "\n")
        
        # Gender Identity coverage
        if (length(so_boxes) > 1) {
          gi_coverage <- so_boxes[[2]] %>%
            html_nodes(".round") %>%
            map_chr(~html_text(.x, trim = TRUE)) %>%
            paste(collapse = " | ")
          
          output <- paste0(output, "Gender Identity Coverage: ", gi_coverage, "\n\n")
        }
        
        # City/County listing - with better error handling
        city_table_node <- html %>% html_node(".table-condensed")
        
        if (!is.null(city_table_node) && !inherits(city_table_node, "xml_missing")) {
          city_table <- html_table(city_table_node, fill = TRUE)
          
          if (!is.null(city_table) && nrow(city_table) > 2) {
            output <- paste0(output, "Cities/Counties with Protections:\n")
            for (i in 3:(nrow(city_table)-2)) {  # Skip header and footer rows
              if (i > nrow(city_table)) break  # Safety check
              city_name <- city_table[i, 1]
              if (!is.na(city_name) && nchar(city_name) > 0) {
                output <- paste0(output, "  • ", city_name, "\n")
              }
            }
          }
        }
      }
    } else {
      output <- paste0(output, "No local ordinance data available\n\n")
    }
    
    # Create output filename
    base_name <- tools::file_path_sans_ext(basename(input_path))
    output_path <- file.path(output_folder, paste0(base_name, "_clean.txt"))
    
    # Save clean text
    writeLines(output, output_path)
    
    cat("  → Saved to:", basename(output_path), "\n")
    
    return(list(
      input = basename(input_path),
      output = basename(output_path),
      state = state_name,
      success = TRUE
    ))
    
  }, error = function(e) {
    warning("Error processing ", basename(input_path), ": ", e$message)
    return(list(
      input = basename(input_path),
      output = NA,
      state = NA,
      success = FALSE,
      error = e$message
    ))
  })
}

# Main processing function (unchanged)
process_all_html_files <- function(input_folder, output_folder) {
  
  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    cat("Created output folder:", output_folder, "\n\n")
  }
  
  # Get all HTML file names
  html_files <- list.files(path = input_folder, 
                           pattern = "\\.html$", 
                           full.names = TRUE)
  
  cat("Found", length(html_files), "HTML files\n")
  cat("Starting aggressive cleaning...\n\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  # Process each file
  results <- map(html_files, ~clean_single_html_aggressive(.x, output_folder))
  
  # Summary
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("\nPROCESSING COMPLETE\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  successful <- sum(sapply(results, function(x) x$success))
  failed <- length(results) - successful
  
  cat("Total files processed:", length(results), "\n")
  cat("Successful:", successful, "\n")
  cat("Failed:", failed, "\n\n")
  
  # Show any errors
  if (failed > 0) {
    cat("Failed files:\n")
    failed_results <- results[!sapply(results, function(x) x$success)]
    for (fail in failed_results) {
      cat("  -", fail$input, ":", fail$error, "\n")
    }
  }
  
  # Calculate file size reduction
  original_size <- sum(file.info(html_files)$size)
  
  output_files <- list.files(output_folder, pattern = "\\.txt$", full.names = TRUE)
  new_size <- sum(file.info(output_files)$size)
  
  cat("\nFile Size Reduction:\n")
  cat("Original:", round(original_size / 1024^2, 2), "MB\n")
  cat("Cleaned:", round(new_size / 1024^2, 2), "MB\n")
  cat("Reduction:", round((1 - new_size/original_size) * 100, 1), "%\n\n")
  
  # Return summary data frame
  results_df <- map_df(results, as.data.frame)
  return(results_df)
}

# ============================================
# RUN THE PROCESSING
# ============================================

input_folder <- here("data", "raw_data", "map", "1_raw", "2020")
output_folder <- here("data", "clean_data", "map", "2020")

# Process all files with aggressive cleaning
summary <- process_all_html_files(input_folder, output_folder)

# View summary
print(summary)
