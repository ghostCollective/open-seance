# Extract the '## How to Play' section from README.md and replace the same section in rules.qmd
library(readr)
library(stringr)
library(fs)

# README to extract from
readme_path <- "README.md"

# Pages to push to
index_path <- "index.qmd"
rules_path <- "rules.qmd"

# Helper files to embed in pages
extracted_description_path <- "_extracted_description.md"
extracted_acknowledgements_path <- "_extracted_acknowledgements.md"
extracted_rules_path <- "_extracted_rules.md"

# Verbatim heading titles of the sections to extract
title_description <- "Goal of the Game"
title_rules <- "How to Play"
title_acknowledgements <- "Acknowledgements"

stop_if_missing <- function(p) {
  if (!fs::file_exists(p)) stop(sprintf("File not found: %s", p), call. = FALSE)
}

stop_if_missing(readme_path)
stop_if_missing(index_path)
stop_if_missing(rules_path)

# Read files
readme <- readr::read_lines(readme_path)
index  <- readr::read_lines(index_path)
rules  <- readr::read_lines(rules_path)

# Helper to slice a section that starts with an h2 "## How to Play" and ends before the next h2 or EOF
slice_h2_section <- function(lines, h2_title_regex) {
  # Find the start line of the exact H2
  h2_start <- which(stringr::str_detect(lines, sprintf("^##\\s*%s\\s*$", h2_title_regex)))
  if (length(h2_start) == 0) stop("Section header not found.", call. = FALSE)
  h2_start <- h2_start[[1]]
  # Next H2 after start (if any)
  next_h2 <- which(seq_along(lines) > h2_start & stringr::str_detect(lines, "^##\\s+"))
  h2_end  <- if (length(next_h2)) min(next_h2) - 1L else length(lines)
  # Return the exact block (including the H2 line)
  block <- lines[h2_start:h2_end]
  
  # 2) Drop the header itself and write extracted_rules.qmd
  if (length(block) == 0 || !stringr::str_detect(block[1], "^##\\s+")) {
    stop("First line of extracted block is not an H2 header.", call. = FALSE)
  }
  
  content_wo_header <- block[-1]
  
  # Trim leading blank lines if any
  while (length(content_wo_header) > 0 && stringr::str_trim(content_wo_header[1]) == "") {
    content_wo_header <- content_wo_header[-1]
  }
  
  return(content_wo_header)
}

description_text <- slice_h2_section(readme, title_description)
readr::write_lines(description_text, extracted_description_path)
cat(sprintf("Wrote %d lines to %s\n", length(description_text), extracted_description_path))

acknowledgements_text <- slice_h2_section(readme, title_acknowledgements)
readr::write_lines(acknowledgements_text, extracted_acknowledgements_path)
cat(sprintf("Wrote %d lines to %s\n", length(acknowledgements_text), extracted_acknowledgements_path))

rules_text <- slice_h2_section(readme, title_rules)
readr::write_lines(rules_text, extracted_rules_path)
cat(sprintf("Wrote %d lines to %s\n", length(rules_text), extracted_rules_path))

