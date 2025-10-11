# Extract the '## How to Play' section from README.md and replace the same section in rules.qmd
library(readr)
library(stringr)
library(fs)

readme_path <- "README.md"
rules_path  <- "rules.qmd"
extracted_path <- "extracted_rules.qmd"

stop_if_missing <- function(p) {
  if (!file_exists(p)) stop(sprintf("File not found: %s", p), call. = FALSE)
}

stop_if_missing(readme_path)
stop_if_missing(rules_path)

# Read files
readme <- read_lines(readme_path)
rules  <- read_lines(rules_path)

# Helper to slice a section that starts with an h2 "## How to Play" and ends before the next h2 or EOF
slice_h2_section <- function(lines, h2_title_regex) {
  # Find the start line of the exact H2
  h2_start <- which(str_detect(lines, sprintf("^##\\s*%s\\s*$", h2_title_regex)))
  if (length(h2_start) == 0) stop("Section header not found.", call. = FALSE)
  h2_start <- h2_start[[1]]
  # Next H2 after start (if any)
  next_h2 <- which(seq_along(lines) > h2_start & str_detect(lines, "^##\\s+"))
  h2_end  <- if (length(next_h2)) min(next_h2) - 1L else length(lines)
  # Return the exact block (including the H2 line)
  lines[h2_start:h2_end]
}

title_rx <- "How to Play"

# 1) Extract rules section from README
block <- slice_h2_section(readme, title_rx)

# 2) Drop the header itself and write extracted_rules.qmd
if (length(block) == 0 || !str_detect(block[1], "^##\\s+")) {
  stop("First line of extracted block is not an H2 header.", call. = FALSE)
}

content_wo_header <- block[-1]

# Trim leading blank lines if any
while (length(content_wo_header) > 0 && str_trim(content_wo_header[1]) == "") {
  content_wo_header <- content_wo_header[-1]
}

write_lines(content_wo_header, extracted_path)
cat(sprintf("Wrote %d lines to %s\n", length(content_wo_header), extracted_path))
