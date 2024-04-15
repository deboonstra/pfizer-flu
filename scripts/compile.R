# The function of this script is to compile a .Rmd files.

# Load package ####
#nolint: devtools::install_github("deboonstra/boonstra")
library(boonstra)

# Creating docs sub-directory ####
if (!dir.exists("./docs")) {
  dir.create("./docs")
}

# Rendering .Rmd files ####
boonstra::render_all(
  files = list.files(
    path = "./outputs",
    pattern = "*.Rmd",
    recursive = TRUE,
    full.names = TRUE
  )
)
boonstra::render_all(
  files = list.files(
    path = "./outputs",
    pattern = "*.Rmd",
    recursive = TRUE,
    full.names = TRUE
  ),
  output_dir = "./docs"
)