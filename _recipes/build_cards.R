# Build the downloadable PDF recipe cards from _recipes/recipes.yml.
# Run from the project root:  Rscript _recipes/build_cards.R
# Writes content/recipes/<id>.pdf using headless Microsoft Edge (or Chrome).

source("_recipes/recipes.R")

browser <- Filter(file.exists, c(
  Sys.getenv("RECIPE_CARD_BROWSER"),
  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
  "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
))[1]
if (is.na(browser)) stop("No Edge/Chrome found; set RECIPE_CARD_BROWSER to its path.")

box <- load_recipe_box()
css <- paste(readLines("_recipes/card.css", encoding = "UTF-8"), collapse = "\n")
out_dir <- file.path("content", "recipes")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
work_dir <- tempfile("recipe-cards-")
dir.create(work_dir)

for (r in box$recipes) {
  html_file <- file.path(work_dir, paste0(r$id, ".html"))
  writeLines(enc2utf8(card_document_html(r, css)), html_file, useBytes = TRUE)
  pdf_file <- normalizePath(card_path(r), mustWork = FALSE)
  unlink(pdf_file)
  system2(browser, c(
    "--headless=new", "--disable-gpu", "--no-pdf-header-footer",
    paste0('--user-data-dir="', file.path(work_dir, "profile"), '"'),
    paste0('--print-to-pdf="', pdf_file, '"'),
    paste0('"file:///', normalizePath(html_file, winslash = "/"), '"')
  ), stdout = FALSE, stderr = FALSE)
  message(if (file.exists(pdf_file)) "Built " else "FAILED ", card_path(r))
}

unlink(work_dir, recursive = TRUE)
