# Helpers for the recipe box. Sourced by recipes.qmd (the web page) and
# _recipes/build_cards.R (the downloadable PDF cards) so both are built from
# the same data in _recipes/recipes.yml.

library(yaml)
library(htmltools)

load_recipe_box <- function(path = "_recipes/recipes.yml") {
  yaml::read_yaml(path)
}

card_path <- function(recipe) {
  file.path("content", "recipes", paste0(recipe$id, ".pdf"))
}

# Minimal inline markdown: **bold**, *italic*, [text](url)
md_inline <- function(x) {
  if (is.null(x)) return("")
  x <- htmltools::htmlEscape(x)
  x <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", '<a href="\\2">\\1</a>', x)
  x <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", x)
  x <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", x)
  x
}

meta_html <- function(meta) {
  if (is.null(meta)) return("")
  cells <- vapply(meta, function(m) paste0(
    '<div class="rc-meta-item"><span class="rc-meta-label">', md_inline(m[[1]]),
    '</span><span class="rc-meta-value">', md_inline(m[[2]]), '</span></div>'
  ), character(1))
  paste0('<div class="rc-meta">', paste(cells, collapse = ""), '</div>')
}

callout_html <- function(callout) {
  if (is.null(callout)) return("")
  paste0(
    '<div class="rc-callout"><strong>', md_inline(callout$label), ':</strong> ',
    md_inline(callout$text), '</div>'
  )
}

table_html <- function(tbl) {
  if (is.null(tbl)) return("")
  head <- paste0("<th>", vapply(tbl$header, md_inline, ""), "</th>", collapse = "")
  rows <- vapply(tbl$rows, function(r) paste0(
    "<tr>", paste0("<td>", vapply(r, md_inline, ""), "</td>", collapse = ""), "</tr>"
  ), character(1))
  paste0(
    '<h4 class="rc-subhead">', md_inline(tbl$title), '</h4>',
    '<div class="rc-table-wrap"><table class="rc-table"><thead><tr>', head,
    '</tr></thead><tbody>', paste(rows, collapse = ""), '</tbody></table></div>'
  )
}

ingredients_html <- function(groups) {
  if (is.null(groups)) return("")
  body <- vapply(groups, function(g) paste0(
    if (!is.null(g$heading)) paste0('<h5>', md_inline(g$heading), '</h5>') else "",
    '<ul>', paste0("<li>", vapply(g$items, md_inline, ""), "</li>", collapse = ""), '</ul>'
  ), character(1))
  paste0(
    '<div class="rc-ingredients"><h4>Ingredients</h4>',
    paste(body, collapse = ""), '</div>'
  )
}

# Steps are numbered continuously across headed groups
steps_html <- function(groups, title = "Instructions") {
  if (is.null(groups)) return("")
  n <- 1
  body <- character(0)
  for (g in groups) {
    body <- c(body, paste0(
      if (!is.null(g$heading)) paste0('<h5>', md_inline(g$heading), '</h5>') else "",
      '<ol start="', n, '" style="counter-reset: rc-step ', n - 1, '">',
      paste0("<li>", vapply(g$items, md_inline, ""), "</li>", collapse = ""),
      '</ol>'
    ))
    n <- n + length(g$items)
  }
  paste0(
    '<div class="rc-steps">', if (nzchar(title)) paste0('<h4>', title, '</h4>') else "",
    paste(body, collapse = ""), '</div>'
  )
}

notes_html <- function(notes) {
  if (is.null(notes)) return("")
  items <- vapply(notes, function(nt) paste0(
    '<div class="rc-note"><span class="rc-note-label">', md_inline(nt$label),
    '</span><p>', md_inline(nt$text), '</p></div>'
  ), character(1))
  paste(items, collapse = "")
}

glossary_html <- function(gl) {
  if (is.null(gl)) return("")
  terms <- vapply(gl$terms, function(t) paste0(
    "<dt>", md_inline(t[[1]]), "</dt><dd>", md_inline(t[[2]]), "</dd>"
  ), character(1))
  paste0(
    '<div class="rc-note rc-glossary"><span class="rc-note-label">',
    md_inline(gl$title), '</span><dl>', paste(terms, collapse = ""), '</dl></div>'
  )
}

# The body of one recipe part, shared by the page and the PDF card
part_html <- function(part) {
  has_ingredients <- !is.null(part$ingredients)
  paste0(
    if (!is.null(part$credit)) paste0('<p class="rc-part-credit">', md_inline(part$credit), '</p>') else "",
    meta_html(part$meta),
    callout_html(part$callout),
    table_html(part$table),
    '<div class="rc-body', if (has_ingredients) "" else " rc-body-single", '">',
    ingredients_html(part$ingredients),
    steps_html(part$steps, title = if (has_ingredients) "Instructions" else ""),
    '</div>',
    notes_html(part$notes),
    glossary_html(part$glossary)
  )
}

# ---- Web page -----------------------------------------------------------------

raw_html <- function(x) paste0("\n```{=html}\n", x, "\n```\n\n")

recipe_index_html <- function(box) {
  sections <- vapply(box$categories, function(cat) {
    recipes <- Filter(function(r) r$category == cat$id, box$recipes)
    tiles <- vapply(recipes, function(r) paste0(
      '<a class="rc-tile" href="#', r$id, '">',
      '<span class="rc-tile-title">', md_inline(r$title), '</span>',
      '<span class="rc-tile-blurb">', md_inline(r$blurb), '</span>',
      '<span class="rc-tags">',
      paste0('<span class="rc-tag">', vapply(r$tags, md_inline, ""), '</span>', collapse = ""),
      '</span></a>'
    ), character(1))
    paste0(
      '<div class="rc-index-group"><h3 class="rc-index-heading">', md_inline(cat$title),
      '</h3><div class="rc-tiles">', paste(tiles, collapse = ""), '</div></div>'
    )
  }, character(1))
  paste0('<nav class="rc-index" id="recipe-index" aria-label="Recipe index">', paste(sections, collapse = ""), '</nav>')
}

recipe_header_html <- function(r) {
  original <- if (!is.null(r$source_url)) paste0(
    '<a class="btn btn-outline-primary btn-sm" href="', htmlEscape(r$source_url),
    '"><i class="bi bi-box-arrow-up-right"></i> Original recipe</a>'
  ) else ""
  paste0(
    '<p class="rc-subtitle">', md_inline(r$subtitle), '</p>',
    '<p class="rc-credit">', md_inline(r$credit), '</p>',
    '<div class="rc-actions">',
    '<a class="btn btn-primary btn-sm" href="', card_path(r), '" download>',
    '<i class="bi bi-file-earmark-arrow-down"></i> Download recipe card (PDF)</a>',
    original,
    '<a class="rc-top" href="#recipe-index">↑ All recipes</a>',
    '</div>'
  )
}

# Markdown + raw HTML for one recipe. Headings stay in markdown so they show
# up in the table of contents; multi-part recipes become a tabset.
recipe_section_md <- function(r) {
  out <- paste0("## ", r$title, " {#", r$id, " .rc-title}\n", raw_html(recipe_header_html(r)))
  if (length(r$parts) == 1) {
    out <- paste0(out, raw_html(paste0('<div class="rc-recipe">', part_html(r$parts[[1]]), '</div>')))
  } else {
    out <- paste0(out, "::: {.panel-tabset}\n\n")
    for (p in r$parts) {
      out <- paste0(
        out, "### ", p$title, "\n",
        raw_html(paste0(
          '<div class="rc-recipe">',
          if (!is.null(p$subtitle)) paste0('<p class="rc-part-subtitle">', md_inline(p$subtitle), '</p>') else "",
          part_html(p), '</div>'
        ))
      )
    }
    out <- paste0(out, ":::\n\n")
  }
  out
}

recipe_page_md <- function(box) {
  out <- raw_html(recipe_index_html(box))
  for (cat in box$categories) {
    out <- paste0(out, "# ", cat$title, "\n\n")
    for (r in Filter(function(r) r$category == cat$id, box$recipes)) {
      out <- paste0(out, recipe_section_md(r))
    }
  }
  out
}

# ---- PDF card ------------------------------------------------------------------

card_document_html <- function(r, css) {
  pages <- vapply(seq_along(r$parts), function(i) {
    p <- r$parts[[i]]
    heading <- if (length(r$parts) > 1 && i > 1) p$title else r$title
    subtitle <- if (!is.null(p$subtitle)) p$subtitle else r$subtitle
    paste0(
      '<section class="card-page">',
      '<header class="card-head"><div><h1>', md_inline(heading), '</h1>',
      '<p class="card-sub">', md_inline(subtitle), '</p></div>',
      if (!is.null(r$tags)) paste0('<span class="card-badge">', md_inline(r$tags[[1]]), '</span>') else "",
      '</header>',
      '<p class="card-credit">', md_inline(r$credit),
      if (!is.null(r$source_url)) paste0('<br><span class="card-url">', htmlEscape(r$source_url), '</span>') else "",
      '</p>',
      part_html(p),
      '</section>'
    )
  }, character(1))
  paste0(
    '<!doctype html><html><head><meta charset="utf-8"><title>', htmlEscape(r$title),
    '</title><style>', css, '</style></head><body>',
    paste(pages, collapse = ""),
    '</body></html>'
  )
}
