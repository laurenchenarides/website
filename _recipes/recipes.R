# Helpers for the recipe collections. Sourced by the web pages (recipes.qmd,
# yiayia.qmd) and by _recipes/build_cards.R (the downloadable PDF cards), so
# each page and its cards are built from the same YAML file in _recipes/.

library(yaml)
library(htmltools)

# The collections, in the order their links appear at the top of each page
recipe_collections <- list(
  list(id = "mine",   title = "My Recipe Box",    href = "recipes.html", data = "_recipes/recipes.yml"),
  list(id = "yiayia", title = "Yiayia's Kitchen", href = "yiayia.html",  data = "_recipes/yiayia.yml")
)

load_recipe_box <- function(path = "_recipes/recipes.yml") {
  box <- yaml::read_yaml(path)
  card_dir <- if (is.null(box$card_dir)) file.path("content", "recipes") else box$card_dir
  box$recipes <- lapply(box$recipes, function(r) { r$card_dir <- card_dir; r })
  box
}

card_path <- function(recipe) {
  file.path(recipe$card_dir, paste0(recipe$id, ".pdf"))
}

# Recipes are tested unless marked `tested: false`
is_untested <- function(recipe) isFALSE(recipe$tested)

untested_badge <- '<span class="rc-tag rc-untested">Not yet tested</span>'

strip_md <- function(x) {
  x <- gsub("\\[([^]]+)\\]\\([^)]+\\)", "\\1", x)
  gsub("\\*", "", x)
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
  # A part may hold one table or a list of tables
  if (is.null(tbl$title)) return(paste(vapply(tbl, table_html, ""), collapse = ""))
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

# Everything a reader might search for: title, blurb, tags, credit, ingredients
tile_search_text <- function(r) {
  ingredients <- unlist(lapply(r$parts, function(p) lapply(p$ingredients, function(g) g$items)))
  text <- c(r$title, r$blurb, unlist(r$tags), r$credit, ingredients)
  htmlEscape(tolower(strip_md(paste(text, collapse = " "))), attribute = TRUE)
}

collection_nav_html <- function(current) {
  links <- vapply(recipe_collections, function(cl) paste0(
    '<a class="rc-collection', if (cl$id == current) ' is-current" aria-current="page' else "",
    '" href="', cl$href, '">', htmlEscape(cl$title), '</a>'
  ), character(1))
  paste0('<div class="rc-collections" aria-label="Recipe collections">', paste(links, collapse = ""), '</div>')
}

recipe_index_html <- function(box) {
  cats <- Filter(function(cat) any(vapply(box$recipes, function(r) r$category == cat$id, logical(1))), box$categories)
  chips <- paste0(
    '<button type="button" class="rc-chip" data-category="all" aria-pressed="true">All</button>',
    paste0(vapply(cats, function(cat) paste0(
      '<button type="button" class="rc-chip" data-category="', cat$id, '" aria-pressed="false">',
      md_inline(cat$title), '</button>'
    ), character(1)), collapse = "")
  )
  sections <- vapply(cats, function(cat) {
    recipes <- Filter(function(r) r$category == cat$id, box$recipes)
    tiles <- vapply(recipes, function(r) paste0(
      '<a class="rc-tile" href="#', r$id, '" data-category="', cat$id,
      '" data-search="', tile_search_text(r), '">',
      '<span class="rc-tile-title">', md_inline(r$title), '</span>',
      '<span class="rc-tile-blurb">', md_inline(r$blurb), '</span>',
      '<span class="rc-tags">',
      if (is_untested(r)) untested_badge else "",
      paste0('<span class="rc-tag">', vapply(r$tags, md_inline, ""), '</span>', collapse = ""),
      '</span></a>'
    ), character(1))
    paste0(
      '<div class="rc-index-group" data-category="', cat$id, '"><h3 class="rc-index-heading">', md_inline(cat$title),
      '</h3><div class="rc-tiles">', paste(tiles, collapse = ""), '</div></div>'
    )
  }, character(1))
  script <- paste(readLines("_recipes/recipes.js", encoding = "UTF-8"), collapse = "\n")
  paste0(
    '<nav class="rc-index" id="recipe-index" aria-label="Recipe index">',
    collection_nav_html(box$collection),
    '<div class="rc-toolbar">',
    '<label class="visually-hidden" for="rc-search">Search recipes</label>',
    '<input type="search" id="rc-search" class="rc-search" placeholder="Search recipes or ingredients…" autocomplete="off">',
    '<div class="rc-chips" role="group" aria-label="Filter by category">', chips, '</div>',
    '</div>',
    paste(sections, collapse = ""),
    '<p class="rc-empty" hidden>No recipes match. Try another word or category.</p>',
    '</nav>',
    '<script>', script, '</script>'
  )
}

recipe_header_html <- function(r) {
  original <- if (!is.null(r$source_url)) paste0(
    '<a class="btn btn-outline-primary btn-sm" href="', htmlEscape(r$source_url),
    '"><i class="bi bi-box-arrow-up-right"></i> Original recipe</a>'
  ) else ""
  paste0(
    '<p class="rc-subtitle">', md_inline(r$subtitle), '</p>',
    if (is_untested(r)) paste0('<p class="rc-untested-note">', untested_badge,
      ' I haven\'t tested this one yet, so amounts and times may need adjusting.</p>') else "",
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
    first_page <- i == 1
    heading <- if (first_page) r$title else p$title
    # Page 1 carries the recipe's subtitle; the first part's own subtitle goes below the header
    subtitle <- if (first_page || is.null(p$subtitle)) r$subtitle else p$subtitle
    part_sub <- if (first_page && length(r$parts) > 1 && !is.null(p$subtitle)) {
      paste0('<p class="rc-part-subtitle">', md_inline(p$subtitle), '</p>')
    } else ""
    paste0(
      '<section class="card-page">',
      '<header class="card-head"><div><h1>', md_inline(heading), '</h1>',
      '<p class="card-sub">', md_inline(subtitle), '</p></div>',
      if (is_untested(r)) '<span class="card-badge card-untested">Not yet tested</span>'
      else if (!is.null(p$badge)) paste0('<span class="card-badge">', md_inline(p$badge), '</span>')
      else if (!is.null(r$tags)) paste0('<span class="card-badge">', md_inline(r$tags[[1]]), '</span>') else "",
      '</header>',
      # Page 1 always carries the recipe credit; later variant pages with their
      # own credit show only that (via part_html)
      if (first_page || is.null(p$credit)) paste0(
        '<p class="card-credit">', md_inline(r$credit),
        if (!is.null(r$source_url)) paste0('<br><span class="card-url">', htmlEscape(r$source_url), '</span>') else "",
        '</p>'
      ) else "",
      part_sub,
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
