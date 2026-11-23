// Recipe page behavior. Inlined at the end of the page by recipe_book_md() in
// _recipes/recipes.R. Without JavaScript every collection simply shows.
(function () {
  // ---- Search + category chips (one set per collection) ----
  document.querySelectorAll(".rc-index").forEach(function (root) {
    var input = root.querySelector(".rc-search");
    var chips = root.querySelectorAll(".rc-chip");
    var tiles = root.querySelectorAll(".rc-tile");
    var groups = root.querySelectorAll(".rc-index-group");
    var empty = root.querySelector(".rc-empty");
    var active = "all";

    function apply() {
      var words = input.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
      var shown = 0;
      tiles.forEach(function (tile) {
        var text = tile.getAttribute("data-search");
        var match = (active === "all" || tile.getAttribute("data-category") === active) &&
          words.every(function (w) { return text.indexOf(w) !== -1; });
        tile.hidden = !match;
        if (match) shown++;
      });
      groups.forEach(function (group) {
        group.hidden = !group.querySelector(".rc-tile:not([hidden])");
      });
      empty.hidden = shown > 0;
    }

    chips.forEach(function (chip) {
      chip.addEventListener("click", function () {
        active = chip.getAttribute("data-category");
        chips.forEach(function (c) { c.setAttribute("aria-pressed", c === chip ? "true" : "false"); });
        apply();
      });
    });
    input.addEventListener("input", apply);
  });

  // ---- Collection toggle ----
  // Each collection is its index plus the category sections whose heading
  // carries data-collection. Mark them all with data-rc-collection.
  var buttons = document.querySelectorAll(".rc-collection[data-collection]");
  var parts = [];
  if (!buttons.length) return;

  document.querySelectorAll(".rc-cat[data-collection]").forEach(function (h) {
    var section = h.closest("section.level1") || h;
    section.setAttribute("data-rc-collection", h.getAttribute("data-collection"));
    if (parts.indexOf(section) === -1) parts.push(section);
  });
  document.querySelectorAll(".rc-index[data-collection]").forEach(function (nav) {
    var id = nav.getAttribute("data-collection");
    nav.setAttribute("data-rc-collection", id);
    // The index sits at the end of the previous collection's last section;
    // move it to just before its own first section
    var first = document.querySelector("section[data-rc-collection='" + id + "']");
    if (first && first.parentNode) first.parentNode.insertBefore(nav, first);
    parts.push(nav);
  });

  function collectionOf(el) {
    var owner = el && el.closest ? el.closest("[data-rc-collection]") : null;
    return owner ? owner.getAttribute("data-rc-collection") : null;
  }
  var current = null;

  // Hide sidebar entries that point into a hidden collection
  function syncToc() {
    document.querySelectorAll("#TOC a[href^='#']").forEach(function (a) {
      var id = decodeURIComponent(a.getAttribute("href").slice(1));
      var owner = collectionOf(document.getElementById(id));
      var li = a.closest("li");
      if (li && owner) li.style.display = owner === current ? "" : "none";
    });
  }

  function show(id) {
    current = id;
    parts.forEach(function (el) {
      el.classList.toggle("rc-hidden", el.getAttribute("data-rc-collection") !== id);
    });
    buttons.forEach(function (b) {
      b.setAttribute("aria-pressed", b.getAttribute("data-collection") === id ? "true" : "false");
    });
    syncToc();
  }
  function isCollection(id) {
    return Array.prototype.some.call(buttons, function (b) { return b.getAttribute("data-collection") === id; });
  }

  buttons.forEach(function (b) {
    b.addEventListener("click", function () {
      var id = b.getAttribute("data-collection");
      show(id);
      if (history.replaceState) history.replaceState(null, "", "#" + id);
    });
  });

  // A link to a recipe (or to a collection) opens the collection it lives in
  function fromHash(scroll) {
    var id = decodeURIComponent(location.hash.slice(1));
    if (id && isCollection(id)) { show(id); return true; }
    var target = id && document.getElementById(id);
    var owner = collectionOf(target);
    if (!owner) return false;
    show(owner);
    if (scroll) target.scrollIntoView();
    return true;
  }

  if (!fromHash(true)) show(buttons[0].getAttribute("data-collection"));
  window.addEventListener("hashchange", function () { fromHash(true); });
  // Quarto builds the sidebar after this script runs on some pages
  document.addEventListener("DOMContentLoaded", syncToc);
})();
