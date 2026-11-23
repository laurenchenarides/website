// Recipe index search and category filter. Inlined into the page by
// recipe_index_html() in _recipes/recipes.R.
(function () {
  var root = document.getElementById("recipe-index");
  if (!root) return;
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
})();
