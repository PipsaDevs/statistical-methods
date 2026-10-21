function getStoredTheme() {
  try { return localStorage.getItem("statmethods-theme"); } catch (e) { return null; }
}

function setStoredTheme(theme) {
  try { localStorage.setItem("statmethods-theme", theme); } catch (e) {}
}

function updateThemeUI(theme) {
  document.documentElement.setAttribute("data-theme", theme);
  var label = document.getElementById("themeToggleLabel");
  var btn = document.getElementById("themeToggle");
  if (label) label.textContent = (theme === "dark") ? "Light" : "Dark";
  if (btn) {
    var nextMode = (theme === "dark") ? "light" : "dark";
    btn.setAttribute("aria-label", "Switch to " + nextMode + " mode");
  }
}

function toggleTheme() {
  var current = document.documentElement.getAttribute("data-theme") || "light";
  var next = (current === "dark") ? "light" : "dark";
  setStoredTheme(next);
  updateThemeUI(next);
}

function filterList() {
  var input = document.getElementById("filterInput");
  var clearBtn = document.getElementById("clearSearch");
  var q = input.value.trim().toLowerCase();

  if (clearBtn) clearBtn.style.display = q ? "block" : "none";

  var totalVisible = 0;
  document.querySelectorAll(".file-section").forEach(function(sec) {
    var fileName = (sec.getAttribute("data-file") || "").toLowerCase();
    var secMatches = 0;

    sec.querySelectorAll(".fn-card").forEach(function(card) {
      var fnName = (card.getAttribute("data-fn") || "").toLowerCase();
      var cardText = card.textContent.toLowerCase();
      var match = !q || fnName.indexOf(q) !== -1 || fileName.indexOf(q) !== -1 || cardText.indexOf(q) !== -1;

      card.style.display = match ? "" : "none";
      if (match) { secMatches++; totalVisible++; }
    });
    sec.style.display = (secMatches > 0) ? "" : "none";
  });

  document.querySelectorAll(".nav-file-section").forEach(function(group) {
    var fileName = (group.getAttribute("data-file") || "").toLowerCase();
    var groupMatches = 0;

    group.querySelectorAll(".nav-sublist li").forEach(function(li) {
      var fnName = (li.getAttribute("data-fn") || li.textContent).toLowerCase();
      var match = !q || fnName.indexOf(q) !== -1 || fileName.indexOf(q) !== -1;
      li.style.display = match ? "" : "none";
      if (match) groupMatches++;
    });
    group.style.display = (groupMatches > 0 || (q && fileName.indexOf(q) !== -1)) ? "" : "none";
  });

  var noRes = document.getElementById("noResults");
  if (noRes) noRes.style.display = (totalVisible === 0 && q !== "") ? "block" : "none";
}

function clearSearch() {
  var input = document.getElementById("filterInput");
  if (input) {
    input.value = "";
    filterList();
    input.focus();
  }
}

document.addEventListener("DOMContentLoaded", function() {
  var activeTheme = document.documentElement.getAttribute("data-theme") || "light";
  updateThemeUI(activeTheme);

  var btn = document.getElementById("themeToggle");
  if (btn) btn.addEventListener("click", toggleTheme);

  document.addEventListener("keydown", function(e) {
    if ((e.key === "/" || (e.ctrlKey && e.key.toLowerCase() === "k")) && document.activeElement !== document.getElementById("filterInput")) {
      e.preventDefault();
      var fi = document.getElementById("filterInput");
      if (fi) { fi.focus(); fi.select(); }
    } else if (e.key === "Escape" && document.activeElement === document.getElementById("filterInput")) {
      clearSearch();
    }
  });
});
