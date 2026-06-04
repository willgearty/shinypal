// Custom sidebar resize handle for shinypal.
//
// Replaces bslib's built-in resize handle.
// Sidebars opt in via the `.shinypal-resizable` class.
// Companion styles live in styles.css.
(function () {
  function initOne(sidebar) {
    var layout = sidebar.parentElement;
    if (!layout || layout.dataset.shinypalResizeInit) return;
    layout.dataset.shinypalResizeInit = "true";

    var isRight = layout.classList.contains("sidebar-right");

    // build the grab strip and its visible indicator
    var handle = document.createElement("div");
    handle.className = "shinypal-resize-handle";
    handle.setAttribute("role", "separator");
    handle.setAttribute("aria-orientation", "vertical");
    handle.setAttribute("aria-label", "Resize sidebar");
    handle.tabIndex = 0;
    var indicator = document.createElement("div");
    indicator.className = "shinypal-resize-indicator";
    handle.appendChild(indicator);
    layout.appendChild(handle);

    var MIN = 150;
    function maxW() { return window.innerWidth - 50; }
    function clamp(w) { return Math.max(MIN, Math.min(maxW(), w)); }
    function currentWidth() {
      return sidebar.getBoundingClientRect().width || 250;
    }
    function setWidth(w) {
      w = clamp(w);
      layout.style.setProperty("--_sidebar-width", w + "px");
      handle.setAttribute("aria-valuenow", Math.round(w));
      handle.setAttribute("aria-valuemin", MIN);
      handle.setAttribute("aria-valuemax", Math.round(maxW()));
    }

    var resizing = false, startX = 0, startWidth = 0;

    handle.addEventListener("pointerdown", function (e) {
      if (e.button && e.button !== 0) return;
      resizing = true;
      startX = e.clientX;
      startWidth = currentWidth();
      try { handle.setPointerCapture(e.pointerId); } catch (err) {}
      layout.classList.add("shinypal-resizing");
      layout.style.setProperty("--_transition-duration", "0ms");
      document.documentElement.style.cursor = "ew-resize";
      e.preventDefault();
    });

    handle.addEventListener("pointermove", function (e) {
      if (!resizing) return;
      var dx = e.clientX - startX;
      setWidth(isRight ? startWidth - dx : startWidth + dx);
      e.preventDefault();
    });

    function end(e) {
      if (!resizing) return;
      resizing = false;
      try { handle.releasePointerCapture(e.pointerId); } catch (err) {}
      layout.classList.remove("shinypal-resizing");
      layout.style.removeProperty("--_transition-duration");
      document.documentElement.style.cursor = "";
      // nudge Shiny outputs (e.g. plots) to reflow at the new width
      window.dispatchEvent(new Event("resize"));
    }
    handle.addEventListener("pointerup", end);
    handle.addEventListener("pointercancel", end);

    // keyboard accessibility: arrows nudge, Home/End jump to min/max
    handle.addEventListener("keydown", function (e) {
      var step = e.shiftKey ? 50 : 10;
      var w = currentWidth(), nw = w;
      if (e.key === "ArrowLeft") nw = isRight ? w + step : w - step;
      else if (e.key === "ArrowRight") nw = isRight ? w - step : w + step;
      else if (e.key === "Home") nw = MIN;
      else if (e.key === "End") nw = maxW();
      else return;
      e.preventDefault();
      setWidth(nw);
      window.dispatchEvent(new Event("resize"));
    });
  }

  function initAll() {
    document
      .querySelectorAll(".bslib-sidebar-layout > .sidebar.shinypal-resizable")
      .forEach(initOne);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initAll);
  } else {
    initAll();
  }
})();
