// Keep shinypal's report blocks in workflow order.
//
// When the workflow is reordered, the server sends the step ids in their new
// order; moving the existing nodes keeps their Shiny bindings and rendered
// outputs instead of re-rendering the whole report.
(function () {
  if (!window.Shiny) return;
  Shiny.addCustomMessageHandler("shinypal-reorder-report", function (ids) {
    var box = document.getElementById("shinypal_report_steps");
    if (!box) return;
    ids.forEach(function (id) {
      var el = document.getElementById("report_" + id);
      if (el) box.appendChild(el);
    });
  });
})();
