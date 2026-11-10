(function ($) {
  "use strict";
  if (window.racafeActionMenus) return;
  window.racafeActionMenus = true;
  function closeMenus() {
    document.querySelectorAll(".rc-panel-acciones").forEach(function (panel) { panel.style.display = "none"; });
  }
  $(document).on("click.rmActions", "[data-rm-panel]", function (event) {
    event.stopPropagation();
    var id = this.dataset.rmPanel, local = this.parentNode.querySelector(".rc-panel-acciones");
    var previous = document.getElementById(id);
    if (local && previous && local !== previous) previous.remove();
    var panel = local || previous;
    if (!panel) return;
    var opened = panel.style.display === "flex";
    closeMenus();
    if (opened) return;
    panel._racafeOwner = this;
    if (panel.parentNode !== document.body) document.body.appendChild(panel);
    var rect = this.getBoundingClientRect();
    panel.style.display = "flex";
    var box = panel.getBoundingClientRect(), gap = 4, edge = 8;
    panel.style.left = Math.max(edge, Math.min(rect.left, window.innerWidth - box.width - edge)) + "px";
    panel.style.top = (rect.bottom + gap + box.height < window.innerHeight ?
      rect.bottom + gap : Math.max(edge, rect.top - box.height - gap)) + "px";
  });
  $(document).on("click.rmActions", "[data-rm-action]", function (event) {
    event.stopPropagation();
    var panel = this.closest(".rc-panel-acciones");
    if (window.Shiny) Shiny.setInputValue(this.dataset.rmInput,
      {codigo: this.dataset.rmCode, accion: this.dataset.rmAction}, {priority: "event"});
    if (panel) panel.style.display = "none";
  });
  $(document).on("click.rmActions", "[data-kanban-action]", function (event) {
    event.stopPropagation();
    var board = this.closest("[data-kanban-ns]");
    if (board && window.Shiny) Shiny.setInputValue(board.dataset.kanbanNs + "AccionSeleccionada",
      {codigo: $(this).data("cod-contacto"), accion: $(this).data("kanban-action"), ts: Date.now()}, {priority: "event"});
  });
  $(document).on("click.rmActions", function (event) {
    if (!event.target.closest(".rc-panel-acciones,.rc-boton-acciones")) closeMenus();
  }).on("hide.bs.modal.rmActions shiny:disconnected.rmActions", closeMenus);
  $(document).on("shiny:value.rmActions", function () {
    document.querySelectorAll(".rc-panel-acciones").forEach(function (panel) {
      if (panel._racafeOwner && !panel._racafeOwner.isConnected) panel.remove();
    });
  });
  window.addEventListener("resize", closeMenus);
  window.addEventListener("scroll", closeMenus, true);
}(window.jQuery));
