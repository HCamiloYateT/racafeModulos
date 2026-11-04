// NAVBAR CORPORATIVO ----
(function ($, Shiny) {
  "use strict";
  if (!Shiny || window.racafeMenuHeader) return;
  window.racafeMenuHeader = true;
  const restore = new Set();
  function close(root) { $(root).find('> .rmh-trigger').dropdown('hide'); }
  document.addEventListener('rsh:list-select', function (event) {
    const root = event.target.closest('.rmh-menu');
    if (!root) return;
    if (event.detail.id !== root.dataset.rmhSelect) {
      Shiny.setInputValue(root.dataset.rmhSelect, event.detail.value, {priority:'event'});
    }
    if (root.dataset.rmhKeep === 'false') close(root);
  });
  // Captura solo el clic de selección: el listener rsh de document continúa recibiéndolo.
  document.addEventListener('click', function (event) {
    if (!(event.target instanceof Element)) return;
    const root = event.target.closest('.rmh-menu'), item = event.target.closest('.rsh-list-item');
    if (!root) return;
    const trigger = event.target.closest('.rmh-trigger');
    if (trigger && $(trigger).data('bs.tooltip')) $(trigger).tooltip('hide');
    if (root.dataset.rmhDirect === 'true' && event.target.closest('.rmh-trigger')) {
      event.preventDefault(); event.stopPropagation();
      Shiny.setInputValue(root.dataset.rmhOpen, Date.now(), {priority:'event'});
      return;
    }
    const keep = root.dataset.rmhKeep !== 'false';
    if (keep && event.target.closest('.rmh-panel') && root.classList.contains('show')) {
      root.dataset.rmhSelecting = 'true';
      setTimeout(function () { delete root.dataset.rmhSelecting; }, 0);
    }
    const footer = event.target.closest('[data-rmh-footer]');
    if (footer) { event.preventDefault(); Shiny.setInputValue(footer.dataset.rmhFooter, Date.now(), {priority:'event'}); }
    if (item && item.getAttribute('aria-disabled') === 'true') return;
  }, true);
  $(document).on('inserted.bs.tooltip.rmh shown.bs.tooltip.rmh', '.rmh-trigger', function () {
    const id = this.getAttribute('aria-describedby'), tip = id && document.getElementById(id);
    if (tip) tip.classList.add('rmh-tooltip');
  });
  $(document).on('shown.bs.dropdown.rmh', '.rmh-menu', function () {
    const panel = this.querySelector('.rmh-panel');
    panel.style.setProperty('--rmh-offset', '0px');
    const rect = panel.getBoundingClientRect();
    const offset = rect.left < 8 ? 8 - rect.left : rect.right > innerWidth - 8 ? innerWidth - 8 - rect.right : 0;
    panel.style.setProperty('--rmh-offset', offset + 'px');
  });
  $(document).on('hide.bs.dropdown.rmh', '.rmh-menu', function (event) {
    if ((this.dataset.rmhKeep !== 'false' && this.dataset.rmhSelecting === 'true') ||
        (this.dataset.rmhKeep === 'true' && restore.has(this))) event.preventDefault();
  });
  $(document).on('show.bs.modal.rmh', '.modal', function () {
    document.querySelectorAll('.rmh-menu.show[data-rmh-keep="true"]').forEach(root => restore.add(root));
  });
  $(document).on('hidden.bs.modal.rmh', '.modal', function () {
    restore.forEach(function (root) {
      if (root.isConnected && !root.classList.contains('show')) $(root).find('> .rmh-trigger').dropdown('show');
    });
    restore.clear();
  });
}(window.jQuery, window.Shiny));
