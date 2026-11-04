const assert = require('node:assert/strict');
const { chromium } = require('playwright');
(async () => {
  const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH,
    args:process.env.CI ? ['--no-sandbox'] : []});
  let checks = 0;
  const check = (condition,message) => { assert.ok(condition,message); checks++; };
  try {
    const page = await browser.newPage({viewport:{width:1440,height:1100}}), errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(process.env.RACAFE_NAVBAR_URL || 'http://127.0.0.1:4070');
    await page.locator('#one-state').filter({hasText:'activities'}).waitFor();
    await page.locator('#two-gt_table table').waitFor();
    check(await page.locator('.shiny-output-error').count() === 0, 'Sin errores R en la galería');
    check(await page.locator('.main-header .navbar-right > li.rmh-menu').count() === 6, 'rightUi contiene li directos');
    check(await page.locator('li.rmh-menu').count() === 6, 'Tres menús por namespace independiente');
    const menu = (parent, type) => page.locator(`li.rmh-menu[data-rmh-select="${parent}-${type}-item_click"]`);
    const oneTasks = menu('one','tasks'), twoTasks = menu('two','tasks');
    const triggerWidth = await oneTasks.locator('.rmh-trigger').evaluate(el => el.getBoundingClientRect().width);
    await oneTasks.locator('.rmh-trigger').click();
    await oneTasks.locator('.rsh-list-item').first().waitFor();
    check(await oneTasks.locator('.rsh-list-item').count() === 2, 'Listado de tareas reactivo');
    check(await oneTasks.locator('.rsh-list-title').first().textContent() === 'Tarea 1 Café & <texto>', 'Tildes y HTML escapado');
    check(await page.locator('.rsh-compact-table script').count() === 0, 'Datos de tabla sin scripts ejecutables');
    check(await oneTasks.locator('.rmh-trigger').evaluate(el => el.getBoundingClientRect().width) === triggerWidth,
      'El botón no adopta el ancho del panel');
    await oneTasks.locator('.rsh-list-item').first().click();
    await page.locator('.rf-modal-detail.show').waitFor();
    check(await oneTasks.evaluate(el => el.classList.contains('show')), 'keep_open conserva dropdown durante el modal');
    check(await page.locator('.rf-modal-detail .rf-modal-footer').count() === 1, 'Footer corporativo sin anidación duplicada');
    check(await page.locator('#one-activities input[value="confirm"]').isChecked(), 'Estado de checklist alineado por clave');
    await page.locator('#one-activities input[value="review"]').check();
    await page.waitForFunction(() => document.querySelector('#one-state').textContent.includes('review'));
    check(true, 'Checklist transmite claves al módulo padre');
    await page.locator('.rf-modal-detail [data-dismiss="modal"]').click();
    await page.waitForFunction(() => !document.querySelector('.modal.show'));
    check(await oneTasks.evaluate(el => el.classList.contains('show')), 'Dropdown conservado después de cerrar modal');
    await oneTasks.locator('.rmh-trigger').click();
    await page.waitForFunction(() => !document.querySelector('[data-rmh-select="one-tasks-item_click"]').classList.contains('show'));
    const clients = menu('one','clients');
    await clients.locator('.rmh-trigger').click();
    await clients.locator('.rsh-list-item').click();
    await page.locator('.modal.show').waitFor();
    check(!await clients.evaluate(el => el.classList.contains('show')), 'keep_open FALSE conserva el cierre solicitado');
    await page.waitForFunction(() => document.querySelector('#one-state').textContent.includes('c1'));
    check(true, 'Selección de cliente reactiva');
    await page.locator('.modal.show [data-dismiss="modal"]').click();
    await page.waitForFunction(() => !document.querySelector('.modal.show'));
    const direct = menu('one','direct');
    await direct.locator('.rmh-trigger').click();
    await page.locator('.modal.show').waitFor();
    check(!await direct.evaluate(el => el.classList.contains('show')), 'Acceso directo sin panel intermedio');
    await page.locator('.modal.show [data-dismiss="modal"]').click();
    await page.waitForFunction(() => !document.querySelector('.modal.show'));
    await page.locator('#one-add').click();
    await page.waitForFunction(() => /\$n\s+\[1\] 3/.test(document.querySelector('#one-state').textContent));
    check(/\$n\s+\[1\] 2/.test(await page.locator('#two-state').textContent()), 'Actualizar un módulo no cambia el segundo');
    await oneTasks.locator('.rmh-trigger').click();
    await page.waitForFunction(() => document.querySelectorAll('[data-rmh-select="one-tasks-item_click"] .rsh-list-item').length === 3);
    check(await oneTasks.locator('.navbar-badge').textContent() === '3', 'Contador del navbar actualizado');
    await oneTasks.locator('.rmh-trigger').click();
    const keyboard = page.locator('.rsh-list-item[data-rsh-input="one-standalone"]').first();
    await keyboard.focus(); await keyboard.press('Enter');
    await page.waitForFunction(() => document.querySelector('#one-state').textContent.includes('keyboard'));
    await keyboard.press('Space');
    check(await keyboard.getAttribute('aria-pressed') === 'true', 'Selección por Enter y Espacio');
    await page.locator('.rsh-list-item[data-rsh-input="one-standalone"][aria-disabled="true"]').dispatchEvent('click');
    check((await page.locator('#one-state').textContent()).includes('keyboard'), 'El elemento disabled no cambia la selección');
    await page.locator('#one-load').click();
    await page.waitForFunction(() => document.querySelector('#one-panel').getAttribute('aria-busy') === 'true' &&
      document.querySelector('#one-load_state').textContent === 'Consultando');
    const cover = await page.locator('#one-panel .rsh-overlay-cover').boundingBox();
    const panel = await page.locator('#one-panel').boundingBox();
    check(Math.abs(cover.width-panel.width) < 1 && Math.abs(cover.height-panel.height) < 1, 'Overlay limitado al contenedor');
    await page.locator('#two-outside').click();
    await page.waitForFunction(() => /\$outside\s+\[1\] 1/.test(document.querySelector('#two-state').textContent));
    check(await page.locator('#one-panel').getAttribute('aria-busy') === 'true', 'El trabajo asíncrono conserva overlay ante idle ajeno');
    await page.waitForFunction(() => document.querySelector('#one-panel').getAttribute('aria-busy') === 'false' &&
      document.querySelector('#one-load_state').textContent.includes('terminada'));
    check((await page.locator('#one-load_state').textContent()).includes('terminada'), 'Overlay se oculta al terminar');
    await page.locator('#one-fail').check(); await page.locator('#one-load').click();
    await page.waitForFunction(() => document.querySelector('#one-load_state').textContent.includes('Error simulado') &&
      document.querySelector('#one-panel').getAttribute('aria-busy') === 'false');
    check(true, 'Overlay se oculta también en la rama de error');
    await page.locator('#one-fail').uncheck();
    await page.locator('#one-query_confirm').click();
    await page.locator('.swal2-confirm').click();
    await page.waitForFunction(() => document.querySelector('#one-panel').getAttribute('aria-busy') === 'true' &&
      document.querySelector('#one-load_state').textContent === 'Consultando');
    check(true, 'El trigger de confirmación lógica muestra el overlay');
    await page.waitForFunction(() => document.querySelector('#one-panel').getAttribute('aria-busy') === 'false' &&
      /\$loads\s+\[1\] 2/.test(document.querySelector('#one-state').textContent));
    check(true, 'La confirmación completa la consulta y limpia el overlay');
    const tableStyle = await page.locator('#one-gt_table table').evaluate(el => {
      const cell = el.querySelector('tbody td'), style = getComputedStyle(cell), table = getComputedStyle(el);
      return {top:style.borderTopStyle,bottom:style.borderBottomStyle,background:table.backgroundColor,
        font:table.fontSize,padding:style.paddingTop};
    });
    check(/rgba\([^)]*,\s*0\)$/.test(tableStyle.background), 'La tabla gt es transparente');
    delete tableStyle.background;
    assert.deepEqual(tableStyle, {top:'none',bottom:'none',font:'11px',padding:'2px'}); checks++;
    const duplicates = await page.evaluate(() => {
      const counts = new Map(); document.querySelectorAll('[id]').forEach(el => counts.set(el.id, (counts.get(el.id)||0)+1));
      return [...counts].filter(([id,n]) => id && n>1);
    });
    assert.deepEqual(duplicates, []); checks++;
    await page.setViewportSize({width:390,height:844});
    await twoTasks.locator('.rmh-trigger').click();
    await twoTasks.locator('.rsh-list-item').first().waitFor();
    const bounds = await twoTasks.locator('.rmh-panel').boundingBox();
    check(bounds.x >= 0 && bounds.x + bounds.width <= 391, 'Panel dentro del viewport móvil');
    check(errors.length === 0, errors.join('; '));
    console.log(`${checks} comprobaciones de navbar, selección, modal, tablas, checklist y overlay superadas.`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode=1; });
