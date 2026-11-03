const assert = require("node:assert/strict");
const { chromium } = require("playwright");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  const check = (value, message) => { assert.ok(value, message); checks++; };
  try {
    const errors = [], external = [];
    async function open(url) {
      const page = await browser.newPage({ viewport: { width: 1440, height: 1100 } });
      page.on("pageerror", error => errors.push(error.message));
      await page.route("**/*", route => {
        if (new URL(route.request().url()).origin !== new URL(url).origin) {
          external.push(route.request().url()); return route.abort();
        }
        return route.continue();
      });
      await page.goto(url); return page;
    }
    const widget = (page, id) => page.locator(".rf-select").filter({ has: page.locator(`[id='${id}']`) });
    const pick = async (page, id, value) => page.locator(`[id='${id}']`).evaluate((el, value) => {
      window.jQuery(el).selectpicker("val", value);
    }, value);
    async function single(page, prefix, newValue) {
      const sel = page.locator(`[id='${prefix}-sel']`);
      await sel.waitFor({ state: "attached" });
      const original = await sel.inputValue();
      check(await sel.locator("option").first().getAttribute("value") === "AGREGAR...", "AGREGAR primero en selección simple");
      check(await widget(page, `${prefix}-sel`).locator(".filter-option").evaluate(el =>
        getComputedStyle(el).textTransform) === "uppercase", "Mayúsculas solo visuales");
      await pick(page, `${prefix}-sel`, "AGREGAR...");
      const input = page.locator(`[id='${prefix}-nuevo_valor']`), save = page.locator(`[id='${prefix}-guardar_nuevo']`);
      const error = page.locator(`[id='${prefix}-error_valor'] .rf-message--error`);
      await input.waitFor();
      await page.waitForFunction(({ prefix, original }) => document.getElementById(prefix + '-sel').value === original,
        { prefix, original });
      check(await page.locator(`[id='${prefix}-nuevo_form'] .rf-required`).count() === 1, "Asterisco requerido único");
      check(await error.count() === 0, "Sin errores antes de guardar");
      function aligned() { return Promise.all([input.boundingBox(), save.boundingBox()]).then(([a, b]) =>
        Math.abs(a.y - b.y) < 1.5 && Math.abs(a.height - b.height) < 1.5); }
      check(await aligned(), "Botón alineado con el input y con la misma altura");
      await save.click(); await error.filter({ hasText: "Complete este campo" }).waitFor();
      check(await error.count() === 1, "Solo un error bajo el campo vacío");
      check(await aligned(), "El error no desplaza el botón respecto al input");
      check(await page.locator(".swal2-popup").count() === 0, "Vacío no abre modal ni SweetAlert");
      await input.fill("  CAFE  "); await input.press("Tab");
      await save.click(); await error.filter({ hasText: "Esta opción ya existe en el catálogo." }).waitFor();
      check(await error.count() === 1, "Duplicado sin repetir mensajes");
      check(await page.locator(".swal2-popup").count() === 0, "Duplicado no abre confirmación");
      await input.fill(`  ${newValue}  `); await input.press("Tab");
      await error.waitFor({ state: "detached" });
      check(true, "El error se elimina al corregir");
      await save.click(); await page.locator(".swal2-popup:visible").waitFor();
      check(await page.locator(".swal2-popup:visible").count() === 1, "Una confirmación solo para el valor válido");
      await page.evaluate(prefix => Shiny.setInputValue(prefix + "-guardar_nuevo", 50, { priority: "event" }), prefix);
      await page.locator(".swal2-confirm").click();
      await input.waitFor({ state: "detached" });
      await page.waitForFunction(({ prefix, value }) => document.getElementById(prefix + '-sel').value === value,
        { prefix, value: newValue });
      check(await sel.locator("option").first().getAttribute("value") === "AGREGAR...", "AGREGAR sigue primero después de actualizar");
      check(await sel.locator("option").evaluateAll((nodes, value) => nodes.filter(el => el.value === value).length, newValue) === 1,
        "Incorporación única pese al clic repetido");
      check(await sel.inputValue() === newValue, "Valor original recortado seleccionado sin convertirlo a mayúsculas");
      check(await page.locator(".rf-notification").count() > 0, "Notificación corporativa de éxito");
      await pick(page, `${prefix}-sel`, "AGREGAR..."); await input.waitFor();
      check(await input.inputValue() === "", "Captura limpia al volver a abrir");
    }
    const demo = await open(process.env.RACAFE_MODULE_URL || "http://127.0.0.1:4012");
    await single(demo, "cat_single", "Nuevo Canal");
    const multi = demo.locator("#seg_multiple-sel");
    await multi.waitFor({ state: "attached" });
    const multiWidget = widget(demo, "seg_multiple-sel");
    await multiWidget.locator("button.dropdown-toggle").click();
    await multiWidget.locator(".bs-select-all").click();
    await demo.waitForFunction(() => {
      const el = document.getElementById('seg_multiple-sel');
      return el.selectedOptions.length === 3 && !Array.from(el.selectedOptions).some(x => x.value === 'AGREGAR...');
    });
    check(await demo.locator("#seg_multiple-nuevo_valor").count() === 0, "TODOS no abre el formulario de agregar");
    check((await multiWidget.locator(".filter-option").innerText()).trim() === "TODOS", "TODOS ignora la opción de acción");
    await pick(demo, "seg_multiple-sel", ["Premium", "AGREGAR..."]);
    const multiInput = demo.locator("#seg_multiple-nuevo_valor"); await multiInput.waitFor();
    await multiInput.fill("Nuevo Segmento"); await multiInput.press("Tab");
    await demo.locator("#seg_multiple-guardar_nuevo").click();
    await demo.locator(".swal2-confirm").click();
    await multiInput.waitFor({ state: "detached" });
    await demo.waitForFunction(() => Array.from(document.getElementById('seg_multiple-sel').selectedOptions)
      .some(x => x.value === 'Nuevo Segmento'));
    const values = await multi.evaluate(el => Array.from(el.selectedOptions, option => option.value));
    check(JSON.stringify(values) === JSON.stringify(["Premium", "Nuevo Segmento"]), "Conserva selección múltiple anterior");
    check(!values.includes("AGREGAR..."), "La acción nunca forma parte de la selección");
    check(await multi.locator("option").first().getAttribute("value") === "AGREGAR...", "Acción siempre primera en múltiple");
    const gallery = await open(process.env.RACAFE_GALLERY_URL || "http://127.0.0.1:4013");
    await gallery.locator(".nav-link[data-value='category12']").click();
    const nested = gallery.locator("#preview select[id$='-simple-sel']");
    await nested.waitFor({ state: "attached" });
    const prefix = (await nested.getAttribute("id")).replace(/-sel$/, "");
    check(prefix.includes("example"), "Ejemplo realmente anidado en el namespace de la galería");
    await single(gallery, prefix, "Canal Galería");
    const nestedMulti = gallery.locator("#preview select[id$='-multiple-sel']");
    check(await nestedMulti.locator("option").evaluateAll(nodes => !nodes.some(el => el.value === "Canal Galería")),
      "Instancias independientes sin compartir catálogo");
    check((await gallery.locator("#code").innerText()).includes("racafeModulos::ListaDesplegableAgregable"),
      "Código copiable de la integración");
    check(errors.length === 0, `Sin errores JavaScript: ${errors.join("; ")}`);
    check(external.length === 0, `Demostraciones sin dependencias externas: ${external.join("; ")}`);
    console.log(`${checks} comprobaciones de lista agregable superadas.`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
