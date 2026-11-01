const assert = require("node:assert/strict");
const { chromium } = require("playwright");
(async () => {
  const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH,
    args: process.env.CI ? ["--no-sandbox"] : [] });
  let checks = 0;
  function check(value, text) { assert.ok(value, text); checks++; }
  try {
    const page = await browser.newPage({ viewport: { width: 1366, height: 1000 } });
    const url = process.env.RACAFE_TEST_URL || "http://127.0.0.1:3940";
    const errors = [], external = [];
    page.on("pageerror", error => errors.push(error.message));
    await page.route("**/*", route => {
      if (new URL(route.request().url()).origin !== new URL(url).origin) {
        external.push(route.request().url()); route.abort();
      } else route.continue();
    });
    await page.goto(url);
    await page.locator("#tablero-estados").filter({ hasText: "Detalle: TRUE Comercial: FALSE" }).waitFor();
    const detail = page.locator("#tablero-detalle"), commercial = page.locator("#tablero-comercial");
    check(await detail.evaluate(el => el.open), "Detalle abierto inicialmente");
    check(!(await commercial.evaluate(el => el.open)), "Comercial cerrado inicialmente");
    check(await page.locator(".rcm-tarjeta").count() === 6, "Seis tarjetas en la cuadrícula");
    const columns = await page.locator(".rcm-tarjeta-grid").evaluate(el => getComputedStyle(el).gridTemplateColumns);
    check(columns.split(" ").length === 3, "Tres columnas de escritorio");
    check(await page.locator(".rcm-tarjeta-action").count() === 2, "Botón opcional en dos tarjetas");
    check((await page.locator("#tablero-kpi1-tarjeta").innerText()).includes("Creación: 20/10/2026"), "Texto secundario");
    check(await detail.locator("summary").evaluate(el => getComputedStyle(el, "::before").borderLeftColor) ===
      "rgb(218, 41, 28)", "Triángulo rojo corporativo");
    check(await detail.locator("summary").evaluate(el => getComputedStyle(el, "::before").transform) !== "none",
      "Triángulo abierto orientado abajo");
    await detail.locator("summary").focus(); await detail.locator("summary").press("Enter");
    await page.locator("#tablero-estados").filter({ hasText: "Detalle: FALSE" }).waitFor();
    check(!(await detail.evaluate(el => el.open)), "Teclado cierra y actualiza input$id");
    check(await detail.locator("summary").getAttribute("aria-expanded") === "false", "Estado accesible sincronizado");
    check(await detail.locator("summary").evaluate(el => getComputedStyle(el, "::before").transform) === "none",
      "Triángulo cerrado orientado a la derecha");
    await detail.locator("summary").press("Space");
    await page.locator("#tablero-estados").filter({ hasText: "Detalle: TRUE" }).waitFor();
    check(true, "Espacio abre el contenedor nativo");
    await page.locator("#tablero-abrir").click();
    await page.locator("#tablero-estados").filter({ hasText: "Comercial: TRUE" }).waitFor();
    check(await commercial.evaluate(el => el.open), "Update con ID local funciona en módulo");
    await page.locator("#tablero-oculto").filter({ hasText: "Valor oculto: 2" }).waitFor();
    check(true, "Output suspendido se reactiva al abrir");
    await page.locator("#tablero-grafico img").waitFor({ state: "visible" });
    check(true, "Gráfico se renderiza dentro del contenedor");
    await page.locator("#tablero-cerrar").click();
    await page.locator("#tablero-estados").filter({ hasText: "Comercial: FALSE" }).waitFor();
    check(!(await commercial.evaluate(el => el.open)), "Update con ID completo no duplica namespace");
    const button = page.locator("#tablero-kpi1-accion");
    check(await button.evaluate(el => el.getBoundingClientRect().width <= 26 && el.getBoundingClientRect().height <= 26),
      "Botón de detalle compacto y discreto");
    await button.evaluate(el => { window.originalCardButton = el; });
    await page.locator("#tablero-incrementar").click();
    await page.waitForFunction(() => [...document.querySelectorAll(".rcm-tarjeta-value")].every(el => el.textContent === "3"));
    check(await button.evaluate(el => el === window.originalCardButton), "Actualizar valor conserva el botón y binding");
    check((await page.locator("#tablero-eventos").innerText()).includes("Pedidos: 0 Lotes: 0"), "Actualizar no genera acciones");
    await page.locator("#tablero-abrir").click();
    await page.locator("#tablero-oculto").filter({ hasText: "Valor oculto: 3" }).waitFor();
    check(true, "Output actualizado mientras estaba oculto se reanuda con el valor actual");
    await page.locator("#tablero-cerrar").click();
    await button.focus(); await button.press("Enter");
    const modal = page.locator("#shiny-modal"); await modal.waitFor({ state: "visible" });
    check((await modal.innerText()).includes("Detalle de Pedidos"), "Tarjeta abre UI de módulo dentro del modal");
    check(await modal.locator("#tablero-edicion-nombre").count() === 1, "Namespace del módulo hijo correcto");
    const save = modal.locator("#tablero-edicion-guardar"), cancel = modal.locator("#tablero-edicion-cancelar");
    check(await save.evaluate(el => getComputedStyle(el).backgroundColor) === "rgb(218, 41, 28)", "Guardar rojo");
    check(await save.evaluate(el => el.getBoundingClientRect().height) ===
      await cancel.evaluate(el => el.getBoundingClientRect().height), "Altura uniforme en footer");
    check(await modal.locator(".rsh-modal-footer .rf-actions-right").evaluate(el => getComputedStyle(el).gap) === "8px",
      "Separación uniforme");
    await cancel.click(); await modal.waitFor({ state: "detached" });
    check((await page.locator("#tablero-eventos").innerText()).includes("Guardados: 0"), "Cancelar no guarda");
    await page.locator("#tablero-kpi2-accion").click(); await modal.waitFor({ state: "visible" });
    check((await modal.innerText()).includes("Detalle de Lotes"), "Segunda instancia abre su propia acción");
    await modal.locator("#tablero-edicion-guardar").click(); await modal.waitFor({ state: "detached" });
    await page.locator("#tablero-eventos").filter({ hasText: "Pedidos: 1 Lotes: 1 Guardados: 1" }).waitFor();
    check(true, "Guardar ejecuta un único evento");
    for (let i = 0; i < 3; i++) {
      await button.click(); await modal.waitFor({ state: "visible" });
      await modal.locator("#tablero-edicion-cancelar").click(); await modal.waitFor({ state: "detached" });
    }
    check((await page.locator("#tablero-eventos").innerText()).includes("Pedidos: 4 Lotes: 1 Guardados: 1"),
      "Reabrir no duplica listeners ni guardados");
    await page.locator("#tablero-nativo").click(); await modal.waitFor({ state: "visible" });
    check(await modal.locator(".rsh-modal-footer").count() === 0, "Modal ajeno conserva su footer nativo");
    check(!(await modal.locator("button[data-dismiss=modal]").getAttribute("class")).includes("rf-btn"),
      "No se añaden estilos corporativos globalmente");
    await modal.locator("button[data-dismiss=modal]").click(); await modal.waitFor({ state: "detached" });
    await page.setViewportSize({ width: 390, height: 844 });
    check(await page.locator(".rcm-tarjeta-grid").evaluate(el => getComputedStyle(el).gridTemplateColumns.split(" ").length) === 1,
      "Tarjetas adaptadas a móvil");
    check(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), "Sin overflow horizontal");
    check(await page.locator('script[src*="components.js"]').count() === 1, "Un solo binding JS");
    check(await page.locator('link[href*="components.css"]').count() === 1, "Un solo CSS de detalle/footer");
    check(await page.locator('link[href*="tarjeta.css"]').count() === 1, "Un solo CSS de tarjetas");
    check(errors.length === 0, errors.join("\n"));
    check(external.length === 0, "Sin dependencias externas");
    console.log(`${checks} comprobaciones UI superadas en ${url}`);
  } finally { await browser.close(); }
})().catch(error => { console.error(error); process.exitCode = 1; });
