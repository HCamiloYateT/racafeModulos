const assert = require('node:assert/strict');
const {chromium} = require('playwright');
(async()=>{
  const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH,
    args:process.env.CI ? ['--no-sandbox'] : []});
  let checks=0; const check=(value,message)=>{assert.ok(value,message);checks++;};
  try {
    const page=await browser.newPage({viewport:{width:1800,height:1100}}),errors=[];
    page.on('pageerror',error=>errors.push(error.message));
    await page.goto(process.env.RACAFE_CARDS_URL || 'http://127.0.0.1:4102');
    await page.locator('#modal-valor_display').filter({hasText:'1,234.56'}).waitFor();
    const card=id=>page.locator(`#${id}-valor_display`).locator('xpath=ancestor::div[contains(@class,"rmcv-card")]');
    const red=card('modal'),blue=card('nav'),green=card('readonly');
    const style=await red.evaluate(el=>{const s=getComputedStyle(el);return {left:s.borderLeftWidth,
      color:s.borderLeftColor,top:s.borderTopWidth,right:s.borderRightWidth,bottom:s.borderBottomWidth,
      background:s.backgroundColor,shadow:s.boxShadow};});
    check(style.left==='3px'&&style.color==='rgb(218, 41, 28)', 'Borde lateral rojo');
    check(style.top==='0px'&&style.right==='0px'&&style.bottom==='0px','Sin contorno completo');
    check(style.background==='rgb(255, 255, 255)'&&style.shadow!=='none','Fondo claro y sombra suave');
    check(await red.locator('.rmcv-title').evaluate(el=>getComputedStyle(el).color==='rgb(218, 41, 28)'),
      'Título con color principal');
    check(await red.locator('.rmcv-value span').evaluate(el=>getComputedStyle(el).color==='rgb(218, 41, 28)'),
      'Valor KPI hereda color de tarjeta');
    check(await blue.locator('.rmcv-value span').evaluate(el=>getComputedStyle(el).color==='rgb(36, 90, 129)'),
      'Color principal configurable');
    check(await blue.locator('.rmcv-footer-content').evaluate(el=>getComputedStyle(el).color==='rgb(36, 90, 129)'),
      'Footer hereda color principal');
    check(await green.locator('.rmcv-value span').evaluate(el=>getComputedStyle(el).color==='rgb(52, 64, 84)'),
      'Color KPI independiente');
    check(await green.locator('.rmcv-footer-content').evaluate(el=>getComputedStyle(el).color==='rgb(102, 112, 133)'),
      'Color footer independiente');
    check(await red.locator('.rmcv-heading button').count()===0&&await red.locator('.rmcv-footer button').count()===1,
      'Botón ubicado únicamente en footer');
    const bounds=await red.boundingBox(),button=await red.locator('button').boundingBox();
    check(bounds.x+bounds.width-button.x-button.width<=12,'Botón alineado a la derecha');
    const value=await red.locator('.rmcv-value').boundingBox();
    check(button.y>=value.y+value.height,'Botón debajo del KPI');
    check(await green.locator('button').count()===0,'Botón opcional ausente');
    const guide=page.locator('.rf-info-grid').first();
    check(await guide.locator('.rmcv-card').count()===4,'Cuatro tarjetas informativas');
    check(await guide.locator('.rmcv-value,button').count()===0,'Sin KPI ni acciones cuando se solicitan ocultos');
    const guideHeights=await guide.locator('.rmcv-card').evaluateAll(nodes=>nodes.map(el=>el.getBoundingClientRect().height));
    check(Math.max(...guideHeights)-Math.min(...guideHeights)<2,'Tarjetas informativas de altura uniforme');
    if(process.env.RACAFE_CARDS_SCREENSHOTS) await guide.screenshot({path:`${process.env.RACAFE_CARDS_SCREENSHOTS}/tarjetas.png`});
    await red.locator('button').click();await page.locator('.rf-modal-detail.show').waitFor();
    check((await page.locator('.modal.show').textContent()).includes('Evento gestionado por la aplicación'),
      'Evento abre modal desde aplicación');
    await page.locator('.modal.show [data-dismiss="modal"]').click();
    await page.waitForFunction(()=>!document.querySelector('.modal.show'));
    await blue.locator('button').click();await page.getByRole('heading',{name:'Navegación recibida'}).waitFor();
    await page.locator('#destination').filter({hasText:'1'}).waitFor();
    check(true,'Evento navega a otra pestaña');
    await page.locator('.nav-link[data-value="tarjetas"]').click();
    await page.waitForFunction(()=>/\$modal\s+\[1\] 1/.test(document.querySelector('#state').textContent)&&
      /\$nav\s+\[1\] 1/.test(document.querySelector('#state').textContent));
    check(true,'Eventos independientes por namespace');
    await red.locator('button').focus();await red.locator('button').press('Enter');
    await page.locator('.modal.show').waitFor();
    await page.locator('.modal.show [data-dismiss="modal"]').click();
    await page.waitForFunction(()=>/\$modal\s+\[1\] 2/.test(document.querySelector('#state').textContent));
    check(true,'Teclado produce un único evento');
    await page.locator('#valor').fill('0');await page.locator('#valor').press('Tab');
    await page.waitForFunction(()=>document.querySelector('#modal-valor_display').textContent==='0.00');
    check(true,'Mantiene valor cero reactivo');
    await page.setViewportSize({width:390,height:844});
    check(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1),'Sin desbordamiento móvil');
    const duplicates=await page.evaluate(()=>{const ids=new Map();document.querySelectorAll('[id]').forEach(el=>
      ids.set(el.id,(ids.get(el.id)||0)+1));return [...ids].filter(([id,n])=>id&&n>1);});
    assert.deepEqual(duplicates,[]);checks++;
    check(await page.locator('.shiny-output-error').count()===0,'Sin errores R');
    check(errors.length===0,errors.join('; '));
    console.log(`${checks} comprobaciones de tarjetas, colores y acciones superadas.`);
  }finally{await browser.close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
