# CajaValor 0.5.0: apariencia y acciones

La presentación usa el patrón de tarjeta informativa corporativa: fondo blanco, borde lateral de 3px,
sombra suave, título con icono y contenido legible. El botón se mueve del encabezado al pie inferior derecho.
Los IDs y el evento click_detalle se mantienen; no se incorpora navegación ni reglas de negocio en el módulo.

| Parámetro | Zona o comportamiento |
| --- | --- |
| color | Acento principal: borde, título, icono y defaults de KPI/footer |
| color_borde, color_icono | Overrides de los dos colores anteriores |
| color_kpi, color_footer | Overrides independientes del valor y el pie |
| footer | Contenido opcional de pie; admite texto o tags/outputs Shiny |
| mostrar_boton | Incluye/omite el botón detalle; conserva su ID |
| label_boton, icono_boton | Texto e icono de la acción |
| mostrar_kpi | FALSE permite tarjetas informativas sin registrar servidor |
| compact | Densidad compacta para dashboards |

cajaValor_server admite color opcional explícito; NULL hereda el color de UI. La condición meta mantiene
su prioridad. El tratamiento opcional empty sigue distinguiendo valores desconocidos de cero.

La aplicación observa modulo$click_detalle() para llamar showModal, updateTabItems u otra acción autorizada.
Si mostrar_boton es FALSE no se genera ningún botón. No se añaden listeners JavaScript ni eventos propios.
Para namespaces, pasar ns("ventas") en la UI del padre y "ventas" al servidor del módulo.

La demo inst/examples/caja-valor/app.R contiene cuatro tarjetas informativas y tres KPIs: modal, navegación
y sin acción. Los colores se verifican en el navegador, incluidos overrides y valores reactivos cero.
La apariencia nueva es intencional; CSS encapsulado en rmcv-card, sin estilos globales sobre tablas o botones.

Validación: 98 pruebas testthat, 25 comprobaciones Playwright de tarjetas y 30 de integración CRM correctas.
R CMD build e instalación correctos y check sin errores, con las 4 advertencias y 1 nota históricas del paquete.
El CRM conserva sus 43 comprobaciones de SQL, cálculos, comentarios y construcción UI.
