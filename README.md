# racafeModulos

Colección de módulos Shiny reutilizables para ecosistemas analíticos corporativos. El paquete agrupa componentes listos para dashboards `bs4Dash`: cajas KPI con modal, tablas `reactable` con selección normalizada y dropdowns de navegación reactivos.

## Instalación

```r
# Requiere devtools
# Repositorio público/privado según la configuración de GitHub de la organización
devtools::install_github("HCamiloYateT/racafeModulos")
```

> **Dependencia privada:** el paquete importa `racafe`; asegúrate de tener acceso e instalarlo antes de usar los módulos.

## Requisitos

- R `>= 4.1.0`.
- `shiny`, `bs4Dash`, `htmltools`, `reactable` y `racafe` para ejecutar los módulos.
- `dplyr`, `plotly`, `gt`, `tibble` y `purrr` para demos y ejemplos extendidos.

## Estructura del paquete

- `R/CajaModal.R`: módulo KPI tipo `bs4ValueBox` con modal opcional.
- `R/TablaReactable.R`: helpers internos y primera versión del módulo de tabla interactiva.
- `R/TablaReactable2.R`: variante de tabla con UI mínima y header/footer renderizados desde server.
- `R/DropdownMenuPlusModule.R`: wrapper modular y reactivo para `racafe::dropdownMenuPlus()`.
- `R/html_helpers.R`: helpers de formato HTML para valores y textos.
- `R/Demo*.R`: aplicaciones Shiny autocontenidas para validar escenarios de integración.

## Funciones exportadas

### Módulos Shiny

| Función | Descripción |
|---------|-------------|
| `CajaModalUI()` | UI de caja tipo `bs4ValueBox` para KPIs clicables. |
| `CajaModal()` | Server del módulo que renderiza la caja y abre un modal opcional. |
| `TablaReactableUI()` | UI del módulo de tabla con header/footer definidos desde UI. |
| `TablaReactable()` | Server de tabla con selección por fila/celda/columna, modal integrado y estilos condicionales. |
| `TablaReactable2UI()` | UI mínima de tabla; título, subtítulo, footer y nota se renderizan desde server. |
| `TablaReactable2()` | Variante de `TablaReactable()` orientada a configuración reactiva completa desde server, incluyendo `col_labels`. |
| `dropdownMenuPlusUI()` | Placeholder `uiOutput` para ubicar un dropdown en `leftUi` o `rightUi` de un navbar. |
| `dropdownMenuPlusServer()` | Server reactivo que renderiza `racafe::dropdownMenuPlus()` con items, badge, header y estado dinámicos. |

### Helpers

| Función | Descripción |
|---------|-------------|
| `html_valor()` | Helper para renderizar valores con formato numérico y color. |
| `html_texto()` | Helper para renderizar texto con estilo. |

### Demos

| Función | Descripción |
|---------|-------------|
| `DemoCajaModal()` | App de demostración con variantes de uso de `CajaModal`. |
| `DemoTablaReactable()` | App de demostración autocontenida para `TablaReactable`. |
| `DemoTablaReactable2()` | App integral para validar UI mínima, headers reactivos, `col_labels`, selección y modales en `TablaReactable2`. |
| `DemoDropdownMenuPlus()` | App autocontenida para probar dropdowns estáticos, tipificados y reactivos. |

## CajaModal

`CajaModalUI()` + `CajaModal()` encapsulan un `bs4Dash::bs4ValueBox` con capacidad de abrir un modal al hacer clic. El contenido del modal se define desde el módulo padre mediante una función sin argumentos evaluada en el momento del clic.

### Uso básico

```r
library(shiny)
library(bs4Dash)
library(racafeModulos)

# UI
CajaModalUI("mi_kpi")

# Server
CajaModal(
  id              = "mi_kpi",
  valor           = reactive(nrow(datos())),
  texto           = "Total clientes",
  icono           = "users",
  contenido_modal = function() reactable::reactableOutput("mi_tabla")
)
```

### Parámetros principales

| Parámetro | Tipo | Descripción |
|-----------|------|-------------|
| `id` | string | ID del módulo Shiny. |
| `valor` | reactive / escalar | Numérico, string, `shiny.tag` o `HTML`. |
| `formato` | string | `"numero"`, `"porcentaje"` o `"moneda"`. |
| `texto` | reactive / escalar | Subtítulo de la caja. |
| `icono` | string | Ícono Font Awesome. |
| `colores` | reactive / vector | Tokens bs4Dash; por ejemplo `c(fondo = "white")`. |
| `color_fondo_hex` | reactive / string | Color CSS para el fondo cuando no se usan tokens bs4Dash. |
| `mostrar_boton` | logical | Activa el botón `Ver detalle`. |
| `contenido_modal` | function / `NULL` | Función sin argumentos que retorna la UI del modal. |
| `titulo_modal` | reactive / string | Título del encabezado del modal. |
| `icono_modal` | string | Ícono del encabezado del modal. |
| `tamano_modal` | string | `"s"`, `"m"`, `"l"` o `"xl"`. |
| `footer` | reactive / string / `NULL` | Texto debajo de la caja. |
| `footer_class` | string | Clase CSS del párrafo footer. |

## TablaReactable

`TablaReactableUI()` + `TablaReactable()` proveen una tabla basada en `reactable` con:

- selección configurable por `fila`, `celda`, `columna` o `ninguno`;
- filtros de interacción en JS mediante `cols_activos` y `filas_bloqueadas`;
- restricción adicional en R mediante `filas_seleccionables`;
- selección normalizada para lógica del módulo padre;
- modal integrado opcional con `modal_titulo_fn`, `modal_contenido_fn`, `modal_pre_fn` y footer semántico;
- búsqueda en español, paginación y ordenamiento controlado desde R;
- estilos condicionales por columna con `cols_heatmap`, `cols_valor_color` y `umbral_valor_color`.

```r
# UI
TablaReactableUI(
  id        = "tabla_ventas",
  titulo    = "Ventas",
  subtitulo = "Haz clic en una fila",
  estilo    = "minimal2"
)

# Server
out <- TablaReactable(
  id = "tabla_ventas",
  data = reactive(df_ventas),
  modo_seleccion = "celda",
  id_col = "id",
  cols_activos = c("ventas", "margen"),
  cols_heatmap = "ventas",
  cols_valor_color = "margen",
  modal_titulo_fn = function(sel) paste("Detalle", sel$id),
  modal_contenido_fn = function(sel) shiny::tags$pre(utils::capture.output(str(sel)))
)

# out$seleccion() retorna la última selección normalizada.
```

### Valor retornado

`TablaReactable()` y `TablaReactable2()` retornan una lista con:

- `seleccion`: `reactive()` con la última selección normalizada.
- `modo`: modo activo de selección.
- `limpiar`: función para reiniciar la selección y el badge.

## TablaReactable2

`TablaReactable2UI()` mantiene una UI mínima: solo recibe `id` y `estilo`. Todo lo demás se configura en `TablaReactable2()`, por lo que es útil cuando título, subtítulo, footer, nota de interacción o etiquetas de columnas deben cambiar reactivamente desde server.

Diferencias principales frente a `TablaReactable()`:

- `titulo`, `subtitulo`, `footer`, `footer_tipo` y `mostrar_nota` aceptan valores estáticos o reactivos desde server.
- `col_labels` permite renombrar encabezados de forma estática o reactiva.
- La UI no duplica configuración visual de contenido; actúa como placeholder estable.

```r
# UI
TablaReactable2UI("tabla_cartera", estilo = "minimal")

# Server
TablaReactable2(
  id = "tabla_cartera",
  data = reactive(df_cartera),
  titulo = reactive(input$titulo_tabla),
  subtitulo = "Vista por segmento",
  footer = reactive(input$nota_fuente),
  col_labels = reactive(c(
    segmento = "Segmento",
    cartera = "Cartera MM",
    mora_pct = "Mora %"
  )),
  id_col = "segmento",
  modo_seleccion = "fila",
  cols_heatmap = "cartera",
  cols_valor_color = "mora_pct"
)
```

## DropdownMenuPlus

`dropdownMenuPlusUI()` + `dropdownMenuPlusServer()` permiten ubicar un menú desplegable reactivo dentro del navbar de `bs4Dash`. El server delega el renderizado visual a `racafe::dropdownMenuPlus()` y resuelve automáticamente valores estáticos o `reactive()` para items, conteos, badge y header.

```r
# UI, por ejemplo dentro de bs4DashNavbar(rightUi = ...)
dropdownMenuPlusUI("menu_alertas")

# Server
dropdownMenuPlusServer(
  id = "menu_alertas",
  icon = shiny::icon("exclamation-triangle"),
  items_r = reactive(lista_items_alertas()),
  numItems_r = reactive(length(lista_items_alertas())),
  badgeStatus_r = reactive(if (hay_criticas()) "danger" else "warning"),
  headerText = reactive("Alertas operativas"),
  footerText = "Ver todas"
)
```

### Parámetros principales

| Parámetro | Tipo | Descripción |
|-----------|------|-------------|
| `items_r` | reactive / list | Items que se pasan a `.list` en `racafe::dropdownMenuPlus()`. |
| `numItems_r` | reactive / entero / `NULL` | Conteo mostrado en el badge; si es `NULL`, usa `length(items_r)`. |
| `badgeStatus_r` | reactive / string / `NULL` | Estado visual del badge, por ejemplo `"danger"`, `"warning"`, `"info"` o `"success"`. |
| `headerText` | reactive / string / `NULL` | Texto del encabezado del dropdown. |
| `type` | string / `NULL` | Tipo delegado a `racafe::dropdownMenuPlus()`: `"messages"`, `"notifications"` o `"tasks"`. |
| `icon` | `shiny::icon()` / `NULL` | Ícono personalizado. |
| `href` | string / `NULL` | URL del footer. |
| `footerText` | string | Etiqueta del footer. |
| `showBadge` | logical | Controla si se muestra el badge. |
| `showHeader` | logical | Controla si se muestra el header. |
| `menuClass` | string / `NULL` | Clases CSS adicionales. |

## Helpers HTML

```r
html_valor(73.4, formato = "porcentaje", color = "#27AE60")
html_texto("Clientes activos", color = "#1A5276")
```

`html_valor()` usa utilidades de `racafe` para formatear números y envolverlos en HTML; `html_texto()` genera subtítulos estilizados para `CajaModal` u otros componentes Shiny.

## Demos

Ejecuta las demos desde una sesión interactiva de R:

```r
racafeModulos::DemoCajaModal()
racafeModulos::DemoTablaReactable()
racafeModulos::DemoTablaReactable2()
racafeModulos::DemoDropdownMenuPlus()
```

## Desarrollo

Flujo sugerido para validar cambios locales:

```r
# Cargar el paquete en desarrollo
devtools::load_all()

# Revisar documentación y estructura del paquete
devtools::document()
devtools::check()
```
