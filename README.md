# racafeModulos

Colección de módulos Shiny reutilizables para ecosistemas analíticos corporativos.

## Instalación

```r
# Requiere devtools
devtools::install_github("HCamiloYateT/racafeModulos")
```

> **Dependencia privada:** el paquete requiere `racafe`. Si no está disponible en CRAN,
> instálalo primero desde su repositorio correspondiente.

## Módulos exportados

| Módulo | Descripción |
|--------|-------------|
| `CajaModalUI()` | UI de caja tipo `bs4ValueBox` para uso en módulos Shiny. |
| `CajaModal()` | Server del módulo que renderiza la caja y abre modal opcional. |
| `DemoCajaModal()` | App de demostración con 12 variantes de uso. |
| `html_valor()` | Helper para renderizar valores con formato y color. |
| `html_texto()` | Helper para renderizar texto con estilo. |

## CajaModal

Encapsula un `bs4ValueBox` con la capacidad de abrir un modal al hacer click.
El contenido del modal es arbitrario y se define desde el módulo padre como una
función sin argumentos.

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
| `id` | string | ID del módulo Shiny |
| `valor` | reactive / escalar | Numérico, string, `shiny.tag` o `HTML` |
| `formato` | string | `"numero"` / `"porcentaje"` / `"moneda"` |
| `texto` | reactive / escalar | Subtítulo de la caja |
| `icono` | string | Ícono Font Awesome |
| `colores` | reactive / vector | Tokens bs4Dash: `c(fondo = "white")` |
| `color_fondo_hex` | reactive / string | Color hex CSS para el fondo |
| `mostrar_boton` | logical | Activa el botón "Ver detalle" |
| `contenido_modal` | function / NULL | `function()` sin args con la UI del modal |
| `titulo_modal` | reactive / string | Título del encabezado del modal |
| `tamano_modal` | string | `"s"` / `"m"` / `"l"` / `"xl"` |
| `footer` | reactive / string / NULL | Texto debajo de la caja |
| `footer_class` | string | Clase CSS del párrafo footer |

## Imports y Suggests

**Imports** (requeridas en tiempo de ejecución del módulo base):

- `shiny >= 1.7.0`
- `bs4Dash >= 2.3.0`
- `htmltools >= 0.5.0`
- `racafe`

**Suggests** (requeridas para `DemoCajaModal()`):

- `reactable >= 0.4.0`
- `dplyr >= 1.1.0`
- `plotly >= 4.10.0`
- `gt >= 0.9.0`
- `tibble >= 3.2.0`

## Demo

```r
racafeModulos::DemoCajaModal()
```
