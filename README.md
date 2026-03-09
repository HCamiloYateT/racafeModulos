# racafeModulos

Coleccion de modulos Shiny reutilizables para ecosistemas analiticos corporativos.

## Instalacion

```r
# Requiere devtools
devtools::install_github("HCamiloYateT/racafeModulos")
```

> **Dependencia privada:** el paquete requiere `racafe`. Si no esta disponible en CRAN,
> instalarlo primero desde su repositorio correspondiente antes de instalar `racafeModulos`.

## Modulos disponibles

| Modulo | Descripcion |
|--------|-------------|
| `CajaModal` / `CajaModalUI` | ValueBox bs4Dash con apertura de modal configurable |

## CajaModal

Encapsula un `bs4ValueBox` con la capacidad de abrir un modal al hacer click.
El contenido del modal es arbitrario y se define desde el modulo padre como una
funcion sin argumentos, lo que permite embeber cualquier output Shiny —tablas,
graficos, formularios— sin acoplar el modulo al contenido especifico.

### Uso basico

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

### Parametros principales

| Parametro | Tipo | Descripcion |
|-----------|------|-------------|
| `id` | string | ID del modulo Shiny |
| `valor` | reactive / escalar | Numerico, string, `shiny.tag` o `HTML` |
| `formato` | string | `"numero"` / `"porcentaje"` / `"moneda"` |
| `texto` | reactive / escalar | Subtitulo de la caja |
| `icono` | string | Icono Font Awesome |
| `colores` | reactive / vector | Tokens bs4Dash: `c(fondo = "white")` |
| `color_fondo_hex` | reactive / string | Color hex CSS para el fondo |
| `mostrar_boton` | logical | Activa el boton "Ver detalle" |
| `contenido_modal` | function / NULL | `function()` sin args con la UI del modal |
| `titulo_modal` | reactive / string | Titulo del encabezado del modal |
| `tamano_modal` | string | `"s"` / `"m"` / `"l"` / `"xl"` |
| `footer` | reactive / string / NULL | Texto debajo de la caja |
| `footer_class` | string | Clase CSS del parrafo footer |

### Helpers de formato

```r
# Valor numerico coloreado
html_valor(73.4, formato = "porcentaje", color = "#27AE60")

# Subtitulo coloreado
html_texto("Clientes Activos", color = "#1A5276")
```

### Clases CSS de footer disponibles

Las siguientes clases deben estar definidas en el `style.css` del proyecto:

| Clase | Uso sugerido |
|-------|-------------|
| `caja-modal-footer` | Footer estandar (texto neutro) |
| `caja-modal-footer-warning` | Footer de advertencia (indicadores en riesgo) |
| `caja-modal-footer-dark` | Footer oscuro (contrastes o datos criticos) |

### Patron para modulos complejos en modal

Cuando el modal contiene un modulo Shiny completo, el server del modulo hijo
**debe registrarse antes del primer click**:

```r
# Server del padre — registro previo obligatorio
ModTablaFiltrable("mod_tabla", data = clientes_r)

CajaModal("kpi_activos",
  valor           = reactive(n_activos()),
  texto           = "Clientes Activos",
  icono           = "check-circle",
  mostrar_boton   = TRUE,
  contenido_modal = function() ModTablaFiltrableUI("mod_tabla")
)
```

## Demo

```r
racafeModulos::DemoCajaModal()
```

Lanza una app Shiny con 12 instancias de `CajaModal` que cubren todas las
variaciones documentadas, con panel de codigo fuente colapsable en cada caja.

## Estructura del repositorio

```
racafeModulos/
├── R/
│   ├── CajaModal.R        # Modulo + helpers html_valor / html_texto
│   └── DemoCajaModal.R    # App de demostracion
├── man/                   # Documentacion generada por roxygen2
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
└── README.md
```

## Dependencias

**Imports** (requeridas en tiempo de ejecucion del modulo base):

- `shiny >= 1.7.0`
- `bs4Dash >= 2.3.0`
- `htmltools >= 0.5.0`
- `racafe`

**Suggests** (requeridas solo por `DemoCajaModal()`):

- `reactable >= 0.4.0`
- `dplyr >= 1.1.0`
- `plotly >= 4.10.0`
- `gt >= 0.9.0`
- `tibble >= 3.2.0`
