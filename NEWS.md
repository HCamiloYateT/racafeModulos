# 0.5.0 — Tarjetas con borde lateral y acciones

- Rediseña cajaValor_ui con fondo claro, sombra suave, título/icono y borde lateral de color.
- Sitúa el botón opcional en el footer, conservando ID y click_detalle para navegación o modales.
- Añade color, color_kpi, color_footer y footer; mantiene los overrides color_borde/color_icono.
- Permite tarjetas informativas sin KPI con mostrar_kpi = FALSE y conserva compact.
- El valor hereda el color de UI salvo override explícito o color condicional por meta.
- Incluye demo, ayuda RStudio y pruebas de navegador de acciones, colores y vista móvil.

# 0.4.1 — Cajas compactas

- Añade compact opcional para reducir padding, separación entre líneas, iconos y valores.
- Conserva el tamaño anterior cuando compact no se solicita.

# 0.4.0 — Cajas corporativas de cliente

- cajaValor_ui acepta color_borde, color_icono y detalle opcionales; sus valores por defecto conservan la apariencia.
- cajaValor_server acepta empty para distinguir cantidades desconocidas de cero, conservando su default anterior.
- Mantiene IDs, eventos y firmas posicionales; añade documentación y pruebas de compatibilidad y valores faltantes.

# 0.3.1 — Presentación de encabezados

- Ordena listados con títulos a todo el ancho, metadatos y badges debajo del contenido.
- Centraliza el desplazamiento en el contenido del menú; encabezado y pie permanecen visibles.
- Mejora espaciado y legibilidad de formularios e indicadores dentro del navbar, sin CSS global.

# 0.3.0 — Módulo CajaValor

- Incorpora cajaValor_ui y cajaValor_server, trasladados desde racafeShiny sin cambiar HTML ni argumentos.
- Reutiliza racafeShiny::FormatearNumero y conserva el evento click_detalle.
- Prueba valores reactivos, cero, metas y namespaces independientes.

# 0.2.0 — Navbar corporativo

- Amplía MenuHeader sin duplicar módulos: li directo, listas reactivas, selección segura y retención configurable.
- Añade acceso directo y galería modular con detalle, checklist, tablas y overlays.
- Requiere racafeShiny >= 1.3.0 y racafeForms >= 0.7.0.

# Componentes corporativos (sin publicar)

- Añade confirmation = "summary" al selector agregable con racafeForms >= 0.6.0.
- Conserva confirmation = "alert" como predeterminado, confirm = FALSE y los IDs existentes.
- Demuestra el resumen en controlbar y prueba cancelación, selección, duplicados y confirmación única.

# Demo de modal y controlbar (sin publicar)

- Precarga dependencias RF y demuestra cancelación/confirmación con botones nativos normalizados.
- Añade listas agregables simples y múltiples al controlbar sin cambiar su implementación.
- Conserva AGREGAR primero, mayúsculas visuales, namespaces, selección y persistencia existente.

# Listas agregables y validación por campo (sin publicar)

- Sustituye controles locales y botones racafeShiny por componentes racafeForms.
- Mantiene AGREGAR... primero, excluido de seleccion y de la acción Todos.
- Valida vacío y duplicados inline después de guardar y limpia errores al corregir.
- Compara mayúsculas, espacios y tildes de vocales sin modificar el catálogo original; conserva ñ.
- Revalida al confirmar, consume cada solicitud y evita incorporaciones duplicadas.
- Conserva selecciones y binding del picker; añade presentación uppercase y confirmación configurable.
- Amplía demo autocontenida, documentación y pruebas; no modifica persistencia ni versiones.

# Desarrollo (sin publicar)

- Añade TarjetaUI y Tarjeta con indicador compacto, texto secundario, iconos y formato numérico.
- Expone accion() y conserva valor() como dato numérico; la aplicación elige modal, expansión o evento.
- Mantiene CajaModal y sus APIs sin cambios; no había una función Tarjeta en main.
- Centraliza el CSS de tarjetas y añade demo modular con componentes públicos de racafeShiny/racafeForms.
- Añade pruebas de reactividad, estabilidad de botones, namespaces y múltiples instancias.
