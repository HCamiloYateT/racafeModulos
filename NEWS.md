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
