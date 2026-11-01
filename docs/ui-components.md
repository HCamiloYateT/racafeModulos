# Tarjeta e integración con componentes UI

La API, ejemplos completos de uso y la demo se documentan en README.md y en
man/Tarjeta.Rd y TarjetaUI.Rd. El informe conjunto, archivos modificados, integración
orientativa con CRM Nacional y límites de validación se entrega en el repositorio
Libreria-Racafe: `racafeShiny/docs/ui-components.md`.

La demo `inst/examples/componentes-ui.R` incluye funciones UI/servidor completas de
padre e hijo; no depende de CRM. Usa los nuevos colapsables y footer de racafeShiny,
seis tarjetas, módulos anidados, Cancelar/Guardar y updates de estado/valores.

Verificación nueva: 22 comprobaciones de Tarjeta y 38 de navegador por modo
(bs4Dash/Shiny), además de las 23 comprobaciones del componente en racafeShiny.
La tarjeta se validó desde su código real; el entorno no tiene instalado el paquete
legacy `racafe` requerido por racafeModulos. Por ello no se certifica el chequeo del
paquete completo. Se conserva la dependencia heredada y no se alteran CajaModal ni
sus funciones para evitar instalarla. Instale las dependencias reales antes de usar
R CMD INSTALL . y la demo distribuida con el paquete.

No se realizaron commits ni publicaciones. No se modificaron CRM Nacional,
PlantillaBs4Dash o Compartido. No se cambiaron versiones ni se añadieron dependencias
runtime; racafeForms y testthat se declaran como sugeridos para demo/pruebas.
