# ListaDesplegableAgregable: catálogo de sesión y validación por campo

## Instalación y demostración

Instale primero la revisión nueva de racafeForms y después racafeModulos. Desde sus clones:

```r
remotes::install_local("/ruta/Libreria-Racafe/racafeForms", dependencies = NA, upgrade = "never")
remotes::install_local("/ruta/racafeModulos", dependencies = NA, upgrade = "never")
# Reiniciar R y recargar el navegador sin caché después de instalar.
racafeModulos::DemoListaDesplegableAgregable()
racafeForms::racafeFormsGallery()
```

Esta ampliación no cambia las versiones de los paquetes. La demo es autocontenida: carga los
recursos incluidos en racafeForms, sin GitHub Raw, CRM, bases de datos ni carpeta externa.
La categoría Agregables de la galería utiliza las mismas funciones exportadas, no otra
implementación. La galería puede ejecutarse sin racafeModulos, mostrando instrucciones para
instalarlo. racafeForms no tiene una dependencia obligatoria de racafeModulos.

## API y compatibilidad

```r
racafeModulos::ListaDesplegableAgregableUI("categoria")
# Dentro del servidor:
result <- racafeModulos::ListaDesplegableAgregable("categoria", label = "Categoría",
  choices = c("Café" = "cafe", "Té" = "te"), selected = "cafe", multiple = FALSE,
  fem = TRUE, uppercase = TRUE, confirm = TRUE)
# Reactivos: result$seleccion(), result$choices()
```

Los argumentos anteriores conservan su orden. uppercase y confirm se añaden al final;
ambos son TRUE por defecto. confirm = FALSE permite incorporar sin confirmación de negocio,
pero sigue validando dos veces antes de modificar el catálogo en memoria. Es una decisión
de la aplicación consumidora. No introduce ni cambia persistencia de catálogos.

Se mantienen los IDs picker_ui, nuevo_ui, sel, nuevo_valor, guardar_nuevo, error_valor,
ConfirmarNuevo y CancelarNuevo. ConfirmarNuevo ahora recibe el lógico de racafeConfirm, en
lugar del contador del botón del modal anterior. No dependa de ese contador interno; use
los reactivos retornados. Los IDs locales se resuelven con session$ns y funcionan en módulos
anidados; en la UI anidada pase ListaDesplegableAgregableUI(ns("categoria")).

racafeForms pasa a ser una dependencia obligatoria de racafeModulos porque construye sus
controles. shinyjs y shinyWidgets dejan de ser dependencias directas del módulo: la activación
y los pickers se gestionan mediante racafeForms. Si su aplicación usa shinyjs directamente,
debe declararlo por su cuenta. El resto de módulos y sus reglas de negocio se conservan.

## Presentación y selección

AGREGAR... es siempre la primera opción y es un valor reservado; no puede estar en el
catálogo inicial. En selección simple la opción vacía ocupa el segundo lugar. Se mantiene
la selección actual al abrir la captura. La opción de acción nunca aparece en seleccion().
En múltiple, TODOS selecciona solo entradas reales: no abre la captura ni cuenta AGREGAR...
como parte del catálogo. La selección total y sus etiquetas respetan el género configurado.

uppercase = TRUE cambia únicamente la presentación mediante CSS: NINGUNO/NINGUNA,
TODOS/TODAS y las etiquetas originales. No cambia choices ni valores. Por ejemplo, CAFÉ puede
verse en el control mientras seleccion() devuelve cafe. FALSE conserva la capitalización
original. No se transforma el catálogo inicial ni se deduplica silenciosamente.

## Captura y validación

La captura usa racafeInputText, racafeButton, racafeFormRow(actions = TRUE) y
racafeFormMessage. La fila corporativa alinea el botón con el input, sin estilos inline ni
JavaScript específico del módulo. Los errores no desplazan el botón.

No hay errores antes de guardar; el obligatorio tiene un asterisco rojo. El primer intento
vacío muestra **Complete este campo.** bajo el input. Un duplicado muestra exactamente
**Esta opción ya existe en el catálogo.**. Se mantiene un único resultado reactivo de
racafeValidateForm para esa captura y se limpia el mensaje al corregir. Nunca se usa un
modal o SweetAlert para informar esos errores. La captura no registra otro validador visual
que pudiera duplicar su racafeFormMessage.

La comparación de duplicados ignora mayúsculas/minúsculas, espacios de extremos y espacios
redundantes interiores, incluidos tabuladores/saltos de línea. Ignora tildes agudas y diéresis
de vocales (también sus marcas combinantes), pero distingue ñ de n. Compara tanto etiquetas
como códigos: Café y cafe son equivalentes; año y ano son distintos. El valor incorporado
solo pierde espacios extremos; conserva su escritura y espacios interiores originales.

Si confirm = TRUE, solo un valor válido puede abrir racafeConfirm. Se conserva la instantánea
del valor que se mostró en la confirmación. Al recibir la aceptación se consume la solicitud
y se vuelve a validar contra el catálogo vigente antes de incorporarla; los clics/aceptaciones
repetidos no incorporan dos veces una opción. Cancelar conserva la captura y no modifica el
catálogo. Una opción incorporada por otro cambio en memoria mientras se confirma impide
una segunda incorporación y muestra el error de duplicado.

Al guardar se actualiza el picker mediante racafeUpdateSelect, sin reconstruir su binding,
se selecciona el valor nuevo y se conservan las selecciones anteriores si multiple = TRUE.
Se limpia y oculta la captura y se muestra una notificación corporativa de éxito. El módulo
no llama funciones de base de datos, no escribe archivos ni modifica datos empresariales.

## Pruebas

```sh
Rscript -e 'testthat::test_local(".")'
Rscript -e 'shiny::runApp(racafeModulos::DemoListaDesplegableAgregable(), port=4012)'
Rscript -e 'shiny::runApp(racafeForms::racafeFormsGallery(), port=4013)'
RACAFE_MODULE_URL=http://127.0.0.1:4012 RACAFE_GALLERY_URL=http://127.0.0.1:4013 \
  node tests/browser/test-lista-agregable.cjs
```

La prueba de navegador requiere Playwright/Chromium y admite NODE_PATH, CHROMIUM_PATH y
CI=1. Comprueba vacío, duplicado, corrección, confirmación, clic repetido, actualización,
selección simple/múltiple, TODOS, alineación antes/después del error, captura limpia,
notificación y múltiples namespaces en la galería. No permite descargar recursos externos.

Archivos modificados: R/ListaDesplegableAgregable.R, DESCRIPTION, README, NEWS, esta guía,
man/ListaDesplegableAgregable.Rd, man/ListaDesplegableAgregableUI.Rd,
man/DemoListaDesplegableAgregable.Rd, tests/testthat/test-lista-agregable.R y
 tests/browser/test-lista-agregable.cjs. No se modifica CRM Nacional ni Compartido.

Resultados: 56 comprobaciones R y 44 comprobaciones de integración en Chromium, sin fallos.
El paquete racafeForms pasa 843 comprobaciones R y R CMD check con estado OK. Su galería
pasa 200 comprobaciones de navegador, la demo de formularios 43 y los modos heredados 11.

R CMD check de racafeModulos completa instalación y pruebas, pero conserva 4 advertencias
y 1 nota verificadas también en origin/main: puntero LICENSE, código histórico no ASCII,
dependencias/llamadas heredadas, documentación de otros componentes y formato de NEWS.
No se ha presentado ese resultado como un check limpio ni se han modificado módulos ajenos
para ocultar esas advertencias. Las tres funciones de este componente disponen ahora de ayuda.
