\name{TablaVancouver}
\alias{TablaVancouverUI}
\alias{TablaVancouverDatos}
\alias{TablaVancouverServer}
\alias{TablaVancouver}
\alias{TablaVancouverCeldas}
\title{Tablas Vancouver con búsqueda y selección}
\description{Los servidores por fila devuelven seleccion, modo y limpiar. La selección por celda conserva id y col. Los callbacks separan presentación, detalle y políticas del consumidor; las notas quedan fuera del contenedor.}
\usage{
TablaVancouverUI(id, titulo = NULL, subtitulo = NULL, footer = NULL, footer_tipo = "info",
                          sortable = TRUE, mostrar_nota = TRUE, estilo = "minimal", searchable = TRUE,
                          nota_fn = .vancouver_nota)
TablaVancouverDatos(datos, columnas = NULL, col_specs = NULL, col_labels = NULL,
                             on_row = NULL, on_sort = NULL, indices = seq_len(nrow(datos)),
                             preparar_fn = NULL, etiquetas_fn = function(x) stats::setNames(as.list(x), x),
                             tabla_fn = .vancouver_tabla,
                             texto_fn = identity)
TablaVancouverServer(id, data, columnas = NULL, col_specs = NULL, col_labels = NULL,
                                   id_col = NULL, modo_seleccion = "ninguno", sortable = TRUE,
                                   searchable = TRUE, page_size = 15, titulo = NULL, subtitulo = NULL, ...,
                                   preparar_fn = NULL,
                                   etiquetas_fn = function(x) stats::setNames(as.list(x), x),
                                   tabla_fn = .vancouver_tabla, texto_fn = identity, nota_fn = .vancouver_nota)
TablaVancouver(..., footer = NULL, footer_tipo = "info", modal_titulo_fn = NULL, modal_contenido_fn = NULL,
                                modal_pre_fn = NULL,
                                modal_size = "xl", modal_icon = NULL, modal_footer = NULL,
                                modal_footer_tipo = "info", nota_fn = .vancouver_nota)
TablaVancouverCeldas(id, data, titulo = NULL, subtitulo = NULL, footer = NULL,
                                col_specs = NULL, id_col = NULL, modal_titulo_fn = NULL,
                                modal_pre_fn = NULL, modal_contenido_fn = NULL, modal_size = "ventana",
                                datos_fn = TablaVancouverDatos, nota_fn = .vancouver_nota,
                                seleccion_permitida = function(sel) !is.null(sel))
}
\arguments{
\item{id}{ID local del componente o módulo, conservando el namespace de Shiny.}
\item{titulo}{Título del componente.}
\item{subtitulo}{Subtítulo opcional.}
\item{footer}{Texto, componente o reactivo de la nota externa.}
\item{footer_tipo}{Tipo de nota para compatibilidad con consumidores anteriores.}
\item{sortable}{Habilitar ordenación sin modificar el origen de datos.}
\item{mostrar_nota}{Opción conservada para compatibilidad de la interfaz.}
\item{estilo}{Nombre de estilo conservado para compatibilidad.}
\item{searchable}{Habilitar búsqueda literal en las columnas visibles.}
\item{nota_fn}{Callback que construye una nota externa; admite NULL para omitirla.}
\item{datos}{Data frame preparado por el consumidor; el componente no consulta bases de datos.}
\item{columnas}{Nombres de columnas visibles.}
\item{col_specs}{Especificaciones de formato y estilo de columnas.}
\item{col_labels}{Etiquetas alternativas nombradas por columna.}
\item{on_row}{Callback que genera el evento de una fila usando su índice original.}
\item{on_sort}{Callback que genera el evento de ordenación de una columna.}
\item{indices}{Posiciones originales de las filas, antes de búsqueda, ordenación y paginación.}
\item{preparar_fn}{Callback opcional para preparar columnas sin alterar el grano ni el alcance autorizado.}
\item{etiquetas_fn}{Callback que devuelve etiquetas nombradas a partir de nombres de columnas.}
\item{tabla_fn}{Constructor de tabla gt; por defecto se utiliza racafeTableVancouver.}
\item{texto_fn}{Callback de presentación de texto; no modifica valores ni claves del origen.}
\item{data}{Data frame o reactivo que lo devuelve, ya limitado al alcance autorizado.}
\item{id_col}{Columna que conserva la identidad estable de las filas.}
\item{modo_seleccion}{Modo de interacción. El servidor de filas acepta fila o ninguno; la selección por celda usa su módulo propio.}
\item{page_size}{Cantidad de filas por página.}
\item{...}{Contenido Shiny, argumentos del renderizador o del servidor de tabla, según la función.}
\item{modal_titulo_fn}{Texto o callback del título del modal de detalle.}
\item{modal_contenido_fn}{Callback que produce el detalle de la selección sin efectuar escrituras implícitas.}
\item{modal_pre_fn}{Callback previo a abrir el detalle.}
\item{modal_size}{Tamaño corporativo del modal.}
\item{modal_icon}{Argumento conservado para compatibilidad; el título de este componente es texto.}
\item{modal_footer}{Nota externa del modal.}
\item{modal_footer_tipo}{Tipo de nota del modal para compatibilidad.}
\item{datos_fn}{Constructor de tabla gt para la selección por celda.}
\item{seleccion_permitida}{Callback que autoriza una selección visual; no sustituye el filtrado de permisos del servidor.}
}
\value{Objeto de presentación, cálculo o contrato reactivo descrito en los detalles de la función.}
