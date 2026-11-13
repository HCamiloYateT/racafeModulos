\name{Tableros}
\alias{TableroTareasTarjetas}
\alias{ColumnaKanbanUI}
\alias{TableroKanbanUI}
\title{Tarjetas y columnas de tableros}
\description{Componentes de presentación sin consultas ni permisos de negocio. El consumidor filtra datos y procesa las acciones; las tarjetas envían AccionTarea con la clave y acción. El consumidor confirma y valida antes de escribir.}
\usage{
TableroTareasTarjetas(datos, prioridades, actividades_fn, ns = identity,
                                  campos = c(id = "Id", titulo = "Titulo", descripcion = "Descripcion",
                                    prioridad = "Prioridad", responsable = "Responsable", tipo = "Tipo",
                                    cumplido = "Cumplido", archivado = "Archivado", vencimiento = "FechaCumplimiento"),
                                  nota_fn = .vancouver_nota, colores = PaletaPrioridadesTareas(),
                                  acciones_fn = NULL, arrastrable = FALSE)
ColumnaKanbanUI(titulo, cuerpo, cantidad, color = "#495057", badges = NULL, acciones = NULL, header_filled = TRUE)
TableroKanbanUI(..., id = NULL, ns = identity)
}
\arguments{
\item{datos}{Data frame preparado por el consumidor; el componente no consulta bases de datos.}
\item{prioridades}{Vector ordenado de prioridades que define las columnas del tablero.}
\item{actividades_fn}{Callback que devuelve textos y estados cumplidos de las actividades de una fila.}
\item{ns}{Función namespace opcional; los IDs ya namespaced no deben duplicarse.}
\item{campos}{Mapa entre campos del componente y columnas del origen. El origen debe contener los campos declarados.}
\item{nota_fn}{Callback que construye una nota externa; admite NULL para omitirla.}
\item{colores}{Vector nombrado de colores por prioridad.}
\item{acciones_fn}{Callback de presentación que devuelve acciones permitidas por fila. NULL muestra detalle.}
\item{arrastrable}{Activa los eventos de arrastre hacia las otras prioridades. No escribe datos.}
\item{header_filled}{Indica si el encabezado tiene fondo o solo borde de color.}
\item{titulo}{Título del componente.}
\item{cuerpo}{Contenido de la columna Kanban.}
\item{cantidad}{Contador visible de la columna.}
\item{color}{Color CSS del encabezado.}
\item{badges}{Badges o controles adicionales del encabezado.}
\item{acciones}{Vector nombrado de etiquetas de acciones permitidas por el consumidor.}
\item{...}{Contenido Shiny, argumentos del renderizador o del servidor de tabla, según la función.}
\item{id}{ID local del componente o módulo, conservando el namespace de Shiny.}
}
\value{Objeto de presentación, cálculo o contrato reactivo descrito en los detalles de la función.}
