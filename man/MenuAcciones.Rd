\name{MenuAcciones}
\alias{MenuAccionesDependencies}
\alias{MenuAccionesUI}
\title{Acciones flotantes para tablas y tarjetas}
\description{Menú con listeners delegados registrados una sola vez. El evento conserva codigo y accion. Para celdas serializadas como texto incluya MenuAccionesDependencies una vez en la UI.}
\usage{
MenuAccionesDependencies()
MenuAccionesUI(codigo, acciones, config = list(), ns = identity,
                           id_evento = "AccionSeleccionada", id_grupo = NULL)
}
\arguments{
\item{codigo}{Clave de la entidad que se devuelve con la acción seleccionada.}
\item{acciones}{Vector nombrado de etiquetas de acciones permitidas por el consumidor.}
\item{config}{Lista nombrada con icono y color opcionales para cada acción.}
\item{ns}{Función namespace opcional; los IDs ya namespaced no deben duplicarse.}
\item{id_evento}{ID local del input que recibe codigo y accion.}
\item{id_grupo}{ID del menú flotante; NULL deriva un ID de codigo y ns.}
}
\value{Objeto de presentación, cálculo o contrato reactivo descrito en los detalles de la función.}
