\name{TimelineUI}
\alias{TimelineUI}
\title{Línea de tiempo vertical reutilizable}
\usage{TimelineUI(eventos, contenido = NULL, max_height = 420)}
\arguments{
\item{eventos}{Data frame con Fecha y Evento. Descripcion, Usuario, Referencia y Estado son opcionales.}
\item{contenido}{Lista de contenido Shiny alineada con las filas originales.}
\item{max_height}{Altura máxima en píxeles.}
}
\description{Ordena los eventos cronológicamente y conserva eventos sin fecha al final sin imputar fechas.}
\value{Contenedor Shiny con desplazamiento interno.}
