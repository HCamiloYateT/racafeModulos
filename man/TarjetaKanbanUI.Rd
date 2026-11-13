\name{TarjetaKanbanUI}
\alias{TarjetaKanbanUI}
\title{Tarjeta compacta compartida de tableros}
\usage{
TarjetaKanbanUI(titulo, responsables = NULL, contexto = NULL, cuerpo = NULL,
  menu = NULL, color = "#495057", codigo = NULL, origen = NULL,
  destinos = character(), busqueda = NULL)
}
\arguments{
\item{titulo}{Título de texto.}
\item{responsables}{Badges o contenido Shiny.}
\item{contexto}{Texto auxiliar compacto.}
\item{cuerpo}{Contenido expandible.}
\item{menu}{Menú de acciones de la aplicación.}
\item{color}{Color del borde izquierdo.}
\item{codigo}{Clave del registro; NULL desactiva arrastre.}
\item{origen}{Columna actual.}
\item{destinos}{Columnas permitidas por las reglas de la aplicación.}
\item{busqueda}{Texto usado para buscar la tarjeta.}
}
\description{Contenedor visual común; el tablero anfitrión valida y confirma los movimientos y las acciones.}
\value{Etiqueta Shiny con detalle colapsado y menú.}
