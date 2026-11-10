\name{cajaValorModal}
\alias{cajaValorModal}
\title{KPI reactivo con detalle opcional}
\description{Compone cajaValor sin cambiar sus IDs ni click_detalle. La tarjeta vive en el output id_tarjeta de la sesión padre. El consumidor controla el contenido del modal.}
\usage{
cajaValorModal(id, valor, formato = "numero", texto = "", icono = "info-circle", colores = NULL,
                      color_fondo_hex = NULL, mostrar_boton = TRUE, contenido_modal = NULL, titulo_modal = "",
                      icono_modal = NULL, tamano_modal = "ventana", footer = NULL, footer_class = NULL,
                           ui_fn = .caja_modal_ui, formatear = NULL)
}
\arguments{
\item{id}{ID local del componente o módulo, conservando el namespace de Shiny.}
\item{valor}{Valor o reactivo del KPI.}
\item{formato}{Formato numérico registrado en racafeShiny.}
\item{texto}{Etiqueta del KPI.}
\item{icono}{Nombre del icono de la tarjeta.}
\item{colores}{Argumento conservado para compatibilidad.}
\item{color_fondo_hex}{Color opcional del fondo de la tarjeta.}
\item{mostrar_boton}{Mostrar una acción únicamente cuando exista contenido de detalle.}
\item{contenido_modal}{Función o contenido Shiny del detalle.}
\item{titulo_modal}{Texto o reactivo del título del detalle.}
\item{icono_modal}{Argumento conservado para compatibilidad; el título no añade iconos.}
\item{tamano_modal}{Tamaño corporativo del detalle; ventana es el tamaño predeterminado.}
\item{footer}{Texto, componente o reactivo de la nota externa.}
\item{footer_class}{Argumento de compatibilidad; las notas usan el componente corporativo.}
\item{ui_fn}{Constructor de tarjeta que conserva los IDs de valor y botón.}
\item{formatear}{Callback opcional del valor. NULL conserva los formatos y metas existentes.}
}
\value{Objeto de presentación, cálculo o contrato reactivo descrito en los detalles de la función.}
