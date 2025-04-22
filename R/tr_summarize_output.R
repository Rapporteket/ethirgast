#' Transponer output fra tidyr::summarize
#'
#' Denne funksjonen tar som input resultatet av tidyr::summarize og returnerer dens
#' transponerte uten at formatene endres.
#'
#' Her kan detaljer skrives
#'
#' @return tr_frame Den transponerte av inputen
#'
#' @export
#'
tr_summarize_output <- function(x, kolnavn1 = ""){

rekkefolge <- names(x)[-1]
y <- x %>% tidyr::gather(names(x)[-1], key=nokkel, value = verdi) %>%
  tidyr::spread(key=names(x)[1], value = verdi)
y <- y[match(rekkefolge, y$nokkel), ]
names(y)[1] <- kolnavn1

return(invisible(y))
}
