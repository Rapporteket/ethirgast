% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fig_distribution.R
\name{plot_distribution}
\alias{plot_distribution}
\title{Plot fordelingsfigur}
\usage{
plot_distribution(
  PlotParams,
  outfile = "",
  utvalgTxt,
  Andeler,
  Antall,
  enhetsUtvalg,
  shtxt,
  fargepalett = "BlaaOff"
)
}
\arguments{
\item{outfile}{Navn på fil figuren skrives til. Default: '' (Figur skrives
til systemets default output device (som regel skjerm))}

\item{enhetsUtvalg}{Lag figur for
0: Hele landet
1: Egen enhet mot resten av landet (Default)
2: Egen enhet}
}
\value{
En figur med søylediagram
}
\description{
Lager søylediagram
}
