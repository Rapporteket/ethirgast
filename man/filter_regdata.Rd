% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_regdata.R
\name{filter_regdata}
\alias{filter_regdata}
\title{Function that filters the data and returns the filtered dataset along with a
text describing the filtering}
\usage{
filter_regdata(
  RegData,
  dateFrom = "2014-01-01",
  dateTo = "2100-01-01",
  minage = 0,
  maxage = 120,
  isMan = 99,
  urgency = 99,
  ab_access = "",
  minPRS = 0,
  maxPRS = 2.2,
  ASA = "",
  whoEcog = "",
  modGlasgow = "",
  malign = 99,
  op_group = "",
  ncsp = "",
  icd = "",
  daytime = 99,
  new_stomi = 99,
  accordion = "",
  colorpalette = "BlaaRapp",
  only_complete = TRUE
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{minPRS}{Minimum PRS (Default 0?)}

\item{maxPRS}{Maksimum PRS (Default 2?)}

\item{ASA}{ASA-grad, flervalg hvor (Default alle)
1: Ingen organisk, fysiologisk, biokjemisk eller psykisk forstyrrelse.
Den aktuelle lidelsen er lokalisert og gir ikke generelle systemforstyrrelser.
2: Moderat sykdom eller forstyrrelser som ikke forårsaker funksjonelle begrensninger.
3: Alvorlig sykdom eller forstyrrelse som gir definerte funksjonelle begrensninger.
4: Livstruende organisk sykdom som ikke behøver å være knyttet til den aktuelle
kirurgiske lidelsen eller som ikke alltid bedres ved det planlagte kirurgiske inngrepet.
5: Døende pasient som ikke forventes å overleve 24 timer uten kirurgi.}

\item{whoEcog}{WHO WCOG score, flervalg hvor (Default alle)
0: Fullt aktiv
1: Lett husarbeid og sittende arbeid
2: Oppe > 50% av dagen, selvstelt
3: Oppe < 50% av dagen, delvis selvstelt
4: Kun i stol/seng, hjelp til alt stell
9: Ukjent}

\item{malign}{Er diagnosen malign eller benign
0: Benign
1: Malign
99: Alle}

\item{ncsp}{NCSP-koder(r) som skal være inkludert i utvalget}

\item{colour_palette}{Colour palette to be used in figures (Default: BlaaRapp)}
}
\value{
OutData A list containing the filtered dataset and a text describing
the filtering. A text string indicating the colour palette is also returned.
}
\description{
Function that filters the data and returns the filtered dataset along with a
text describing the filtering
}
