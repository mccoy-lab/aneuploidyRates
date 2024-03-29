% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_biopsy.R
\name{summarize_biopsy}
\alias{summarize_biopsy}
\title{Returns a biopsy summary for a set of meiotic and mitotic error rates and dispersal.}
\usage{
summarize_biopsy(
  num.em = 100,
  meio,
  mito,
  num.cell = 200,
  num.chr = 1,
  dispersal = 0,
  concordance = 0,
  hide.default.param = TRUE
)
}
\arguments{
\item{num.em}{the number of embryos to be created}

\item{meio}{the probability of having a meiotic error}

\item{mito}{the probability of having a meiotic error}

\item{num.cell}{the number of cells in the embryo}

\item{num.chr}{the number of chromosome pairs per cell}

\item{dispersal}{the dispersion vector of the aneuploid cells (0-1)}

\item{concordance}{the concordance between aneuploid cells for each chromosome (0-1).}

\item{hide.default.param}{the boolean to exclude other parameters used in
constructing the embryo: num.cell, num.chr, concordance. If false, the returned
vector will contain these values.}
}
\value{
a vector with columns: "prop.aneu", "prob.meio", "prob.mito", "dispersal",
and the three biopsy types shown as percentages.
}
\description{
A wrapper function for generating the biopsy results, it counts the proportions
of euploid, mosaic, and aneuploid biopsy results out of sampling a batch of
embryos with the same error rates and dispersal.
}
\examples{
summarize_biopsy(
  meio = 0,
  mito = 0.3,
  dispersal = 0.5,
  hide.default.param = TRUE
)
summarize_biopsy(
  num.em = 200,
  meio = 0.3,
  mito = 0.02,
  num.cell = 200,
  num.chr = 1,
  dispersal = 0,
  concordance = 0,
  hide.default.param = FALSE
)

}
