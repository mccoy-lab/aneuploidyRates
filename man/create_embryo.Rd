% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_embryo.R
\name{create_embryo}
\alias{create_embryo}
\title{Create an embryo with the given proportion of aneuploidies.
Aneuploid cells are either adjacent or dispersed. The concordance of aneuploid
cells for each chromosome can be specificed; if fully concordant, a cell aneuploid
for chr1 will also be aneuploid for chr2 and all the other chrs.}
\usage{
create_embryo(
  n.cells = 200,
  n.chrs = 1,
  prop.aneuploid = 0.2,
  dispersal = 0,
  concordance = 0,
  embryo.size.fixed = T,
  embryo.size.sd = 5,
  euploidy = 2,
  rng.seed = NULL
)
}
\arguments{
\item{n.cells}{the number of cells in the embryo}

\item{n.chrs}{the number of chromosome pairs per cell}

\item{prop.aneuploid}{the proportion of aneuploid cells (0-1)}

\item{dispersal}{the dispersion vector of the aneuploid cells (0-1)}

\item{concordance}{the concordance between aneuploid cells for each chromosome (0-1).}

\item{embryo.size.fixed}{if true, the embryo is exactly the size in \code{n.cells}. If false, the embryo
size can vary according to \code{embryo.size.sd}.}

\item{embryo.size.sd}{the standard deviation of cell number if \code{embryo.size.fixed} is false.
The actual embryo size will be sampled from a normal distribution with mean of \code{n.cells} and
standard deviation \code{embryo.size.sd}.}

\item{euploidy}{the number of chromosome to be considered an euploid}

\item{rng.seed}{the seed for the rRandom Number Generation (RNG). Defaults to NULL. Use this to get the same embryo each time}

\item{n}{the neighbor matrix for cells}

\item{index}{the cell to test}

\item{embryo}{the embryo}

\item{cell.index}{the cell to test (0 for all cells)}

\item{ploidy}{the ploidy matrix from the embryo}

\item{chromosome}{the chromosome to set aneuploidies for}

\item{dispersion}{the dispersion of the aneuploid cells (0-1)}
}
\value{
an Embryo object

the modified ploidy table

Returns true if any of the closest cells are aneuploid

if cell.index is >0, return true if the chromosome is aneuploid,
Set aneuploidies in an embryo

Aneuploid cells are either adjacent or dispersed

the ploidy matrix with aneuploidies
}
\description{
Create an embryo with the given proportion of aneuploidies.
Aneuploid cells are either adjacent or dispersed. The concordance of aneuploid
cells for each chromosome can be specificed; if fully concordant, a cell aneuploid
for chr1 will also be aneuploid for chr2 and all the other chrs.
}
\examples{
Create an embryo with 200 cells, 20\% aneuploid and a single pair of chromosomes
per cell. Aneuploid cells are highly dispersed

Create the embryo above, but using a fixed seed for the random number generator
so the resulting embryo is reproducible.
embryo <- create_embryo(n.cells = 200, n.chrs = 1,  prop.aneuploid = 0.2,
                 dispersal =  0.9, rng.seed = 42)

Create an embryo with 3 pairs of chromosomes per cell, with all chromosome pairs
aneuploid in the same cells.
embryo <- create_embryo(n.cells = 200, n.chrs = 3,  prop.aneuploid = 0.2,
                 dispersal =  0.9, concordance = 1)

As above, but specifying a different aneuploidy level for each chromosome pair.
embryo <- create_embryo(n.cells = 200, n.chrs = 3,  prop.aneuploid = c(0.2, 0.1, 0.4),
                 dispersal =  0.9)
Set a cell to contain an aneuploid chromosome

}