% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/occupar_occupation.R
\name{isco88toESeC}
\alias{isco88toESeC}
\title{Compute ESeC}
\usage{
isco88toESeC(
  isco88,
  employed = NULL,
  unemployed = NULL,
  self.employed = NULL,
  supervisor = NULL,
  n.employees = NULL,
  n.classes = 10,
  keep.unemployed = TRUE,
  isco88.armed.forces.code = c(0, 10, 100, 110)
)
}
\arguments{
\item{isco88}{a vector with the ISCO-88 codes}

\item{employed}{a vector containing the value 1 whenever the person is employed}

\item{unemployed}{a vector containing the value 1 whenever the person is unemployed}

\item{self.employed}{a vector containing the value 1 whenever the person is self-employed}

\item{supervisor}{a vector containing the value 1 whenever the person is a supervisor}

\item{n.employees}{a vector containing the number of employees.}

\item{n.classes}{either one of the following integers: 10 (default), 7 ,6, or 4 indicating the number of categories to be used inthe ESEeC scheme. See details.}

\item{keep.unemployed}{boolean (default \code{TRUE}) indicating to keep (if \code{TRUE}) or not (if \code{FALSE}) the class of unemployed in the reduced ESeC scheme (with number of classes equal ot 7, 6, or 4). See details.}

\item{isco88.armed.forces.code}{numeric vector with the code for armed forces. Default is \code{c(0, 10, 100, 110)}}
}
\value{
It returns a list with two elements, the ESec code and its label
}
\description{
This function returns the ESeC (European Socio-economic classification) scheme
}
\details{
Harrison & Rose, 2006, "(ESeC) User Guide" explains the conceptual derivation of the class scheme. In the page 9-10, they suggest a possible reductions in the number of classes from 10 to 6, 5, or 3. The reduced ESeC class scheme in that case excludes the unemployed, but they state that it can be added if desired. This script by default keep the unemployed class, producing reduced schemes with 7, 6, or 4 categories. To exclude the unemployed, set \code{keep.unemployed=TRUE}
}
