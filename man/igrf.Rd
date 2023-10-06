\name{igrf}
\title{igrf}
\alias{igrf}
\description{International Geomagnetic reference field version 13}
\usage{igrf(date=c(2020,1,1),lat=70,lon=20,height=100,isv=0,itype=1)}

\arguments{
\item{date}{c(year,month,day)}
\item{lat}{latitude north (-90...90)}
\item{lon}{longitude east (0...360)}
\item{height}{height in km, from sea-level if itype=1, from centre of Earth if itype=2}
\item{isv}{0 if main field values are required, 1 for secular variation}
\item{itype}{coordinate system, 1 for geodetic,2 for geocentric}
}

\value{
\item{isv}{isv}
\item{date}{date as decimal year}
\item{itype}{itype}
\item{alt}{altitude}
\item{colat}{colatitude}
\item{elong}{longitude east}
\item{x}{North field component [nT] if isv==0, [nT/year] if isv==1}
\item{y}{East field component [nT] if isv==0, [nT/year] if isv==1}
\item{z}{Vertical down field component [nT] if isv==0, [nT/year] if isv==1}
\item{f}{Total field intensity [nT] if isv==0, rubbish if isv==1}
}

\author{Ilkka Virtanen (University of Oulu, Finland)\cr
  \email{ilkka.i.virtanen@oulu.fi}}


\references{The function uses the FORTRAN routine igrf11syn from the
  package by Susan Macmillan and William Brown http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html}