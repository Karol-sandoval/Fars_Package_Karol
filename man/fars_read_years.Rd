% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Fars_read_years function}
\usage{
fars_read_years(years)
}
\arguments{
\item{Years}{a vector of numerical objects (years).}
}
\value{
this function returns a list where each component 
of the list contains the month and year of each record 
belonging to the exported dataset.
}
\description{
function that takes a set of years to generate file names 
and thus extract the month and year corresponding to each 
record in the dataset.
}
\note{
The function will create the name of a file 
depending on the set of input years, if the year 
does not exist, the function will throw an error.
}
\examples{
fars_read_years(c(2013,2014))

}
