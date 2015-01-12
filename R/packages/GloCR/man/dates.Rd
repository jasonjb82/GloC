\name{date}

\alias{dateFromDoy}
\alias{doyFromDate}
\alias{dayFromDate}
\alias{monthFromDate}
\alias{yearFromDate}
\alias{isLeapYear}
\alias{daysInYear}
\alias{daysOfYear}
\alias{daylength}
\alias{daylength2}

\title{ date routines }
\description{
  Conversion between day of year number to date and back, and extraction of parts of a date.
}

\usage{
dateFromDoy(doy, year)
doyFromDate(date)
dayFromDate(date)
monthFromDate(date)
yearFromDate(date)
isLeapYear(year)
daysInYear(year)
daylength(lat, doy)
daylength2(lat, doy)
}

\arguments{
  \item{doy}{Day of the year (1..365) or (1:366) for leap years }
  \item{year}{year, e.g. 1982 }
  \item{date}{date (Date or character class) as 'yyyy-mm-dd', e.g. '1982-11-23' }  
  \item{lat}{latitude, in degrees. I.e. between -90.0 and 90.0 }
}

\value{
  Day of the year or Date
}

\author{ Robert J. Hijmans }
\examples{
doy <- 88
year <- 1970
date <- dateFromDoy(doy, year)
date
dateFromDoy(-15, 2000)
doyFromDate(date)
isLeapYear(2000)
daysInYear(2000)
daysInYear(1999)
}
\keyword{ spatial }
