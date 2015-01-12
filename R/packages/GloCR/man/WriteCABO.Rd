\name{Write weater data files}
\alias{writeCABOwth}

\title{ Write weather data files }

\description{
  write CABO format weather file
}

\usage{
writeCABOwth(wth, country='AAA', station=1, wind=2.5,  path=getwd(), print=TRUE, boundlat=TRUE, rainfact=1, tempfact=0)
}

\arguments{
  \item{wth}{ an object of class 'weather'}
  \item{country}{Character }    
  \item{station}{Integer}    
  \item{wind}{ value for daily wind speed}    
  \item{path}{ Describe \code{x} here }    
  \item{print}{ print progress to screen}    
  \item{boundlat}{ Bound latitide between -59 and 59 }    
  \item{rainfact}{ Describe \code{x} here }    
  \item{tempfact}{ Describe \code{x} here }    
}


\value{
Nothing. Used for its side effect of creating files.
}


\author{Robert J. Hijmans}


\examples{
wth <- getWthFile(system.file("weather/daily_weather_28368.nasa", package = "cropsim"))
writeCABOwth(wth)
}

\keyword{ spatial }
