\name{getWthXY}

\alias{getWthXY}


\title{ Get weather data}

\description{
  Get weather data from a NASA website
}


\usage{
getWthXY(lon, lat, start="1993-1-1", end="2009-12-31") 
}

\arguments{
  \item{lon}{ Latitude }  
  \item{lat}{ Longitude }    
  \item{start}{ First date }    
  \item{end}{ Last date }  
}


\value{
 An object of class 'weather'
}

\author{ Robert J. Hijmans }
