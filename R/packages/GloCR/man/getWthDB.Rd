\name{Get weater data}

\alias{AccessGetWthXY}
\alias{AccessGetWthCell}
\alias{DBgetWthXY}
\alias{DBgetWthCell}
\alias{DBgetWthCellNoDSN}


\title{ Get weather data }

\description{
  Get weather data from a database
}


\usage{
AccessGetWthXY(database, table, lon, lat, rst=raster()) 
AccessGetWthCell(database, table, cell) 
DBgetWthXY(database, table, lon, lat, rst=raster()) 
DBgetWthCell(database, table, cell, verbose=FALSE)
DBgetWthCellNoDSN(table, cell, user, pwd, driver="MySQL ODBC 5.1 Driver", server="geo.irri.org", database="nasa") 
}

\arguments{
  \item{lon}{ Latitude }  
  \item{lat}{ Longitude }    
  \item{start}{ First date }    
  \item{end}{ Last date }  
  \item{database}{ }
  \item{table}{ }  
  \item{rst}{  }  
  \item{cell}{  }
  \item{user}{  }
  \item{pwd}{  }
  \item{driver}{  }
  \item{server}{  }
  \item{verbose}{  }
}


\value{
 An object of class 'weather'
}

\author{ Robert J. Hijmans and JOrrel Aunario }


