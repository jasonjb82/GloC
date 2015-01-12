# R classes 
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.1
# Licence GPL v3


setClass('weather', 
	representation (
        stn ='character',
        rmk ='character',
		lon ='numeric',
		lat ='numeric',		
		alt ='numeric',
		vars = 'data.frame',
		w = 'data.frame'
	),
	prototype (	
        stn = "Station name",
        rmk = paste(Sys.time()),
		lon = 0,
		lat = 0,
		alt = 0,
		vars = data.frame(varcode=character(0),varname=character(0),unit=character(0),stringsAsFactors=FALSE),
		w = data.frame()
	),	
	validity = function(object)
	{
		con <- (x >= -180 & x <= 180 & y <= 90 & y >= -90)
		return(con)
	}
)

setMethod ('show' , 'weather', 
	function(object) {
		cat('class   :' , class(object), '\n')
		cat('Station ID:' , object@stn, '\n')
        cat('Remarks:' , object@rmk, '\n')
		cat('\n')	
		cat('Location' , '\n')
        cat('longitude:' , object@lon, '\n')
		cat('latitude :' , object@lat, '\n')
		cat('altitude :' , object@alt, '\n')
		cat('\n')
		l <- dim(object@w)[1]
		if (l == 0) {
			cat('no weather data\n')
		} else {
			print(object@vars)
			cat(l ,'rows: \n')
			if (l < 25) {
				print(object@w)
			} else {
				print(object@w[1:5,])
				cat('\n')
				cat('  (... ...  ...)\n')
				cat('\n')
				print(object@w[(l-5):l,])
			}
		}
	}
)	
