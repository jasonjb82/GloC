# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1  July 2010

Thorntwaite <- function(tavg, lat=0) {
#Thornthwaite, C.W., 1948.
	# L <- avgMonthDaylength(lat)
	L <- rep(12, 12)
	tavg[tavg < 0] <- 0
	I <- sum((tavg/5)^1.514)
	alpha <- 6.75e-07 * I^3 - 7.71e-05 * I^2 + 1.79e-02 * I + 0.492
	PET <- 16 * L/12 * (10*tavg / I)^alpha

	# Willmott for > 26C
	i <- tavg > 26
	PET[i] <- -415.85 + 32.24*tavg[i] - 0.43*tavg[i]^2
	PET[tavg == 0] <- 0
	return(PET / 30)
}


ThornWillmott <- function(tavg, lat=0) {
	# L <- avgMonthDaylength(lat)
	L <- rep(12, 12)
	tavg[tavg < 0] <- 0
	I <- sum((tavg/5)^1.514)
	alpha <- 6.75e-07 * I^3 - 7.71e-05 * I^2 + 1.79e-02 * I + 0.492
	PET <- 16 * L/12 * (10*tavg / I)^alpha

	# Willmott for > 26C
	i <- tavg > 26
	PET[i] <- -415.85 + 32.24*tavg[i] - 0.43*tavg[i]^2
	PET[tavg == 0] <- 0
	return(PET / 30)
}



ThornWillCamargo <- function(tmin, tmax, lat=0, Pereira=FALSE) {
	# L <- avgMonthDaylength(lat)
	L <- rep(12, 12)
	
	trange <- tmax - tmin
	# Camargo FFECTIVE TEMPERATURE
	tef <- 0.35 * (3 * tmax - tmin)
	# Pereira adjustment
	if (Pereira) {
		tef <- tef * L / (24-L)	
	}
	
	tef[tef < 0] <- 0
	I <- sum((tef/5)^1.514)
	alpha <- 6.75e-07 * I^3 - 7.71e-05 * I^2 + 1.79e-02 * I + 0.492
	PET <- 16 * L/12 * (10*tef / I)^alpha

	# Willmott for > 26C
	i <- tef > 26
	PET[i] <- -415.85 + 32.24*tef[i] - 0.43*tef[i]^2
	PET[tef == 0] <- 0
	return(PET / 30)
}

#Camargo, A.P., Marin, F.R., Sentelhas, P.C. and Picini, A.G., 1999. Adjust of the Thornthwaite’s method to estimate the potential evapotranspiration for arid and superhumid climates, based on daily temperature amplitude. Rev. Bras. Agrometeorol. 7(2):251–257 
#Pereira, A.R. and W.O. Pruitt, 2004. Adaptation of the Thornthwaite scheme for estimating daily reference evapotranspiration. Agricultural Water Management 66: 251-257
#Thornthwaite, C.W., 1948. An approach toward a rational classification of climate. Geogr. Rev. 38:55–94. 
#Willmott, C.J., Rowe, C.M. and Mintz, Y., 1985. Climatology of the terrestrial seasonal water cycle. J. Climatol. 5:589–606. 
