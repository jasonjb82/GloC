# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1  January 2009

iRH <- function(temp,mvp){
	es_Ta <- svp(temp) #saturated water vapor pressure at Ta (hPa)
	
	# Instantaneous relative humidity, RHumi%)
	RHum <- mvp / es_Ta * 100
	if (RHum > 100) RHum <- 100
	return(RHum)
}

rhMinMax <- function(rhavg, tmin, tmax, tavg=(tmin+tmax)/2) {
	tmin <- pmax(tmin, -5)
	tmax <- pmax(tmax, -5)
	tavg <- pmax(tavg, -5)
  es <- SVP(tavg)
	vp <- rhavg / 100 * es
  es <- SVP(tmax)
  rhmin <- 100*vp/es;
	rhmin <- pmax(0, pmin(100, rhmin))
  es <- SVP(tmin)
  rhmax <- 100*vp/es;
	rhmax <- pmax(0, pmin(100, rhmax))
	return(cbind(rhmin, rhmax))
}	



SVP <- function(temp) {
    .611 * 10^(7.5 * temp / (237.7 + temp))  #kpa
#	6.112 * exp(17.67*temp/(243.5 + temp))
}

vaporPressureDeficit <- function(rh, tavg){
    svp <- SVP(tavg)
    return((1-(rh/100))*svp)
}

tDew <- function(temp, rh) {
	temp - (100 - rh)/5
}


FtoC <- function(x) {(5/9)*(x-32) }
CtoF <- function(x) { x*9/5 + 32 }

atmp <- function(alt) {
  101.325 * (1 - 2.25577 * 10^-5 * alt) ^ 5.25588   # kPa 
}

rel2abshum <- function(rh, t) {
	es <- SVP(t)
	ea <- rh * es / 100
	M <- 18.02 # g/mol
	R <- 8.314472 # Pa?m?/(mol?K)
	T <- t + 273.15  # C to K
	hum <- ea*M/(T*R)
	return(hum)
}

abs2relhum <- function(hum, t) {
	M <- 18.02 # g/mol
	R <- 8.314472 # Pa?m?/(mol?K)
	T <- t + 273.15  # C to K
	ea <- hum / (M/(T*R))
	es <- SVP(t)
	rh <- 100 * ea / es
	rh  <- pmin(rh, 100)
	return(rh)
}



rel2spechum <- function(rh, t, alt) {
	es <- SVP(t)
	ea <- es * (rh / 100)
	p <- atmp(0)
	0.62198*ea / (p - ea)
}

spec2relhum <- function(spec, t, alt) {
	es <- SVP(t)
	100 * (spec * atmp(alt)) / ((0.62198 + spec) * es)
}




diurnalRH <- function(lat, date, rhavg, tmin, tmax, tavg=(tmin+tmax)/2) {
	hrtemp <- diurnalTemp(lat, date, tmin, tmax) 
	vp <- SVP(tavg) * rhavg / 100 
	hr <- 1:24
	es <- SVP(hrtemp[hr])
	rh <- 100*vp/es
	rh <- pmin(100, pmax(0, rh))
	return(rh)
}

