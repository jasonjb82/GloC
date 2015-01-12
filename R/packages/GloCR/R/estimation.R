# Author: Jorrel Khalil Aunario, jaunario@gmail.com
# License GPL3
# Version 0.1  September 2009

hargreavesSRad <- function(doy, tmax, tmin, latdd){
    DR <- 1+0.033*cos(pi*2/366*doy)    
    LatRad <- pi/180*latdd
    SRdec <- 0.409*sin(2*pi/366*doy-1.98)
    SHA <- acos(-tan(LatRad)*tan(SRdec))
    R_a <- 24*60/pi*0.082*DR*(SHA*sin(LatRad)*sin(SRdec)+cos(LatRad)*cos(SRdec)*sin(SHA))
    return(R_a*0.16*sqrt(tmax-tmin))       
}
