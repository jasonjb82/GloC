##############################################################################
# title         : load_libraries.R;
# purpose       : load libraries & data to downscale NASA/POWER weather data;
# producer      : prepared by A. Sparks;
# last update   : in Los Banos, IRRI, June 2012;
##############################################################################

library(raster)
library(RODBC)
library(maptools)
library(stringr)
library(fields)

source('load_data.R')
