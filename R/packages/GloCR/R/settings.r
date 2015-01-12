# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  1 March 2012
# Version 0.0.2
# Licence GPL v3


# CONSTANTS
if (file.exists("settings.Rdata")) {
	message("Loading Geoclimate System settings")
	load("settings.Rdata")
}

message("Loading Geoclimate System constants")
# Upload Modes
UM.new    <- 1
UM.update <- 2
UM.append <- 3

# Source Modes
SM.database <- 1
SM.file <- 2

# Timestep Modes
TS.hourly <- 1
TS.daily <- 2
TS.monthly <- 3
TS.annually <- 4

message("Loading Geoclimate System settings")
# Database Settings
if (!exists("databases")) databases <- data.frame(dbms=character(0), host=character(0),port=numeric(0),user=character(0),password=character(0))

# Local Sources
