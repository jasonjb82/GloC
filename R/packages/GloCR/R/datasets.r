# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  15 March 2013
# Version 0.0.1
# Licence GPL v3

datasets <- function(con){
	return(sqlQuery(con,"SELECT * FROM datasets"))
}


