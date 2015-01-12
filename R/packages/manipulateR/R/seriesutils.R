# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  30 April 2010
# Version 0.0.1
# Licence GPL v3


maxConsecutive <- function(nums){
	dvec <- cumsum(c(FALSE,diff(nums)!=1))	
	return(max(table(dvec)))
}
