#Takes a dataset, and calculates the R squared if you leave a member out for all the members
##  INPUT: two vectors of the same length, y is the response. Log is a logical variable, 
# if its true, the r squared will be calculated for a lineal regression in the log space
## OUTPUT: a vector with all the r squared in the position of the element removed.

loov <- function(x, y, log = T)
{
	if(length(x) == length(y))
	{
		dataset <- cbind(x,y)
		colnames(dataset) <- c('x','y')
	}else{
		print('Elements do not have the same length')
		return(0)
	}
	
	r_squared <- c(1:nrow(dataset))
	for(i in 1:nrow(dataset))
	{
		todo <- dataset[-i,]
		if(log == T)
		{
			fit <- lm(log(todo[,2]) ~ log(todo[,1]))
			r_squared[i] <- summary(fit)$r.squared
		}else{
			fit <- lm(todo[,2] ~ todo[,1])
			r_squared[i] <- summary(fit)$r.squared
		}
	}
	
	return(r_squared)
}

