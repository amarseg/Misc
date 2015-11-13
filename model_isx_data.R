library('matrixStats')
library('dplyr')
library('polynom')

isx.loader <- function(folder, filename, ext)
{
	data <- list()
	
	for(i in 0:11)
	{
		data[[i+1]] <- read.delim(paste0(folder, filename, i, ext), skip =  3, strings = F)
	}
	
	return(data)
}


median.calculator <- function(l1)
{
	ret <- as.data.frame(matrix(ncol = 6, nrow = 12, NA))
	colnames(ret) <- colnames(l1[[1]])
	
	for(i in 1:12)
	{
		ret[i,] <- apply(l1[[i]], 2, median)
	}
	
	return(ret)
}


sho1_isx <- isx.loader('C:/Users/am4613/Documents/Summaries_as_timecourses/ISX data/sho1/', 'cdc2_as_2502_tp1-_','_batch.txt')
sho2_isx <- isx.loader('C:/Users/am4613/Documents/Summaries_as_timecourses/ISX data/sho2/', 'cdc2as_1006_tp_','.txt')
sho3_isx <- isx.loader('C:/Users/am4613/Documents/Summaries_as_timecourses/ISX data/sho3/', 'cdc2as_1906_tp_','.txt')

avg_sho1 <- median.calculator(sho1_isx)
avg_sho1$rep <- 1
avg_sho2 <- median.calculator(sho2_isx)
avg_sho2$rep <- 2
avg_sho3 <- median.calculator(sho3_isx)
avg_sho3$rep <- 3


###Modelling with repeats

avg_isx <- rbind(avg_sho1, avg_sho2, avg_sho3)
avg_isx$time <- rep(1:12, 3)

write.table(avg_isx, sep = '\t', ' ')

write.table(avg_isx, sep = '\t', 'C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/isx_data_summary.txt')

fit <- lm(Length_Erode.M03..4. ~ poly(time,3,raw = T), data = avg_isx)
elongation_rate <- deriv(polynomial(fit$coefficients))

par(mfrow = c(2,1))
plot(avg_isx$Length_Erode.M03..4 ~ avg_isx$time)
points(fitted(fit)[1:12], type = 'l', col = 'red')
plot(predict(elongation_rate,c(1:12)), type = 'l', col = 'blue')

par(mfrow = c(1,1))
plot(avg_isx$Length_Erode.M03..4 ~ avg_isx$time, ylim = c(0,60))
points(fitted(fit)[1:12], type = 'l', col = 'red')
points(predict(elongation_rate,c(1:12)), type = 'l', col = 'blue')

par(mar = c(5,4,4,5)+0.1)
plot(avg_isx$Length_Erode.M03..4 ~ avg_isx$time)
points(fitted(fit)[1:12], type = 'l', col = 'red', lwd = 3)
par(new = T)
plot(predict(elongation_rate,c(1:12)), type = 'l', col = 'blue', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', lwd = 3)
axis(4)
legend('topleft', col = c('red','blue'), legend = c('Model', 'Elongation'))


###Separate plots

par(mfrow = c(1,3))

models <- list()
st_deriv <- list()

for(i in 1:3)
{
	data <- avg_isx[avg_isx$rep == i,]
	models[[i]] <- lm(Length_Erode.M03..4. ~ poly(time,3,raw = T), data = data)
	st_deriv[[i]] <- deriv(polynomial(models[[i]]$coefficients))
	plot(data$Length_Erode.M03..4. ~ data$time)
	points(fitted(models[[i]]), type = 'l', col = 'red', lwd = 3)
	par(new = T)
	plot(predict(st_deriv[[i]], c(1:12)), type = 'l', col = 'blue',xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', lwd = 3)
	axis(4)
}


