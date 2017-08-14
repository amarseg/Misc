rm(list = ls())
library(ggplot2)
setwd('Z:/Amalia/live_cells_141216/')
library(corrplot)
library(plyr)
library(pheatmap)
library(zoo)
library(tools)
library(RColorBrewer)

file_names <- dir()
file_names <- file_names[grep(file_names, pattern = '.txt', fixed = T)]
lms <- list()
pdf('gene_results.pdf')
for(i in 1:length(file_names))
{
	ToDo <- read.delim(file_names[i], header = T, strings = F, skip = 3)
	ToDo$Conc <- ToDo[,3]/ToDo[,2]
	lms[[i]] <- lm(ToDo[,3] ~ ToDo[,2])
	colnames(ToDo) <- c('Number','Area','Intensity','Conc')
	ord_data <- ToDo[order(ToDo$Area),]
	n <- 8
	ord_data$bin <- cut(ord_data$Area, breaks = n, labels = 1:n)
	boxplot(data = ord_data, Conc ~ bin, main = file_names[i])
}
dev.off()

r_squared <- sapply(lms, function(x) summary(x)$r.squared)
coeff <- sapply(lms, function(x) coefficients(x))

limits <- c(0,10,15,20,25,30,35,40,45,50) 

obj_boxplots <- list()

pdf('gene_results_area_bin.pdf')
for(i in 1:length(file_names))
{
	ToDo <- read.delim(file_names[i], header = T, strings = F, skip = 3)
	ToDo$Conc <- ToDo[,3]/ToDo[,2]
	colnames(ToDo) <- c('Number','Area','Intensity','Conc')
	ord_data <- ToDo[order(ToDo$Area),]
	n <- 8
	ord_data$bin <- cut(ord_data$Area, breaks = limits)
	t <- boxplot(data = ord_data, Conc ~ bin, varwidth = T, main = file_names[i])
	obj_boxplots[[i]] <- t$stats
}
dev.off()

data_medians <- lapply(obj_boxplots, function(x) x[3,])
mean_sizes <- c(5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5)
median_lms <- list()
##Extract medians for all of them
for(i in 1:length(data_medians))
{
	median_lms[[i]] <- lm(data_medians[[i]] ~ mean_sizes)
}

coeff_medians <- sapply(median_lms, function(x) coefficients(x))
r_squared_median <- sapply(median_lms, function(x) summary(x)$r.squared)

window_mean <- list()
pdf('sliding_window_all.pdf')
for(i in 1:length(file_names))
{
	todo <- read.delim(file_names[i], header = T, strings = F, skip = 3)
	todo$Conc <- todo[,3]/todo[,2]
	colnames(todo) <- c('Number','Area','Intensity','Conc')
	k = 200
	ord <- todo[order(todo$Area),]
	n <- 100
	h <- replicate(n,expr = rollmean(ord$Conc[sample(length(ord$Conc))], k = k, fill = NA, center = 'right'))
	ord_mean <- rollmean(ord$Conc, k = k, fill = NA)
	sd_h <- apply(h, 1, sd)
	bound <- data.frame(up = ord_mean+2*sd_h, down = ord_mean-2*sd_h, mean = ord_mean, area = ord$Area)
	window_mean[[i]] <- bound
	p <- ggplot()
	t <-p + geom_point(data = todo, aes(x = Area, y = Conc), color = 'red') + geom_line(data = bound, aes(x = area, y = mean), color = 'blue') + geom_ribbon(data = bound, aes(ymin = down, ymax = up, x = area, y = NULL), alpha = 0.25, fill = 'blue') + theme_bw() + ggtitle(file_names[i])
	print(t)
}
dev.off()

plot(x = window_mean[[5]]$area, y = window_mean[[5]]$mean,col = 'darkgreen', ylim = c(0,150))
points(x = window_mean[[1]]$area, y = window_mean[[1]]$mean, col = 'blue')
points(x = window_mean[[2]]$area, y = window_mean[[2]]$mean, col = 'red')
points(x = window_mean[[3]]$area, y = window_mean[[3]]$mean, col = 'purple')
points(x = window_mean[[4]]$area, y = window_mean[[4]]$mean, col = 'orange')
abline(h = 50)

##normalise the data to the largest bin
norm_fun <- function(bins)
{
	norm_bin <- bins
	norm_bin$mean <- bins$mean/min(bins$mean,na.rm = T)
	return(norm_bin)
}

norm_data <- lapply(window_mean, norm_fun)
plot(x = norm_data[[5]]$area, y = norm_data[[5]]$mean,col = 'darkgreen', ylim = c(1,2.2))
points(x = norm_data[[1]]$area, y = norm_data[[1]]$mean, col = 'blue')
points(x = norm_data[[2]]$area, y = norm_data[[2]]$mean, col = 'red')
points(x = norm_data[[3]]$area, y = norm_data[[3]]$mean, col = 'purple')
points(x = norm_data[[4]]$area, y = norm_data[[4]]$mean, col = 'orange')

