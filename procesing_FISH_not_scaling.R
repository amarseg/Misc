###Exploring FISH data for not scaling genes
rm(list = ls())
library(zoo)
library(ggplot2)
library(plyr)
library(boot)
ggplotRegression <- function (fit) {
	
	require(ggplot2)
	
	ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
		geom_point() +
		stat_smooth(method = "lm", col = "red") +
		labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
											 "Intercept =",signif(fit$coef[[1]],5 ),
											 " Slope =",signif(fit$coef[[2]], 5),
											 " P =",signif(summary(fit)$coef[2,4], 5))) 
}


path <- 'Y:/Pers - Amalia/LAS_X/Not-scaling/'
setwd(path)

all_data <- read.delim('all_genes.txt', header = T, strings =F)
all_data$volume <- (pi*all_data$Width^2*(all_data$Length - all_data$Width/3))/4
all_data$surface <- all_data$Width*pi*(all_data$Length-all_data$Width) + 2*pi*all_data$Width^2
all_data$conc <- all_data$Spots/all_data$Area

genes <- unique(all_data$gene)

p <- ggplot(data = all_data, aes(y = Spots, x = Area))
p + geom_point(colour = 'red') + facet_grid(~gene) + theme_bw() + stat_smooth(method = 'lm', col = 'blue')
ggsave('sm_FISH_newPlot.wmf', device = 'wmf')


for(i in 1:7)
{
	todo <- all_data[which(all_data$gene == genes[i]),]
	ggplotRegression(lm(Spots ~ Area,data = todo))
	ggsave(filename = paste0(genes[i],'.pdf'), device = pdf)
	
	todo_wo <- todo[todo$Width != 0,]
	ggplotRegression(lm(Spots ~ volume, data = todo_wo))
	ggsave(filename = paste0(genes[i],'volumen.pdf'))
	
	todo_wo <- todo[todo$Width != 0,]
	ggplotRegression(lm(Spots ~ surface, data = todo_wo))
	ggsave(filename = paste0(genes[i],'surface.pdf'))
	
	k = 10
	ord <- todo[order(todo$Area),]
	n <- 100
	h <- replicate(n,expr = rollmean(ord$Spots[sample(length(ord$Spots))], k = k, fill = NA, center = 'right'))
	ord_mean <- rollmean(ord$Spots, k = k, fill = NA)
	sd_h <- apply(h, 1, sd)
	bound <- data.frame(up = ord_mean+2*sd_h, down = ord_mean-2*sd_h, mean = ord_mean, area = ord$Area)
	p <- ggplot()
	p + geom_point(data = todo, aes(x = Area, y = Spots), color = 'red') + geom_line(data = bound, aes(x = area, y = mean), color = 'blue') + geom_ribbon(data = bound, aes(ymin = down, ymax = up, x = area, y = NULL), alpha = 0.25, fill = 'blue') + theme_bw()
	ggsave(filename = paste0('Sliding_window',genes[i],'.pdf'), device = pdf)
	
	p2 <- ggplot(bound, aes(x = area, y = mean))
	p2+geom_line()+geom_ribbon(data = bound, aes(ymin = down, ymax = up, x = area), alpha = 0.5)
}




