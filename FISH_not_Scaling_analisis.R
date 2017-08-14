##FISH not scaling analyser
rm(list = ls())
library(ggplot2)
library(data.table)
library(zoo)
library(ggplot2)
library(ply)
library(boot)
path <- 'C:/Users/am4613/OneDrive/FISH/'
setwd(path)

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
##We need to trim the experiment IDs so the areas can be matched with the results
results <- read.delim('Result/results.txt', strings = F, header = T) 



areas <- read.delim('Outline/New/formatted_area.txt', strings = F, header = T) 
areas$Sample <- substr(areas$Sample, start = 1, stop = nchar(areas$Sample) - 24)

genes <- c('set1','rpb2','pdr1')

##First we split the result file to have the differet channels separately

sep_results <- list()

for(i in 0:2)
{
	channel <- paste0('C=',i)
	sep_results[[i+1]] <- results[grep(pattern = channel, x = results$Sample_Name),]
	sep_results[[i+1]]$Sample_Name<- substr(sep_results[[i+1]]$Sample_Name, start = 1, stop = nchar(sep_results[[i+1]]$Sample_Name) - 15) 
}

names(sep_results) <- genes

for(i in 1:3)
{
	
	sep_results[[i]] <- merge(sep_results[[i]], areas, by.x = c('Sample_Name','Cell'), by.y = c('Sample','Cell'), all.x = T)
	sep_results[[i]]$gene <- genes[i]
	
}

for(i in 1:3)
{
	ggplotRegression(lm(Area ~ Spots,data = sep_results[[i]],))
	ggsave(filename = paste0(genes[i],'.pdf'), device = pdf)
}

all_data <- rbindlist(sep_results)
write.table(all_data, sep = '\t', file = paste0(genes[1],'_',genes[2],'_',genes[3],'.txt'))


genes <- unique(all_data$gene)


for(i in 1:3)
{
	todo <- all_data[which(all_data$gene == genes[i]),]
	ggplotRegression(lm(Spots ~ Area,data = todo))
	ggsave(filename = paste0(genes[i],'.pdf'), device = pdf)
	
	k = 10
	ord <- todo[order(todo$Area),]
	n <- 100
	h <- replicate(n,expr = rollmean(ord$Spots[sample(length(ord$Spots))], k = k, fill = 'extend', center = 'right'))
	ord_mean <- rollmean(ord$Spots, k = k, fill = 'extend')
	sd_h <- apply(h, 1, sd)
	bound <- data.frame(up = ord_mean+2*sd_h, down = ord_mean-2*sd_h, mean = ord_mean, area = ord$Area)
	p <- ggplot()
	p + geom_point(data = todo, aes(x = Area, y = Spots), color = 'red') + geom_line(data = bound, aes(x = area, y = mean), color = 'blue') + geom_ribbon(data = bound, aes(ymin = down, ymax = up, x = area, y = NULL), alpha = 0.25, fill = 'blue') + theme_bw()
	ggsave(filename = paste0('Sliding_window',genes[i],'.pdf'), device = pdf)
	
	p2 <- ggplot(bound, aes(x = area, y = mean))
	p2+geom_line()+geom_ribbon(data = bound, aes(ymin = down, ymax = up, x = area), alpha = 0.5)
}




