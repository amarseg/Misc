##FISH not scaling analyser
rm(list = ls())
library(ggplot2)
library(data.table)
path <- 'Y:/Amalia/LAS_X/Not-scaling/Nup107-prd1/'
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
results <- read.delim('results/results.txt', strings = F, header = T) 



areas <- read.delim('outlines/formatted_area.txt', strings = F, header = T) 
areas$Sample <- substr(areas$Sample, start = 1, stop = nchar(areas$Sample) - 17)

genes <- c('Nup107','rpb2','prd1')

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
p <- ggplot(all_data, aes(y = Area, x = Spots, col = gene ))
p + geom_point(alpha = 0.5) + stat_smooth(method = lm)
ggsave(filename = paste0(genes[1],'_',genes[2],'_',genes[3],'.pdf'), device = pdf)
