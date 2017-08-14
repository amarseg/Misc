##Are half-lifes of mRNA or protein different in my datasets than
## in the average?



half_life_plotter <- function(gene_list, data = 'rna', test = 't.test')
{
	library(ggplot2)
	my_test <- if(test == 't.test') t.test else wilcox.test
	
	
	if(data == 'rna')
	{
		mrna_half <- read.delim('C:/Users/am4613/OneDrive/Summaries_as_timecourses/analysis/rna_half_lives.txt', header = T,strings = F, skip = 6)
		mrna_half$type <- NA
		mrna_half[mrna_half$Gene.name %in% gene_list,]$type <- 'Gene list'
		mrna_half[which(is.na(mrna_half$type)),]$type <- 'Not in list'
		pval <- my_test(mrna_half$Average.half.life ~ mrna_half$type)
		
		p <- ggplot(mrna_half, aes(x = type, y= Average.half.life,color = type))
		print(p + geom_boxplot(notch = T) )
		return(pval)
		
	}else if(data == 'protein')
	{
		prot_half <- read.delim('C:/Users/am4613/OneDrive/Summaries_as_timecourses/analysis/proteins_half_lives.txt', header = T, strings = F)
		prot_half$type <- NA
	 	prot_half[prot_half$ENSG %in% gene_list,]$type <- 'Gene List'
		prot_half[which(is.na(prot_half$type)),]$type <- 'Not in list'
		pval <- my_test(as.numeric(prot_half$t1.2..min.) ~ prot_half$type)
		p <- ggplot(prot_half, aes(x = type, y= as.numeric(t1.2..min.), color = type))
		print(p + geom_boxplot(notch = T) )
		return(pval)
		
	}
}

