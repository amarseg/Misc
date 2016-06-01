##Are half-lifes of mRNA or protein different in my datasets than
## in the average?
rm(list =ls())

setwd('C:/Users/am4613/Documents/Summaries_as_timecourses/')



rna_list <- read.delim('both_clustering/Cluster_2.txt', header = T, strings = F)
prot_list <- read.delim('protein_isx_correlation/negative_correlation_length.txt', header = T, strings = F)

half_life_plotter <- function(gene_list, data = 'rna')
{
	if(data == 'rna')
	{
		mrna_half <- read.delim('analysis/rna_half_lives.txt', header = T,strings = F, skip = 6)
		ToDo <- mrna_half[mrna_half$Gene.name %in% gene_list,]
		boxplot(ToDo$Average.half.life, mrna_half$Average.half.life,
						names = c('Gene list','All genes'), outline = F)
		pval <- t.test(ToDo$Average.half.life, mrna_half$Average.half.life)
		legend('topleft', legend = paste0('P value: ',round(pval$p.value,3)), bty = 'o')
		
	}else if(data == 'protein')
	{
		prot_half <- read.delim('analysis/proteins_half_lives.txt', header = T, strings = F)
		ToDo <- prot_half[prot_half$ENSG %in% gene_list,]
		boxplot(as.numeric(ToDo$t1.2..min.),as.numeric(prot_half$t1.2..min.),
						names = c('Gene_list', 'All genes'), outline = F)
		pval <- t.test(as.numeric(ToDo$t1.2..min.),as.numeric(prot_half$t1.2..min.))
		legend('topleft', legend = paste0('P value: ',round(pval$p.value,3)), bty = 'o')
	}
}

