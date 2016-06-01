##Different enrichments
rm(list = ls())
library('clusterProfiler')
setwd('C:/Users/am4613/Documents/Summaries_as_timecourses/')
set.seed(500)

up_both <- read.delim('both_clustering/Cluster_1.txt', header = T, strings = F)
down_both <- read.delim('both_clustering/Cluster_2.txt', header = T, strings = F)

not_scaling <- read.delim('../pearson_slope_list.txt', header = T, strings = F)
row.names(not_scaling) -> not_scaling

nc_remover <- function(ids)
{
	new_ids <- ids[grep(ids, pattern = 'SPNC', invert = T)]
	return(new_ids)
}

no_nc_up <- nc_remover(up_both[,1])
no_nc_down <- nc_remover(down_both[,1])
no_nc_scaling <- not_scaling

up_enrich <- enrichKEGG(no_nc_up, organism = 'spo')
down_enrich <- enrichKEGG(no_nc_down, organism = 'spo')

enrichMap(up_enrich)
enrichMap(down_enrich)

scaling_enrich <- enrichKEGG(no_nc_scaling, organism = 'spo')
enrichMap(scaling_enrich)
