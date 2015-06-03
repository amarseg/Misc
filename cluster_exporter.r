
##Take the result of a maSigPro analysis and exports each cluster to a text file, for posterior analysis 
## k = number of clusters

cluster_exporter <- function(data, pathway, k)
{
	gene_names <- data$summary

	expression_levels <- data[[2]]

	for(i in 1:length(expression_levels))
	{
		for(j in 1:k)
		{
			cluster <- gene_names[[i]][which(expression_levels[[i]][[1]][13] == j)]
			write.table(cluster, file = paste0(pathway,"degree",i,"_cluster",j,".tsv"), sep = "\t") 
		}
	}

}
