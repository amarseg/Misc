gene_plotter <- function(ids, what, norm = 'DESeq', rowlabs = NULL)
{
	library('pheatmap')
	source('C:/Users/am4613/Documents/GitHub/Proteomics/normalise_script.R')
	library('gtools', verbose = F)
	library('gplots')
	if(what == 'RNA' & norm == 'RPKM')
	{
		data <- read.delim('C:/Users/am4613/OneDrive/Summaries_as_timecourses/analysis/me_rpkm.txt', strings = F, header = T)
		norm_data <- normalise_rna(data)
		
	}else if(what == 'RNA' & norm == 'DESeq'){
		
		library('DESeq2', verbose = F)
		data <- read.delim('C:/Users/am4613/OneDrive/Summaries_as_timecourses/analysis/all_rev_counts.txt', strings = F, header = T)
		row.names(data) <- data$ID
		data <- data[,-1]
		data <- data[,mixedorder(colnames(data))]
		
		exp_design = data.frame(Time = factor(rep(0:11,each = 3)), 
														Replicate = rep(1:3,12), 
														group = rep(1,ncol(data)),
														row.names = colnames(data))
		
		cds <- DESeqDataSetFromMatrix(countData = data, colData = exp_design, design = ~Time)
		cds <- estimateSizeFactors(cds)
		size_fact <- sizeFactors(cds)
		
		norm_data <- data
		
		for(i in 1:36)
		{
			norm_data[,i] <- data[,i]/size_fact[i]
		}
		
		norm_data <- reorder_proteomics(norm_data)
		norm_data <- normalise_rna(norm_data)
		
		row.names(norm_data) <- row.names(data)
		
		
		
	}else if(what == 'Protein')
	{
		data <- read.delim('C:/Users/am4613/OneDrive/Summaries_as_timecourses/analysis/SQ_Results_PROTEIN.tsv', strings = F, header = T)
		norm_data <- normalise_ProtDataset(data, what = 'heatmap')
		norm_data <- reorder_proteomics(norm_data[,7:42])
	}else if(what == 'Both')
	{
		rna_data <- read.delim('C:/Users/am4613/OneDrive/Summaries_as_timecourses/analysis/me_rpkm.txt', strings = F, header = T)
		rna_norm <- normalise_rna(rna_data)
		
		prot_data <- read.delim('C:/Users/am4613/OneDrive/Summaries_as_timecourses/analysis/SQ_Results_PROTEIN.tsv', strings = F, header = T)
		prot_norm <- normalise_ProtDataset(prot_data, what = 'heatmap')
		prot_norm <- reorder_proteomics(prot_norm[,7:42])

		
		norm_data <- merge(rna_norm, prot_norm, by = 'row.names',all.x = T)
		row.names(norm_data) <- norm_data[,1]
		norm_data <- norm_data[,-1]
	}
	col = colorRampPalette(c('blue','gray','yellow'))(100)
	if(ids == 'all')
	{
		heatmap_tree <- pheatmap(as.matrix(norm_data), cluster_cols  = F, clustering_method = 'ward.D2', annotation_row = rowlabs, color = col)
	}else{
		todo <- norm_data[which(row.names(norm_data) %in% ids),]
		if(!is.null(rowlabs)){
			heatmap_tree <- pheatmap(as.matrix(todo), cluster_cols = F, clustering_method = 'ward.D2', annotation_row = rowlabs, color = col)
		}else{
			heatmap_tree <- pheatmap(as.matrix(todo), cluster_cols = F, clustering_method = 'ward.D2',annotation_row = rowlabs, color = col)
		}
	}
	
	return(heatmap_tree)
}
