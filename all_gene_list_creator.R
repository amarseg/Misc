###Full gene list creation
rm(list = ls())
setwd('C:/Users/am4613/Documents/Summaries_as_timecourses/')
source('C:/Users/am4613/Documents/GitHub/Misc/gene_plotter.R')
source('C:/Users/am4613/Documents/GitHub/Proteomics/normalise_script.R')
load('../GitHub/Misc/GO.analysis.110914.rda')


rpkms <- read.delim('analysis/me_rpkm.txt', header = T, strings = F)
norm_rpkms <- normalise_rna(rpkms)

all_genes <- list()

up_betr <- read.delim('betr_clustering/Cluster_1.txt', strings = F, header = T)
down_betr <- read.delim('betr_clustering/Cluster_2.txt', strings = F, header = T)

cl1_masigpro <- read.delim('Masigpro_clustering/Cluster_1.txt', strings = F, header = T)
cl2_masigpro <- read.delim('Masigpro_clustering/Cluster_2.txt', strings = F, header = T)
cl3_masigpro <- read.delim('Masigpro_clustering/Cluster_3.txt', strings = F, header = T)
cl4_masigpro <- read.delim('Masigpro_clustering/Cluster_4.txt', strings = F, header = T)

##Create clusters for the intersect between masigpro and betr

masig_total <- read.delim('Masigpro_clustering/result_masigpro.txt', header = T, strings = F)
betr_total <- read.delim('DE_genes/result_betr.txt', header = T, strings = F)

both <- intersect(masig_total$ID, row.names(betr_total))

gene_plotter(both, what = 'RNA', norm = 'heatmap')
hc <- hclust(dist(norm_rpkms[which(row.names(norm_rpkms) %in% both),]))

plot(hc)
k = 2
rect.hclust(hc, k = k)


pval <- 0.05
clusters<- cutree(hc, k)


pdf(file  = 'both_clustering/Results_both.pdf')
for(i in 1:max(clusters))
{
	cl <- names(clusters[clusters == i])
	write.table(cl, file = paste0('both_clustering/Cluster_',i,'.txt'), sep = '\t')
	
	go_results <- GOanalysis(cl, GOtable, all = 5123)
	go_results <- go_results[go_results[,2] > pval, ]
	write.table(go_results, file = paste0('both_clustering/GO_cluster',i,'.txt'), sep = '\t')
	
	gene_plotter(cl, what = 'RNA')
}
dev.off()

##Collate all data ina super list

up_both <- read.delim('both_clustering/Cluster_1.txt', header = T, strings = F)
down_both <- read.delim('both_clustering/Cluster_2.txt', header = T, strings = F)

all_genes <- list(up_betr, down_betr, cl1_masigpro, cl2_masigpro, cl3_masigpro, cl4_masigpro, up_both, down_both)

names(all_genes) <- c('up_betr','down_betr','cl1_masigpro','cl2_masigpro','cl3_masigpro','cl4_masigpro','up_both','down_both')
save(all_genes, file = 'all_DE_genes.rda')

##Create list with clusters coming from DESeq normalisation

de_up_masigpro <- read.delim('Masigpro_clustering/Deseq_norm//Cluster_1.txt', strings = F, header = T)
de_down_masigpro <- read.delim('Masigpro_clustering/Deseq_norm//Cluster_2.txt', strings = F, header = T)

de_up_betr <- read.delim('betr_clustering/Deseq_norm//Cluster_1.txt', strings = F, header = T)
de_down_betr <- read.delim('betr_clustering/Deseq_norm//Cluster_2.txt', strings = F, header = T)

##Plotting genes in DE in both using DESEq normalsation
deseq_norm <- read.delim('DESeq_norm_counts.txt', header = T, strings = F)

de_deseq_norm <- deseq_norm[which(row.names(deseq_norm) %in% both),]

gene_plotter(row.names(de_deseq_norm), what = 'RNA', norm = 'DESeq' )

de_deseq_norm <- normalise_rna(de_deseq_norm)

deseq_clust <- hclust(dist(de_deseq_norm))

plot(deseq_clust)
k = 2
rect.hclust(hc, k = k)


pval <- 0.05
clusters<- cutree(hc, k)


pdf(file  = 'both_clustering/DESeq_both/both_clustering.pdf')
for(i in 1:max(clusters))
{
	cl <- names(clusters[clusters == i])
	write.table(cl, file = paste0('both_clustering/DESeq_both/Cluster_',i,'.txt'), sep = '\t')
	
	go_results <- GOanalysis(cl, GOtable, all = 5123)
	go_results <- go_results[go_results[,2] > pval, ]
	write.table(go_results, file = paste0('both_clustering/DESeq_both/GO_cluster',i,'.txt'), sep = '\t')
	
	gene_plotter(cl, what = 'RNA', norm = 'DESeq')
}
dev.off()

up_both_deseq <- read.delim('both_clustering/DESeq_both/Cluster_1.txt', strings = F, header = T)
down_both_deseq <- read.delim('both_clustering/DESeq_both/Cluster_2.txt', strings = F, header = T)

de_deseq_all_genes <- list(de_up_masigpro, de_down_masigpro, de_up_betr, de_down_betr, up_both_deseq, down_both_deseq)
names(de_deseq_all_genes) <- c('up_masig','down_masig','up_betr','down_betr','up_both','down_both')
save(de_deseq_all_genes,file = 'DE_deseq_all_genes.rda')

##Create VENN diagram of both BETR and masigpro

mat <- data.frame(gene_name = row.names(rpkm), masipro = rep(0), betr = rep(0))