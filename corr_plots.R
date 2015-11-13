###Combined heatmap transcriptomics and proteomics
library('matrixStats')
library('corrplot')
load('C:/Users/am4613/Documents/GitHub/Misc/GO.analysis.110914.rda')

as_safequant <- read.delim('C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/SQ_Results_PROTEIN.tsv', header = T, strings = F)


##Preprocessing, and averaging between technical replicates.Reordering

numbers <- as_safequant[,1:78]
numbers <- numbers[grep(numbers[,1], pattern = 'SP'),]

titulos <- colnames(numbers)[7:78]
titulos <- substr(titulos, 0, nchar(titulos) - 3)
titulos <- unique(titulos)

norm_sq <- matrix(ncol = 6+36, nrow = nrow(numbers), NA)
norm_sq <- as.data.frame(norm_sq)
colnames(norm_sq) <- c(colnames(numbers[,1:6]), titulos)

norm_sq[,1:6] <- numbers[,1:6]

##Average between technical replicates

j = 7

for(i in seq(7,78,2))
{
	norm_sq[,j] <- rowMeans(cbind(numbers[,i],numbers[,i+1]), na.rm = T)
	
	j = j +1
}

##Averaging between biological replicates for fuzzy clustering

median_sq <- matrix(ncol = 6+12, nrow = nrow(numbers), NA)
median_sq <- as.data.frame(median_sq)
colnames(median_sq) <- c(colnames(norm_sq[,1:6]),titles)

j = 7

for(i in seq(7,42,3))
{
	median_sq[,j] <- rowMedians(cbind(norm_sq[,i],norm_sq[,i+1],norm_sq[,i+2]), na.rm = T)
	
	j = j +1
}

row.names(median_sq) <- norm_sq[,1]
a <- strsplit(row.names(median_sq), "\\|")
vector_with_ids <- sapply(a,"[[",1)
row.names(median_sq) <- vector_with_ids

##Separate biological replicates into different lists, so they appear together on the heatmap

separate_prot <- list()

for(i in 1:3)
{
	separate_prot[[i]] <- median_sq[,seq(i+6,39+i,3)]
}

new_order <- cbind(separate_prot[[1]],separate_prot[[2]],separate_prot[[3]])
ids <- norm_sq[,1]
a <- strsplit(ids, '\\|')
ids <- sapply(a,"[[",1)
row.names(new_order) <- ids

load('P:/CDC2_RNA_051015.rda')

sam_rpkm <- cbind(CDC2_2,CDC2_3,CDC2_4)

transcriptomics <- sam_rpkm[,grep(colnames(sam_rpkm), pattern = 'Annot', invert = T)]
transcriptomics <- transcriptomics[,grep(colnames(transcriptomics), pattern = 'MM', invert = T)]
transcriptomics <- transcriptomics[,grep(colnames(transcriptomics), pattern = 'common', invert = T)]

median_trans <- matrix(ncol = 12, nrow = nrow(transcriptomics), NA) 
median_trans <- as.data.frame(median_trans)
row.names(median_trans) <- row.names(transcriptomics)

for(i in 1:12)
{
	median_trans[,i] <- rowMedians(cbind(transcriptomics[,i], transcriptomics[,i+12], transcriptomics[,i+24]))
}

load('C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/clusters_prot.rda')
prot_clusters <- f_clusters
load('C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/transcript_clusters.rda')
rna_clusters <- f_clusters

prot_cluster_plotting <- function(k)
{
	proteins <- median_sq[row.names(median_sq) %in% prot_clusters[[k]], 7:18]
	rna <- median_trans[row.names(median_trans) %in% prot_clusters[[k]],]
	cor_mat <- cor(proteins, rna, method = 'spearman')
	corrplot(cor_mat, method = 'square')
}

rna_cluster_plotting <- function(k)
{
	proteins <- median_sq[row.names(median_sq) %in% rna_clusters[[k]], 7:18]
	rna <- median_trans[row.names(median_trans) %in% row.names(proteins),]
	cor_mat <- cor(proteins, rna, method = 'spearman')
	corrplot(cor_mat, method = 'square')
}