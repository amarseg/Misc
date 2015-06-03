setwd("~")

proteomics <- read.delim("pombase_prot.tsv", header = T, strings = F)
RNA.seq <- read.delim("~/rna_seq0804/RPKM.tsv", header = T, strings = F)

row.names(proteomics) <- proteomics[,1]
proteomics <- log2(proteomics[,2:13]/proteomics[,2])
is.na(proteomics) <- sapply(proteomics, is.infinite)
is.na(proteomics) <- sapply(proteomics, is.nan)
proteomics[is.na(proteomics)] <- 0 

RNA.seq <- log2(RNA.seq/RNA.seq[,1])
is.na(RNA.seq) <- sapply(RNA.seq, is.infinite)
is.na(RNA.seq) <- sapply(RNA.seq, is.nan)
RNA.seq[is.na(RNA.seq)] <- 0 

plot_time <- function(ac)
{
	time = c(0:11)
	par(mfrow = c(1,2))
	
	rna <- RNA.seq[row.names(RNA.seq) %in% ac,]
	plot(x = time, y = rna, type = "b", ylim = c(-5,5))
	
	prot <- proteomics[row.names(proteomics) %in% ac,]
	plot(x = time, y = prot, type = "b", ylim = c(-5,5))
}
