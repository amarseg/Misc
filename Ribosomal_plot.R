setwd("~")

proteomics <- read.delim("pombase_prot.tsv", header = T, strings = F)
RNA.seq <- read.delim("~/rna_seq0804/RPKM.tsv", header = T, strings = F)
ribosomal_dataset <- read.delim("~/proteomics0804/RP.table.280911.txt", header = T, strings = F)

ribosomal_rna <- subset(RNA.seq, row.names(RNA.seq) %in% ribosomal_dataset[,3])
ribosomal_prot <- subset(proteomics, proteomics[,1] %in% ribosomal_dataset[,3])
rpls <- unique(ribosomal_dataset[,1])

ribosome_sub <- matrix(NA, nrow = length(rpls), ncol = 26)
ribosome_sub[,1] <- rpls
names(ribosome_sub) <- c("Acs", "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7","r8","r9","r10", "r11","p0", "p1", "p2", "p3", "p4", "p5", "p6", "p7","p8","p9","p10", "p11")
	

for(i in 1:length(rpls))
{
	acs <- subset(ribosomal_dataset, ribosomal_dataset[,1] == rpls[i])
	ribosome_sub[i,2:13] <- apply(FUN = sum, MARGIN = 2,X = RNA.seq[row.names(RNA.seq) == acs[,3],])
	ribosome_sub[i,14:26] <- proteomics[proteomics[,1] %in% acs, 2:13]
}

save(ribosome_sub, file = "ribosomal_bussiness.rda") 



