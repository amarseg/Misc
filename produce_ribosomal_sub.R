setwd("~")


proteomics <- read.delim("pombase_prot.tsv", header = T, strings = F)
RNA.seq <- read.delim("~/rna_seq0804/RPKM.tsv", header = T, strings = F)
ribosomal_dataset <- read.delim("~/proteomics0804/RP.table.280911.txt", header = T, strings = F)

proteomics <- aggregate(. ~ proteomics[,1], data = proteomics[,2:13], FUN = sum)
row.names(proteomics) <- proteomics[,1]
proteomics <- proteomics[,2:13]

ribosome_sub <- matrix(NA, nrow = nrow(ribosomal_dataset), ncol = 26)
ribosome_sub <- as.data.frame(ribosome_sub)
ribosome_sub[,1:2] <- ribosomal_dataset[,2:3]
names(ribosome_sub) <- c("Ribosomal protein","Acs", "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7","r8","r9","r10", "r11","p0", "p1", "p2", "p3", "p4", "p5", "p6", "p7","p8","p9","p10", "p11")


for(i in 1:nrow(ribosomal_dataset))
{
  
  acs <- ribosome_sub[i,2]
  if(acs %in% row.names(RNA.seq))
  {
    ribosome_sub[i,3:14] <- RNA.seq[which(acs == row.names(RNA.seq)),]
  }else
  {
    ribosome_sub[i,3:14] <- NA
  }
  if(acs %in% row.names(proteomics))
  {
    ribosome_sub[i,15:26] <- proteomics[which(acs == row.names(proteomics)),]
  }else
  {
    
    ribosome_sub[i,15:26] <- NA
  }
    
}

save(ribosome_sub, file = "ribosomal_bussiness2.rda") 

load("ribosomal_bussiness2.rda")
library("gplots")

ribosome_sub[,3:14] <- log2(ribosome_sub[,3:14]/ribosome_sub[,3])
ribosome_sub[,15:26] <- log2(ribosome_sub[,15:26]/ribosome_sub[,15])

is.na(ribosome_sub) <- sapply(ribosome_sub, is.infinite)
is.na(ribosome_sub) <- sapply(ribosome_sub, is.nan)
heatmap.2(as.matrix(ribosome_sub[,3:26]), labRow = ribosome_sub[,1], Colv = F, col = colorRampPalette(c("red","black","green")), trace = "none", na.color = "black")

