setwd("C:/Users//am4613/Desktop/am4613/")

proteomics <- read.delim("pombase_prot.tsv", header = T, strings = F)
RNA.seq <- read.delim("C:/Users//am4613/Desktop/am4613/rna_seq0804/RPKM.tsv", header = T, strings = F)

ids_repair <- read.delim("chromatin_modification", header = T, strings = F)

acetyl <- read.delim("acetilases", header = T, strings = F)
deacetyl <- read.delim("deacetylases", header = T, strings = F)
methyl <- read.delim("methylation", header = T, strings = F)
demethyl <- read.delim("demethylases", header = T, strings = F)

repair_data <- matrix(nrow = nrow(ids_repair), ncol = 15, NA)
repair_data <- as.data.frame(repair_data)
names(repair_data) <- c("Acs", "Name", "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7","r8","r9","r10", "r11","Color")
repair_data[,1:2] <- ids_repair[,1:2]

for(i in 1:nrow(repair_data))
{
  acs <- repair_data[i,1]
  repair_data[i,3:14] <- RNA.seq[which(acs == row.names(RNA.seq)),]
  if(acs %in% acetyl[,1])
  {
    repair_data[i,15] = "red"
    
  }else if(acs %in% deacetyl[,1])
  {
    repair_data[i,15] = "blue"
    
  }else if(acs %in% methyl[,1])
  {
    repair_data[i,15] = "purple"
    
  }else if(acs %in% demethyl[,1])
  {
    repair_data[i,15] = "green"
    
  }else{
    
    repair_data[i,15] = "white"
  }
    
  
}

library(gplots)
norm_repair <- repair_data
norm_repair[,3:14] <- log2(norm_repair[,3:14]/norm_repair[,3])

is.na(norm_repair) <- sapply(norm_repair, is.infinite)
is.na(norm_repair) <- sapply(norm_repair, is.nan)
arbolico <- heatmap.2(as.matrix(norm_repair[,3:14]), 
                      labRow = norm_repair[,2], Colv = F, col = colorRampPalette(c("blue","grey","yellow")), 
                      trace = "none", na.color = "black", symbreaks = T,cexRow = 0.20, RowSideColors = norm_repair[,15])
