setwd("~")

proteomics <- read.delim("pombase_prot.tsv", header = T, strings = F)
RNA.seq <- read.delim("~/rna_seq0804/RPKM.tsv", header = T, strings = F)

insu <- read.delim("~/proteomics0804/haploinsuficient.txt", strings = F, header = F)
pro <- read.delim("~/proteomics0804/haploproficient.txt", strings = F, header = F)
ids <- rbind(insu[1], pro[1])
category <-  c(rep("insufficcient", nrow(insu[1])), rep("proficient", nrow(pro[1])))
haplo_ids <- cbind(ids, category)

haplo_set <- matrix(nrow = nrow(haplo_ids), ncol = 26, NA)
haplo_set <- as.data.frame(haplo_set)
names(haplo_set) <- c("Acs", "Haplo?", "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7","r8","r9","r10", "r11","p0", "p1", "p2", "p3", "p4", "p5", "p6", "p7","p8","p9","p10", "p11")
haplo_set[,1:2] <- haplo_ids

for(i in 1:nrow(haplo_set))
{
  acs <- haplo_set[i,1]
  haplo_set[i,3:14] <- RNA.seq[which(acs == row.names(RNA.seq)),]
  if(acs %in% proteomics[,1])
  {
    ToDo <- subset(proteomics, proteomics[,1] == acs)
    haplo_set[i,15:26] <- as.numeric(ToDo[,2:13])
  }else{
    haplo_set[i,15:26] <- NA
  }
}

save(haplo_set, file = "haplo_set.rda")


library("gplots")

haplo_set[which(haplo_set[,15]) == "<NA>", 15] <- NA

Norm_haplo <- haplo_set

Norm_haplo[,3:14] <- log2(haplo_set[,3:14]/haplo_set[,3])
Norm_haplo[,15:26] <- log2(haplo_set[,15:26]/haplo_set[,15])

is.na(Norm_haplo) <- sapply(Norm_haplo, is.infinite)
is.na(Norm_haplo) <- sapply(Norm_haplo, is.nan)
arbolico <- heatmap.2(as.matrix(Norm_haplo[,3:26]), labRow = Norm_haplo[,2], Colv = F, col = colorRampPalette(c("blue","grey","yellow")), trace = "none", na.color = "black", symkey = T,cexRow = 0.5)
##To add colour row with proficient and insufficient
for(i in 1:nrow(Norm_haplo))
{
  if(Norm_haplo[i,2] == "proficient")
  {
    Norm_haplo[i,27] = "red"
  }else{
    
    Norm_haplo[i,27] = "blue"
  }
}

arbolico <- heatmap.2(as.matrix(Norm_haplo[,3:26]), labRow = Norm_haplo[,2], Colv = F, col = colorRampPalette(c("blue","blue","blue","darkgrey","yellow", "yellow","yellow")), trace = "none", 
                      na.color = "darkgrey", symbreaks = T,cexRow = 0.5, RowSideColors = Norm_haplo[,27])
sa
##To get genes for both clusters that i can see
reordered <- Norm_haplo[rev(arbolico$rowInd), arbolico$colInd]

reor_c1 <- reordered[1:164,]
reor_c2 <- reordered[165:274,]

tabla <- c(nrow(reor_c1[which(reor_c1[,2] == "insufficcient"),]), nrow(reor_c2[which(reor_c2[,2] == "insufficcient"),]))
tabla <- rbind(tabla,c(nrow(reor_c1[which(reor_c1[,2] == "proficient"),]), nrow(reor_c2[which(reor_c2[,2] == "proficient"),])))
row.names(tabla) <- c("insu", "pro")
col.names(table) <- c("cluster1", "cluster2")
fisher.test(tabla)

chromatin <- read.delim("chromatin_remodelling(GO).txt", header = T, strings = F)
remodellers <- reor_c1[which(reorc1[,1] %in% chromatin[,1])]
