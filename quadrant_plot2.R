###4 quadrant plot hopefully

rpkms <- read.delim("C:/Users/am4613/Desktop/am4613/rna_seq0804/RPKM.tsv", header = T, strings = F)
rpkms <- log2(rpkms)
is.na(rpkms) <- sapply(rpkms, is.nan)
is.na(rpkms) <- sapply(rpkms, is.infinite)
rpkms[is.na(rpkms)] <- 0

prot_quant <- read.delim("C:/Users/am4613/Desktop/am4613/pombase_prot.tsv", header = T, strings = F)
prot_quant[,2:13] <- log2(prot_quant[,2:13]/prot_quant[,2])
is.na(prot_quant) <- sapply(prot_quant, is.nan)
is.na(prot_quant) <- sapply(prot_quant, is.infinite)
prot_quant[is.na(prot_quant)] <- 0 


col = c("red","blue", "green", "black", "pink","limegreen", "orange","cyan","darkblue", "purple", "yellow", "darkgreen")

par(mfrow = c(1,2))
time <- c(0:11)

time_norm <- list()

med_time_norm <- matrix(nrow = 12, ncol = 12, NA)
med_time_norm <- as.data.frame(med_time_norm)

for(i in 1:12){
	
	time_norm[i] <- read.delim(paste0("C:/Users/am4613/Desktop/Quadrant_plot/time/degree1_cluster",i,".tsv"), strings = F)
	ToDo <- subset(rpkms, row.names(rpkms) %in% time_norm[[i]])
	med_time_norm[i] <- apply(ToDo, 2, median)
}

med_prot_norm <- matrix(nrow = 12, ncol = 12, NA)
med_prot_norm <- as.data.frame(med_prot_norm)

for(i in 1:12)
{
		ToDo <- subset(prot_quant, prot_quant[,1] %in% time_norm[[i]])
		med_prot_norm[i] <- apply(ToDo[,2:13], 2, median)
}

matplot(med_time_norm, x = time , col = col, type = rep("l",12), lty = 1, lwd = 3, xlab = "Time at 36.5 [h]", ylab = "Log2(RNA levels)")
matplot(med_prot_norm, x = time, col = col, type = rep("l",12),lty = 1, lwd = 3, xlab = "Time at 36.5 [h]", ylab = "Log2(Protein level)")

plot(x = med_time_norm[1], y = med_prot_norm[1], col = col[1])
for(i in 2:12)
{
	line(x = med_time_norm[i], y = med_prot_norm[i], col = col [i])
}