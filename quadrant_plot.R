###4 quadrant plot hopefully

rpkms <- read.delim("C:/Users/am4613/Desktop/am4613/rna_seq0804/RPKM.tsv", header = T, strings = F)
rpkms <- log2(rpkms)
is.na(rpkms) <- sapply(rpkms, is.nan)
is.na(rpkms) <- sapply(rpkms, is.infinite)
rpkms[is.na(rpkms)] <- 0

col = c("red","blue", "green", "black", "pink","limegreen", "orange","cyan","darkblue", "purple", "cyan", "darkgreen")

par(mfrow = c(2,2))
time <- c(0:11)

rna_cell <- c(17.59,20.03,30.37,39.96,51.12,47.54,45.35,58.52,73.30,68.80,86.45,91.28)
rna_cell <- sort(rna_cell)


time_norm <- list()

med_time_norm <- matrix(nrow = 12, ncol = 12, NA)
med_time_norm <- as.data.frame(med_time_norm)

for(i in 1:12){
	
	time_norm[i] <- read.delim(paste0("C:/Users/am4613/Desktop/Quadrant_plot/time/degree1_cluster",i,".tsv"), strings = F)
	ToDo <- subset(rpkms, row.names(rpkms) %in% time_norm[[i]])
	med_time_norm[i] <- apply(ToDo, 2, median)
}

matplot(med_time_norm, x = time, type = "l", col = col)
matplot(med_time_norm, x = rna_cell, type = "l", col = col)

med_rna_norm <- matrix(nrow = 12, ncol = 12, NA)
med_rna_norm <- as.data.frame(med_rna_norm)


rna_norm <- list()

for(i in 1:12){
	
	rna_norm[i] <- read.delim(paste0("C:/Users/am4613/Desktop/Quadrant_plot/rna_norm/degree1_cluster",i,".tsv"), strings = F)
	ToDo <- subset(rpkms, row.names(rpkms) %in% rna_norm[[i]])
	med_rna_norm[i] <- apply(ToDo, 2, median)
}

matplot(med_rna_norm, x = time, type ="l", col = col)
matplot(med_rna_norm, x = rna_cell, type = "l", col = col)
