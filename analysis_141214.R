load("ribosomal_bussiness2.rda")
library("gplots")

Norm <- ribosome_sub

Norm[,3:14] <- log2(ribosome_sub[,3:14]/ribosome_sub[,3])
Norm[,15:26] <- log2(ribosome_sub[,15:26]/ribosome_sub[,15])

is.na(Norm) <- sapply(Norm, is.infinite)
is.na(Norm) <- sapply(Norm, is.nan)
heatmap.2(as.matrix(ribosome_sub[,3:26]), labRow = ribosome_sub[,1], Colv = F, col = colorRampPalette(c("red","black","green")), trace = "none", na.color = "black")

