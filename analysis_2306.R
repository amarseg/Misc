source("C:/Users/am4613/Documents/R scripts/oneDplot.v2.r")

path = "C:/Users/am4613/Desktop/cdc2as/"

cdc2 <- list()

filename <- "cdc2as_1906_tp_"

for(i in 0:11)
{
	cdc2[[i+1]] <- read.delim(file=paste0(path,filename,i,".txt"), stringsAsFactors=F, header = T, skip = 3)
}




areas_1 <- list (cdc2[[1]][,3],cdc2[[2]][,3], cdc2[[3]][,3],cdc2[[4]][,3],
								 cdc2[[5]][,3],cdc2[[6]][,3],cdc2[[7]][,3],cdc2[[8]][,3],
								 cdc2[[9]][,3],cdc2[[10]][,3], cdc2[[11]][,3], cdc2[[12]][,3])

medianas_1 <- lapply(FUN = median, areas_1)

oneDplot(areas_1, ylim = c(2,8), spread = 150, breaks = 50, col = "grey")
oneDplot(medianas_1, col = "red", add = T)
