
source("C:/Users/am4613/Documents/R scripts/oneDplot.v4.r")

path = "P:/Time Courses/270215/"

cdc2 <- list()

filename <- "cdc2_as_2502_tp1-_"

for(i in 0:11)
{
	cdc2[[i+1]] <- read.delim(file=paste0(path,filename,i,"_batch.txt"), stringsAsFactors=F, header = T, skip = 3)
}




areas_1 <- list (cdc2[[1]][,3],cdc2[[2]][,3], cdc2[[3]][,3],cdc2[[4]][,3],
								 cdc2[[5]][,3],cdc2[[6]][,3],cdc2[[7]][,3],cdc2[[8]][,3],
								 cdc2[[9]][,3],cdc2[[10]][,3], cdc2[[11]][,3], cdc2[[12]][,3])

medianas_1 <- lapply(FUN = median, areas_1)

oneDplot(areas_1, col = "grey", ylim = c(3.5,6.5), spread = 125, breaks = 200)
oneDplot(medianas_1, col = "red", add = T)