



path = "P:/Time Courses/Amalia 090414/"

cdc2_1 <- list()

filename <- "cdc2_t_"

for(i in 0:11)
{
  cdc2_1[[i+1]] <- read.delim(file=paste0(path,filename,i,"_batch3.txt"), stringsAsFactors=F, header = T, skip = 3)
}


areas_1 <- list (cdc2[[1]][,3],cdc2[[2]][,3],cdc2[[3]][,3],cdc2[[4]][,3],
               cdc2[[5]][,3],cdc2[[6]][,3],cdc2[[7]][,3],cdc2[[8]][,3],
               cdc2[[9]][,3],cdc2[[10]][,3], cdc2[[11]][,3], cdc2[[12]][,3])

medianas_1 <- lapply(FUN = median, areas_1)


path = "P:/Time Courses/Amalia 060614/ISX files/"

cdc2_2 <- list()

filename <- "cdc2_2605__"

for(i in 0:11)
{
  cdc2_2[[i+1]] <- read.delim(file=paste0(path,filename,i,".txt"), stringsAsFactors=F, header = T, skip = 3)
}



areas_2 <- list (cdc2_2[[1]][,3],cdc2_2[[2]][,3],cdc2_2[[3]][,3],cdc2_2[[4]][,3],
                 cdc2_2[[5]][,3],cdc2_2[[6]][,3],cdc2_2[[7]][,3],cdc2_2[[8]][,3],
                 cdc2_2[[9]][,3],cdc2_2[[10]][,3],cdc2_2[[11]][,3],cdc2_2[[12]][,3])

medianas_2 <- sapply(FUN = mean, areas)

path = "P:/Time Courses/Amalia 040614/ISXfiles/"

cdc2_3 <- list()

filename <- "cdc2_3004__"

for(i in 0:11)
{
  cdc2_3[[i+1]] <- read.delim(file=paste0(path,filename,i,"_new.txt"), stringsAsFactors=F, header = T, skip = 3)
}



areas_3 <- list (cdc2_3[[1]][,3],cdc2_3[[2]][,3],cdc2_3[[3]][,3],cdc2_3[[4]][,3],
                 cdc2_3[[5]][,3],cdc2_3[[6]][,3],cdc2_3[[7]][,3],cdc2_3[[8]][,3],
                 cdc2_3[[9]][,3],cdc2_3[[10]][,3],cdc2_3[[11]][,3],cdc2_3[[12]][,3])

medianas_3 <- sapply(FUN = mean, areas)

source("C:/Users//am4613/Documents/R scripts/oneDplot.v2.r")

#par(mfrow = c(1,3))

oneDplot(areas_1, col = "grey", ylim = c(3,6), spread = 125, breaks = 200)
oneDplot(medianas_1, col = "red", add = T)
oneDplot(areas_2, col = "grey", ylim = c(3,6), spread = 100, breaks = 75)
oneDplot(areas_3, col = "grey", ylim = c(3,6), spread = 100, breaks = 75)


