


data <- list()

for(i in 1:2)
{
  data[[i]] <- read.xlsx("Z:/Lab_Data/ImageStream/Dividing cells S1 data.xlsx", sheetIndex = i, header = T)
}

area <- list(data[[1]][,1],data[[1]][,2],data[[1]][,3],data[[1]][,4],data[[1]][,5],data[[1]][,6],data[[1]][,7],data[[1]][,8])

long <- list(data[[2]][,1],data[[2]][,2],data[[2]][,3],data[[2]][,4],data[[2]][,5],data[[2]][,6],data[[2]][,7],data[[2]][,8])

source("C:/Users/am4613/Documents/R scripts/oneDplot.v2.r")

oneDplot(area, col = "darkblue", spread = 200, ylim = c(4,6.5))
media <- lapply(area, median)
oneDplot(media, col = "red", spread = 200, ylim = c(4,6.5), add = T)


oneDplot(long, col = "darkblue", spread = 5, ylim = c(2,4.5))