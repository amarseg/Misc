




library("xlsx", lib.loc="C:/Users/am4613/Documents/R/win-library/3.0")
source("C:/Users/am4613/Documents/R scripts/oneDplot.v2.r")


data <- read.xlsx2("Z:/Lab_Data/Malika/Phenotyping analysis/Microscopy/Microscopy Set 1 data/summary.xlsx", 1,startRow = 1, header = F, 
                  as.data.frame=F, colClasses = rep("numeric",10))





oneDplot(data, ylim = c(2,6), spread = 50, breaks = 20)