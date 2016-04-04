##Preparing cell cycle database 
rm(list = ls())
setwd('C:/Users/am4613/Documents/Summaries_as_timecourses/fission_timecourses/')
library(plyr)

exp_splitter <- function(df, tosplit)
{
	exp_data <- df[,tosplit]
	#Remove curly brackets
	todo <- gsub(exp_data,pattern = '\\{|\\}',replacement = '')
	#Split strings 
	sep_values <- strsplit(todo, split =  ',')
	sep_values <- data.frame(t(sapply(sep_values, c)))
	
	out <- cbind(df[,1:(tosplit - 1)], sep_values)
	return(out)
}


exp_levels <- read.delim('fission_experiments.tsv', header = T, strings = F)
colnames(exp_levels)[4] <- 'Expression'
colnames(exp_levels)[1] <- 'Experiment'

exp_id <- unique(exp_levels$Experiment)

times <- read.delim('fission_metadata.tsv', header = T, strings = F)
colnames(time)[3] <- 'x-values'

a <- gsub(times[,3], patter = '\\{|\\}', replacement = '')
a <- strsplit(a, split = ',')

time_list <- a
names(time_list) <- times[,1]

exp_list <- split(exp_levels, exp_levels[,1])

exp_list <- lapply(exp_list, exp_splitter, tosplit = 4)
names(exp_list) <- exp_id

save(exp_list, file = 'cell_cycle_data.rda')
save(time_list,file = 'exp_time.rda')

