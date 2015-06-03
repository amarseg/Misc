#Flow cytometry analysis
library("flowCore")
library("flowViz")

path = "P:/Flow_cyt/090415/Stained/"

setwd(path)

filenames <- list.files(path)

fcs <- grep(pattern = ".pdf", x = filenames, value = T, fixed = T, invert = T)

flow_data <- list()

for(i in 1:length(fcs))
{
	flow_data[[i]] <- read.FCS(paste0(path, fcs[i]), transformation = FALSE)##Always produces warnings <- Ignore them
}
	
col = rainbow(12, alpha =0.5)

plot(flow_data[[1]], "525/50 B-W", breaks = 256, col = col[1])

for(i in 2:12)
{
	plot(flow_data[[i]], "525/50 B-W", breaks = 256, col = col[i], add = T)
}

for(i in 1:12)
{
	pdf(file = paste0(filenames[i],"_2.pdf"))
	plot(flow_data[[i]], c("525/50 B-W", "525/50 B-H"), main = filenames[i])
	dev.off()
}
