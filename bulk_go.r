##Bulk GO analysis


load("c:/Users/am4613/Desktop/am4613/Rstuff/GO.analysis.110914.rda")


bulk_go <- function(pathway = "~", slim = FALSE)
{
	filenames <- list.files(path = pathway)
	
	if(slim == T)
	{	
		go = GOfinal
	}else{
	
		go = GOtable
	}
	
	for (i in 1:length(filenames))
	{
		ID_list <- read.delim(paste0(pathway,filenames[i]), stringsAsFactors = FALSE)
		GO_list <- GOanalysis(li = ID_list[,1], go = go, all = 5200, adjust = "fdr", sort = TRUE)
		names(GO_list) <- c("GO term", "pval", "#geneslist","#genesterm", "overlap") 
		write.table(subset(GO_list, GO_list[,2] <= 0.05),file = paste0(pathway, basename(filenames[i]), "_go.txt"), sep = "\t")
	}
}

