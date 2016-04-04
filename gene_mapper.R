allID2GeneName <- function(ids)
{
	ID_table <- read.delim('C:/Users/am4613/Documents/Summaries_as_timecourses/sysID2product.tsv', header = F, skip = 2, strings = F)
	colnames(ID_table) <- c('Systematic_ID', 'Primary_name','Secondary_name','Description')
	
	#Writing ID in primary name for genes that do not have one
	#I do this to make the assigment easier in the next step
	ID_table[ID_table$Primary_name == '',]$Primary_name <- ID_table[ID_table$Primary_name == '',]$Systematic_ID
	
	##Asignment
	name_table <- apply(ids,2,oneID2GeneName)
	
	return(name_table)
	
}

oneID2GeneName <- function(id, ID_table)
{
	if(ID_table$Systematic_ID %in% id)
		{
			ToDo <- ID_table[ID_table$Systematic_ID %in% id,]$Primary_name
	}else{
		ToDo <- NA
	}
	return(ToDo)
}