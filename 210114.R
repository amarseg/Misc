#Calculates acetylation and methylation ratio of the different peptides
path = "C:/Users//am4613/Desktop/"

mod_pep <- read.delim(paste0(path,"acetyl.txt"), header = T, strings = F)
unmod_pep <- read.delim(paste0(path, "unmod.txt"), header = T, strings = F)

j = 1

results_mod <- data.frame(Accesion = character(), Description = character(), Sequence = character(), Modifications = character(), )

for(i in nrow(unmod_pep))
{
  ToDo <- mod_pep[grep(unmod_pep[i,3], mod_pep[,3], value = F),]
   if(is.numeric(ToDo))
    {
     result_mod[j,1:4] <- ToDo[1,1:4]
     result_mod[j,5:16] <- ToDo[1,5:16]/unmod_pep[i,5:16]
     j <- j+1
    }
 
}
   
  
library("gplots")

heatmap.2(as.matrix(result_mod[,3:15]))
