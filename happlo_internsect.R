rm(list = ls())


setwd('C:/Users/am4613/OneDrive/Summaries_as_timecourses/haplo_insu_pro/')

insu <- read.delim('haploinsuficient.txt', strings = F)
profi <- read.delim('haploproficient.txt', strings = F)

rna <- read.delim('rna_not_scaling.txt', header = T)
rna <- rna[,1:9]

prot <- read.delim('protein_not_scaling.txt', header = T)
prot <- prot[,1:9]

ribo <- read.delim('../analysis/ribosome&biogenesisGenes', header = T, strings = F)
ribo_ids <- ribo$ensembl_id

ribo_insu <- intersect(ribo_ids, insu[,1])

ribo_pro <- intersect(ribo_ids, profi[,1])
ribo_insu <- intersect(ribo_ids, insu[,1])

intersect(rna$systematic_id, ribo_insu)

intersect(prot$systematic_id, ribo_insu)

intersect(rna$systematic_id, ribo_pro)

intersect(prot$systematic_id, ribo_pro)
