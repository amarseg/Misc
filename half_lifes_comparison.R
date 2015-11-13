load(file = 'C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/second_der_rna.rda')
load(file = 'C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/first_der_rna.rda')

rna_lives <- read.delim('C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/rna_half_lives.txt', header = T, skip = 6, strings = F)

##only CDS

first_lives <- merge(rna_lives, sol_firstdf, by.y = 'row.names', by.x = 'Gene.name')
second_lives <- merge(rna_lives, sol_seconddf, by.y = 'row.names', by.x = 'Gene.name')


par(mfrow = c(1,1))
plot(x = second_lives$Average.half.life, y = second_lives[,13], log = 'xy')

plot(x = first_lives$Average.half.life, y = first_lives[,13], log = 'xy')
points(x = first_lives$Average.half.life, y = first_lives[,14], col = 'blue')


first_prot <- read.delim('C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/first_sol_prot.txt', strings = F)
second_prot <- read.delim('C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/second_sol_prot.txt', strings = F)
prot_hl <- read.delim('C:/Users/am4613/Documents/Summaries_as_timecourses/analysis/proteins_half_lives.txt', strings = F, header = T)

pfirst_lives <- merge(first_prot, prot_hl, by.x = 'row.names', by.y = 'ENSG')
psecond_lives <- merge(second_prot, prot_hl, by.x = 'row.names', by.y = 'ENSG')

plot(y = psecond_lives[,2], x = psecond_lives$t1.2..hours., log = 'xy')

plot(y = pfirst_lives[,2], x = pfirst_lives$t1.2..hours., log = 'xy')
points(y = pfirst_lives[,3], x = pfirst_lives$t1.2..hours., col = 'blue')

