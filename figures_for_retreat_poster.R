##Figures for the poster
rm(list = ls())
library(ggplot2)
library(reshape2)
in_path <- 'C:/Users/am4613/Documents/Summaries_as_timecourses/'
out_path <- 'C:/Users/am4613/Documents/retreat_poster/'
##Violin plot of proteomics data
##Doesnt work very well :(
prot_data <- read.delim(paste0(in_path,'/analysis/cpc_proteins.txt'), header = T, strings = F)
melted_pdata <- melt(prot_data)


p <- ggplot(melted_pdata, aes(x = variable, y = value))
p + geom_violin(adjust = 0.25) + geom_jitter()
ggsave(paste0(out_path,'violin_plot.png'))

##Maybe dotplot with colours?
new_melted_pdata <- melt(prot_data, measure.vars = c(2:12))

p <- ggplot(new_melted_pdata, aes(x = T0, y = value, color = variable))
p + geom_point(alpha = 0.5) + xlab('log2(Number of copies at time point zero') + ylab('Log2(number of copies)')
ggsave(paste0(out_path,'dotplot.png'))
##Density plot with colours
p <- ggplot(melted_pdata, aes(value, color = variable, fill = variable))
p + geom_density(alpha = 0.25)
ggsave(paste0(out_path,'density_plot.png'))

##Barplot with total protein copies and comparison between tp and tp11
total_cpc <- colSums(2^prot_data)
m_total <- melt(total_cpc)
m_total$variable <- c(0:11)
c

p <- ggplot(m_total, aes(y = value, x = variable, fill = variable))
p + geom_bar(stat = 'identity') + xlab('Time') + ylab('Total number of proteins') + scale_fill_gradient2(low ='limegreen', mid = 'limegreen', high =  'darkgreen')
ggsave(paste0(out_path,'total_barplot.png'))

p <- ggplot(prot_data, aes(x = T0, y = T11), color = 'darkgreen')
p + geom_point(color = 'darkgreen') +geom_abline(slope = 1,intercept = 0, color = 'red') + xlab('log2(Copies per cell at time point 0)') + ylab('log2(Copies per cell at time point 11)')+ geom_abline(slope = 1, intercept =1 , color = 'red',linetype='dashed') + geom_abline(slope = 1,intercept =-1,color='red',linetype = 'dashed')

ggsave(paste0(out_path,'corr_dotplot.png')) 
##Intersection RNA and protein lists
prot_list <- read.delim(paste0(in_path,'protein_isx_correlation/negative_correlation_length.txt'), header = T, strings = F)
prot_list <- row.names(prot_list)

rna_list <- read.delim(paste0(in_path,'both_clustering/Cluster_2.txt'), header = T, strings = F)
rna_list <- rna_list[,1]
