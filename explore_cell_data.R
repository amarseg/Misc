library(lattice)
library(ggplot2)
library(gridExtra)

main_table <- read.delim('C:/Users/am4613/OneDrive - Imperial College London/ondedriveBACK/Summaries_as_timecourses/compilation_table.txt', header = T, strings = F)
isx_data <- read.delim('C:/Users/am4613/OneDrive - Imperial College London/ondedriveBACK/Summaries_as_timecourses/analysis/isx_data.txt', header = T, strings = F)
main_table$Area <- isx_data$Area_Erode.M03..4.

main_table$ProtLength <- main_table$ProteinPerCell/main_table$Area
main_table$RNALength <- main_table$RNAPerCell/main_table$Area
main_table$ratio <- main_table$RNAPerCell/main_table$ProteinPerCell

xyplot(ProteinPerCell ~ Time, data = main_table)

xyplot(ProteinPerCell ~ Length|Repeat, data = main_table)

xyplot(RNAPerCell ~ ProteinPerCell|Repeat, data = main_table)


g <- ggplot(main_table, aes(x = Time, y = Length, color = Repeat, group = Repeat))
g + geom_line(size = 1.5) + geom_point(size = 3) + theme(legend.position = 'none') + theme_bw()

p1 <- ggplot(main_table,aes(x = Time, y = RNAPerCell, color = Repeat, group = Repeat)) + geom_line(size = 1.5) + geom_point(size = 3) + theme(legend.position= 'none') + theme_bw()
p2 <- ggplot(main_table,aes(x = Time, y = ProteinPerCell, color = Repeat, group = Repeat)) + geom_line(size = 1.5) + geom_point(size = 3) + theme(legend.position= 'none') + theme_bw()
p3 <- ggplot(main_table,aes(x = Time, y = Area, color = Repeat, group = Repeat)) + geom_line(size = 1.5, show.legend = F) + geom_point(size = 3, show.legend = F) + theme(legend.position= 'none') + theme_bw()

svg(file = 'C:/Users/am4613/OneDrive - Imperial College London/ondedriveBACK/Summaries_as_timecourses/total_cell_data_summary.svg', paper = 'a4r')
grid.arrange(p1,p2,p3, ncol = 3)
p1
p2
p3
dev.off()

ggplot(main_table,aes(y = ProteinPerCell, x = Area, color = Repeat, group = Repeat)) + geom_line(size = 1.5, show.legend = F) + geom_point(size = 3, show.legend = F) + theme(legend.position= 'none')+ theme_bw()


p4 <- grid.arrange(p1,p2,p3, ncol = 3)

ggsave(p4, file = 'C:/Users/am4613/OneDrive - Imperial College London/ondedriveBACK/Summaries_as_timecourses/total_cell_data_summary.pdf', paper = 'a4r', device = 'pdf')


percell_area <-ggplot(main_table,aes(y = RNAPerCell, x = Area, color = Repeat, group = Repeat)) + geom_line(size = 1.5, show.legend = F) + geom_point(size = 3, show.legend = F) + theme(legend.position= 'none') + theme_bw()
ggsave(percell_area, file = 'C:/Users/am4613/OneDrive - Imperial College London/ondedriveBACK/Summaries_as_timecourses/rna_per_cell.pdf', paper = 'a4r', device = 'pdf')
ggsave(percell_area, file = 'C:/Users/am4613/OneDrive - Imperial College London/ondedriveBACK/Summaries_as_timecourses/rna_per_cell.wmf', device = 'wmf')


prot_per_cell <- ggplot(main_table,aes(y = ProteinPerCell, x = Area, color = Repeat, group = Repeat)) + geom_line(size = 1.5, show.legend = F) + geom_point(size = 3, show.legend = F) + theme(legend.position= 'none')+ theme_bw()
ggsave(prot_per_cell, file = 'C:/Users/am4613/OneDrive - Imperial College London/ondedriveBACK/Summaries_as_timecourses/prot_per_cell.pdf', paper = 'a4r', device = 'pdf')

