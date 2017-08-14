zhu_data <- read.delim('C:/Users/am4613/Desktop/ZHURINSKY_NORMALISED_DATA.txt', header = T, strings = F)
wee1_cdc25 <- zhu_data[,8:11]
row.names(wee1_cdc25) <- zhu_data[,1]
wee1_cdc25 <- wee1_cdc25[-1:-3,]
wee1_cdc25 <- as.data.frame(apply(wee1_cdc25, c(1,2), as.numeric))

wee1_cdc25$avg_wee1 <- rowMeans(cbind(wee1_cdc25$SW_1413.24_wee1, wee1_cdc25$SW_1413.22_wee1.ds), na.rm = T)*2.03
wee1_cdc25$avg_cdc25 <- rowMeans(cbind(wee1_cdc25$SW_1413.23_cdc25, wee1_cdc25$SW_1354.24_cdc25.ds), na.rm = T)*5.04

wee1_cdc25$ratio <- wee1_cdc25$avg_wee1/wee1_cdc25$avg_cdc25

t<- wee1_cdc25[which(wee1_cdc25$ratio > 0.90 & wee1_cdc25$ratio < 1.3),]

write.table(t, file = 'C:/Users/am4613/Desktop/wee1_cdc25_list.txt', sep = '\t')

