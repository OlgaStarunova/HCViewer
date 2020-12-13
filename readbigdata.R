data = read.csv('D:/medas/2016/базы/_грязная.csv_Pieces/_грязная_1.csv', header=TRUE, sep=';', dec=',', na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE, encoding = "UTF-8")
f=400000
data2 = read.csv('D:/medas/2016/базы/_грязная.csv_Pieces/_грязная_2.csv', header=TRUE, sep=';', dec=',', na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE, encoding = "UTF-8", skip = 0)
f=f+nrow(data2)

data3 = read.csv('D:/medas/2016/базы/_грязная.csv_Pieces/_грязная_3.csv', header=TRUE, sep=';', dec=',', na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE, encoding = "UTF-8", skip = 0)
f=f+nrow(data3)

data4 = read.csv('D:/medas/2016/базы/_грязная.csv_Pieces/_грязная_4.csv', header=TRUE, sep=';', dec=',', na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE, encoding = "UTF-8", skip = 0)
f=f+nrow(data3)

data5 = read.csv('D:/medas/2016/базы/_грязная.csv_Pieces/_грязная_5.csv', header=TRUE, sep=';', dec=',', na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE, encoding = "UTF-8", skip = 0)
f=f+nrow(data3)

data6 = read.csv('D:/medas/2016/базы/_грязная.csv_Pieces/_грязная_6.csv', header=TRUE, sep=';', dec=',', na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE, encoding = "UTF-8", skip = 0)
f=f+nrow(data6)

all = rbind(data[data$'Sex'=='М',], data2[data2$'Sex'=='М',], data3[data3$'Sex'=='М',],data4[data4$'Sex'=='М',],data5[data5$'Sex'=='М',],data6[data6$'Sex'=='М',])
all_fem = rbind(data[data$'Sex'=='Ж',], data2[data2$'Sex'=='Ж',], data3[data3$'Sex'=='Ж',],data4[data4$'Sex'=='Ж',],data5[data5$'Sex'=='Ж',],data6[data6$'Sex'=='Ж',])


write.table(all_fem, 'D:/medas/2016/базы/_грязная_Ж.csv', sep=';', row.names = FALSE, quote = FALSE, dec = ',')
