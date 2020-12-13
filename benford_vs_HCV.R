library(plotly)
library(zoo)
library(benford.analysis)
all.data =   as.data.frame(read.csv('D:/RNF20-15-00386/1 декабря-базы/all_marked.csv', header=TRUE, sep=';', dec=',', 
                                    quote='\"', na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE, skipNul = TRUE, encoding = 'utf-8'))

#половозрастная структура
nrow(all.data[(all.data$Age.full.years>18)&(all.data$Sex=='М'),])
nrow(all.data[all.data$Sex=='Ж',])

obs=c()
ym=c()
for (i in c(1:max(all.data$ObsMonth)))
{     
    obs[i] = nrow(all.data[all.data$ObsMonth == i,])
    ym[i] = (as.yearmon(2010 + (i-1)/12))
}

# распределение данных по месяцам
fig = plot_ly(x=~ym, y = ~obs, type='scatter', mode = 'lines+markers')
fig = fig %>% layout(yaxis = list(title = "Количество измерений за месяц"), xaxis = list(title = "Годы"))
fig


head(all.data)
####################################################
rep =   as.data.frame(read.csv('D:/RNF20-15-00386/1 декабря-базы/deleted_summary.csv', header=TRUE, sep=';', dec='.', 
                                quote='\"', na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE, skipNul = TRUE, encoding = 'utf-8'))
#убираем первую строку с итоговым результатом по Москве
rep=rep[-1,]

#складываем данные в одном ЛПУ за все годы
#short = aggregate(cbind(rep[[5]], rep[[6]], rep[[8]], rep[[10]], rep[[12]], rep[[14]], rep[[16]], rep[[18]], rep[[20]], rep[[22]], rep[[24]], rep[[26]], rep[[28]], rep[[30]], rep[[32]], rep[[34]]), by=list(Category=rep$LPU), FUN=sum)
short = aggregate(cbind(rep[[5]], rep[[22]] + rep[[24]], rep[[30]], rep[[32]], rep[[34]]), by=list(Category=rep$LPU), FUN=sum)
#добавляем названия адм.округов
regs = c()
for (i in c(1:nrow(short)))
{
  regs[i] = rep[rep$LPU == short[i, 1], "FO"][1]
}

X_Emul =   round(short[,3]/short[,2]*100, 0)
X_Out = round(short[,4]/short[,2]*100, 0)
X_Clones = round(short[,5]/short[,2]*100, 0)
X_Incorr = round(short[,6]/short[,2]*100, 0)
X_EO= pmax(X_Emul, X_Out)
#генерируем таблицу
short = data.frame(short[,1], regs, short[,2], X_EO, X_Clones, X_Incorr)
names(short)= c('LPU','CityDist', 'Total', 'X_Emul_Out', 'X_Clones', 'X_Incorrect')

#оставляем только те ЛПУ, где больше 1000 пациентов и сортируем
short = short[short$'Total' >= 1000,]
short = short[order(short$X_Incorrect),]
short[short$LPU=='ГП 115 ДЗМ', 'X_Emul_Out'] = round(2261/2530*100)

#short

benf = c()
benf_conf = c()
distf = c()
for(i in c(1:nrow(short)))
{
  lpu_name = short$LPU[i]
  y=benford(all.data[all.data$Place == lpu_name, 'R50']^10)
  benf[i] = round(y$MAD, 3)
  benf_conf[i] = y$MAD.conformity
  distf[i] = y$distortion.factor
}  

benf_conf[benf_conf == 'Acceptable conformity'] = 'Достаточное соответствие'
benf_conf[benf_conf == 'Close conformity'] = 'Близкое cоответствие'
benf_conf[benf_conf == 'Nonconformity'] = 'Неcоответствие'
benf_conf[benf_conf == 'Marginally acceptable conformity'] = 'Минимально приемлемое cоответствие'

short = cbind(short, benf, benf_conf, distf)
names(short) = c('LPU', 'CityDist',"Total",'X_Emul_Out', 'X_Clones','X_Incorrect', 'MAD', "MADconf", 'DistFact')


#Close conformity — 0.000 to 0.004
#Acceptable conformity — 0.004 to 0.008
#Marginally acceptable conformity — 0.008 to 0.012
#Nonconformity — greater than 0.012
fig = plot_ly(data = short, x= ~X_Incorrect, y = ~MAD, text = ~LPU, color = ~benf_conf, size = ~Total, colors = c( "darkgreen", "green", "orange", 'red'))
fig = fig %>% layout(yaxis = list(title = "Среднее абсолютное отклонение\nот распределения Бенфорда"), xaxis = list(title = "Процент некорректных измерений в ЦЗ"))
fig <- fig %>% layout(legend = list(orientation = 'h', yanchor="bottom",
                                    y=1.02,
                                    xanchor="right",
                                    x=1))
fig
cor(short$X_Incorrect, short$MAD)

x = all.data[all.data$Place == 'ГП 11 ДЗМ филиал 1', ]
y=benford(all.data[all.data$Place == 'ГП 8 ДЗМ филиал 3', 'R50']^10)
plot(y)
x = x[order(x$RTime),]
x=x[16000:20000,]
plot(x$R50)
z = x[x$'R50'==556,]
hist(z, 400)

hist(x[x$Xc50<100,'Xc50'],350)


hist(x[x$R50==381, 'Xc50'], 250)
plot(x$R50)#,  ylim=c(35, 100))
