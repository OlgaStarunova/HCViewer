library(shiny)
library(ggplot2)
#Пакеты R, которые нам пригодятся для рисования карт
library(spdep)
library(sp)
library(leaflet)
library(maptools)
library(RColorBrewer)
library(benford.analysis)
library(plyr)
library(reshape)
library(plotly)
library(gamlss)
library(R.utils)
source("centiles.with.report.table.R")	

#устанавливаем мак размер входного файла 10 Гб
if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

#устанавливаем кодировку
#Sys.setlocale(,"russian")
Sys.setlocale("LC_ALL", "Russian")

shinyServer(function(input, output) {
  
  output.folder <- reactive({

    #if (input$Sex == 'М') 
    #    output.folder = paste0("../", as.character(format(Sys.time(), "%F %H-%M-%S ")), "М")
    #else if (input$Sex == 'Ж') 
    #  output.folder = paste0("../", as.character(format(Sys.time(), "%F %H-%M-%S ")), "Ж")
    output.folder =  paste0("../", as.character(format(Sys.time(), "%F %H-%M-%S")))
    
    dir.create(output.folder)
    output.folder
  })
  
  ###############################
  #    LOAD RAW DATA
  ##############################
  
  data <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=TRUE, sep=input$sep, dec=input$dec, 
             quote=input$quote, na.strings=c("NULL", "NA", ""), stringsAsFactors = FALSE)
    
    
   
    
  })
  
  ########################################################
  # TRY TO GUESS SOME COLUMN NAMES
  ########################################################
  sex <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    for(i in c(1:200))
      for(j in c(1:ncol(data())))
        if(data()[i, j] %in% c("М","Ж"))
        {
          return(names(data()[j]))
        }
    
    return("Sex")
  }) 
  
  fo <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    for(i in c(1:200))
      for(j in c(1:ncol(data())))
        if(data()[i, j] %in% c("ДВФО","ЦФО","УрФО","СФО","СКФО","CЗФО","ПФО","ЮФО"))
          return(names(data()[j]))
    return("FO")
  }) 
  
  reg <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    for(i in c(1:200))
      for(j in c(1:ncol(data())))
        if(strsplit(as.character(data()[i, j]), " ")[[1]][2] %in% c("регион", "республика","область","край") | strsplit(as.character(data()[i, j]), " ")[[1]][1] %in% c("Москва", "Санкт-Петербург","С.-Петербург","Севастополь", "С.-П."))  
        {
          return(names(data()[j]))
        } 
    
    return("Regions")
  })  
  
  city <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    for(i in c(1:200))
      for(j in c(1:ncol(data())))
        if(data()[i, j] %in% c("СВАО", "ЗАО","ЮВАО","ВАО"))  
        {
          return(names(data()[j]))
        } 
    
    return("City.District")
  }) 
  
  lpu <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    for(i in c(1:200))
      for(j in c(1:ncol(data())))
        if(strsplit(as.character(data()[i, j]), " ")[[1]][1] %in% c("КГБУЗ",	"ГБУЗ",	"УЗ", "ГУЗ",	"МБУЗ",	"ОГКУЗ",	"ОГБУЗ",	"ГАУЗ",	"БУЗ",	"МУЗ",	"ОБУЗ",	"ОГАУЗ",	"МАУЗ",	"ГАУ",	"МБЛПУ",	"КОГБУЗ", "МБУ",	"ГБУ",	"МНПЦ",	"ФГБУ",	"МЛПУ",	"ГОБУЗ",	"ГОАУЗ",	"БУЗОО",	"МУ",	"ГУ",	"БУ",	"РГБЛПУ",	"МЛПУЗ",	"ГБОУ",	"ООО",	"ЛО",	"МАУ",	"ПМБУЗ",	"ГЛПУ",	"ММЛПУ","\"КГБУЗ",	"\"ГБУЗ", "\"ГУЗ",	"\"МБУЗ",	"\"ОГКУЗ",	"\"ОГБУЗ",	"\"ГАУЗ",	"\"БУЗ",	"\"МУЗ",	"\"ОБУЗ",	"\"ОГАУЗ",	"\"МАУЗ",	"\"ГАУ",	"\"МБЛПУ",	"\"КОГБУЗ",	"\"МБУ",	"\"ГБУ",	"\"МНПЦ",	"\"ФГБУ",	"\"МЛПУ", "\"ГОБУЗ",	"\"ГОАУЗ",	"\"БУЗОО",	"\"МУ",	"\"ГУ",	"\"БУ",	"\"РГБЛПУ",	"\"МЛПУЗ",	"\"ГБОУ",	"\"ООО",	"\"ЛО",	"\"МАУ",	"\"ПМБУЗ",	"\"ГЛПУ",	"\"ММЛПУ"))
        {
          return(names(data()[j]))
        } 
    
    return("Place")
  }) 
  
  time <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    # for(i in c(1:200))
    #   for(j in c(1:ncol(data())))
    #     if(":" %in% strsplit(as.character(data()[i, j]), ""))
    #     {
    #       return(names(data()[j]))
    #     } 
    
    return("RTime")
  }) 
  
  output$dataColNames <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    
    fluidPage(
      helpText("Уточните названия столбцов базы, в которых указаны:"),
      fluidRow(
        column(6, textInput("SexColName", label = "Пол", value = sex())),
        column(6, textInput("AgeColName", label = "Возраст", value = "Age"))
      ),
      fluidRow(
        column(6, textInput('FOColName', "Фед. округ", value = fo())),
        column(6, textInput('RegColName', "Субъект РФ", reg()))
      ),
      fluidRow(
        column(6, textInput('CityColName', "Город/Адм.округ", city())),
        column(6, textInput('LPUColName', "Название ЛПУ", lpu())),
       ),
      fluidRow(
        column(6, textInput('TimeColName', "Дата и время обследования", time()))
      )# of fluid row
    )#of fluidPage
  })
  #Федеральный округ -> субъект РФ -> Регион(город/административный округ) -> субрегион (муниципалитет).
  ##########################################
  # FILTER DATA BY AGE, SEX and OBSERVATION YEAR
  #########################################
  
  dataFirstFilter <-reactive({
    if (is.null(input$file1))
      return(NULL)
    if(is.null(input$SexColName) | is.null(input$AgeColName))
      return(NULL)
    all.data = data()
    
    #print(all.data[[input$AgeColName]])
    #print(all.data[[input$SexColName]])
    #print(all.data[['RTime']])
   # # выкидываем все строки, где нет пола, возраста и R50
    all.data = all.data[complete.cases(all.data[[input$AgeColName]]) & complete.cases(all.data[[input$SexColName]]) & complete.cases(all.data[['R50']]) & complete.cases(all.data[['Xc50']]),]
    # фильтруем по полу, 
    all.data = all.data[(all.data[[input$SexColName]] %in% input$Sex), ]
    #затем фильтруем по возрасту
    all.data = all.data[(all.data[[input$AgeColName]] >= input$Age[1])&(all.data[[input$AgeColName]] <= input$Age[2]) ,]
    
    #'Приводим все даты измерений к стандартному виду
    
    if((nrow(all.data) != 0) & (!input$isDateFormat))
    {
         progress <- shiny::Progress$new(min = 1, max = (nrow(all.data) %/% 10000))
         on.exit(progress$close()) 
         progress$set(message = 'Приводим все даты измерений к стандартному виду\n')
         
         all.data$"Date.meas" = as.Date(all.data$"Date.meas", "%d.%m.%Y")
         all.data$"RTime" = paste(all.data$"Date.meas", all.data$Time.meas) 
         all.data$"ObsYear" =  format(all.data$"Date.meas", "%Y")
         #номер месяца (считаем с января 2010, т.е. январь 2010 - это 1, январь 2011 - это 13)
         all.data$ObsMonth = as.numeric(format(all.data$"Date.meas", "%m")) + 12*(as.numeric(all.data$"ObsYear") - 2010)
         
    # 
    #     # приводим все даты к единому виду
    #     for(i in c(1:nrow(all.data)))
    #     {
    #   
    #       if (is.na(as.Date(all.data[[input$TimeColName]][i],format = '%Y-%m-%d')))
    #       {
    #           d = strsplit(all.data[[input$TimeColName]][i], ' ')
    #           dd = strsplit(d[[1]][1],'\\.')
    #     
    #           all.data[[input$TimeColName]][i] = paste0(dd[[1]][3],'-',dd[[1]][2], '-', dd[[1]][1], ' ', d[[1]][2],':00')
    #       }
    #       if (i%%10000 == 0)
    #         progress$set(value = i %/% 10000, detail = paste( "Измерение №", i))
    #     }
         # затем фильтруем по году измерения
    #     all.data = all.data[(format(as.POSIXlt(all.data[[input$TimeColName]]), '%Y') %in% input$ObsYear),]
    }    
     
    if(nrow(all.data) == 0)
      return(NULL)
    else
    {  
      if (is.null(all.data$'deletedA'))#если столбца с разметкой - кодами удаления еще нет в базе (т.е.база еще не размечена)
          all.data$'deletedA' = rep(0)
      if (is.null(all.data$'ObsYear'))#если столбца с годами нет
         all.data$'ObsYear' = rep(2010)
      if (is.null(all.data$'ObsMonth'))#если столбца с годами нет
        all.data$'ObsYear' = rep(1)
      if (is.null(all.data$'ObsDate'))#если столбца с месяцами измерений нет
        all.data$'ObsDate' = rep(1)
      
    }
    
    all.data
  })
  
  output$shortdata <- renderDataTable({
    if (is.null(input$file1))
      return(NULL)
    if (input$"AlreadySummary" == TRUE)
    {
      return(data())
    }  
    dataFirstFilter()
    
    
  })
  
  #####################################
  # PRINT SIZE OF BASE FILERED DATA
  ##################################### 
  
  output$alldatasize <- renderPrint({ 
    if (is.null(input$file1))
      return(NULL)
    nrow(dataFirstFilter()) 
  })
  
  #####################################
  # РАСПРЕДЕЛЕНИЕ ПО ВОЗРАСТАМ В ИСХОДНОЙ БАЗЕ ДАННЫХ
  ##################################### 
  
  output$AgeHist <- renderPlotly({
    if (is.null(input$file1))
      return(NULL)
 
    p = plot_ly(x=dataFirstFilter()[[input$AgeColName]], nbinsx =input$AgeBins, type= "histogram", marker = list(color = '#192BC2', line = list(color = "#D9D9D9",width = 1.3)))
    p = layout(p, title = "Возрастная структура", xaxis = list(title="Возраст, лет"), yaxis = list(title="Количество записей"))
    p
    
   # jpeg(paste0(output.folder(),"/AgeDistrHist.png"))
   #  hist(x=dataFirstFilter()[[input$AgeColName]], breaks =input$AgeBins, main="Распределение по возрастам", xlab = "Возраст, лет", ylab = "Количество записей")
   #  dev.off()
 })
  
  #############################################################
  # ИМЕНА СТОЛБЦОВ ИСХОДНОЙ БАЗЫ ДАННЫХ - интерфейс и список
  ############################################################# 
  
  output$ParametersNames <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    checkboxGroupInput("ParametersNames", "Выберите интересующие параметры:", choices=datanames(), selected = "R50")
  })
  
  datanames <- reactive({
    if (is.null(input$file1))
      return(NULL)
    names(data())[1:length(names(data()))-1]
  })
  
  #####################################################
  # ИМЕНА ФЕДЕРАЛЬНЫХ ОКРУГОВ В ИСХОДНОЙ БАЗЕ ДАННЫХ
  ##################################################### 
  
  FOs <- reactive({
    if (is.null(input$file1))
      return(NULL)
    unique(as.character(dataFirstFilter()[[input$FOColName]]))
  })
  
  ###############################################################
  # ИМЕНА РЕГИОНОВ В ИСХОДНОЙ БАЗЕ ДАННЫХ - интерфейс и список
  ############################################################### 
  
  output$Regions <- renderUI({
    if (is.null(input$file1))
      return(NULL) 
    selectInput('Regions', 'Выберите регион(ы)', regs(), multiple=TRUE, selectize=TRUE)
    
  })
  
  regs <- reactive({
    unique(as.character(dataFirstFilter()[[input$RegColName]]))
  })
  
  ###############################################################
  # ИМЕНА Городов/адм.округов В ИСХОДНОЙ БАЗЕ ДАННЫХ - интерфейс и список
  ############################################################### 
  
  output$Cities <- renderUI({
    if (is.null(input$file1))
      return(NULL) 
    selectInput('Cities', 'Выберите города/адм.округа', cities(), multiple=TRUE, selectize=TRUE)
    
  })
  
  cities <- reactive({
    unique(as.character(dataFirstFilter()[[input$CityColName]]))
  })
  
  #####################################
  # НАЗВАНИЯ ЛПУ В ВЫБРАННЫХ РЕГИОНАХ
  ##################################### 
  
  output$HCname <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    lpu = c()
    for(reg in input$Regions)
    {
      lpu = c(lpu, unique(as.character(dataFirstFilter()[dataFirstFilter()[[input$RegColName]]== reg, input$LPUColName])))
    } 
    
    selectInput('HCname', 'Выберите ЛПУ (по умолчанию все ЛПУ считаются выбранными)', lpu, multiple=TRUE, selectize=TRUE)
    
  })
  
  ####################################################################
  # ОТБИРАЕМ ИЗ БАЗЫ СТОЛБЦЫ И СТРОКИ для указанных параметров и ЦЗ
  ####################################################################
  
  chosendata = reactive({
    if (is.null(input$file1))
      return(NULL)
    reg = input$Regions
    lpu = input$HCname
    parameter = input$ParametersNames
    
    if(is.null(reg))
      chosendata = filtered.data.A()[, c(input$AgeColName, parameter, "deletedA")]
    else if (is.null(lpu))
      chosendata = filtered.data.A()[filtered.data.A()[[input$RegColName]] %in% reg, c(input$AgeColName, parameter, "deletedA")]
    else
      chosendata = filtered.data.A()[(filtered.data.A()[[input$RegColName]] %in% reg)&(filtered.data.A()[[input$LPUColName]] %in% lpu), c(input$AgeColName, parameter, "deletedA")]
    
  
    chosendata
  })
  
  ############################################################################
  # РАЗМЕР ОТОБРАННОЙ БАЗЫ ДАННЫХ (С КОНКРЕТНЫМИ ЦЗ) - до и после фильтрации
  ############################################################################ 
  
  output$chosendatasize.all <- renderPrint({
    if (is.null(input$file1))
      return(0)
    nrow(chosendata())
  })
  
  output$chosendatasize.filteredA <- renderPrint({
    if (is.null(input$file1))
      return(0)
    nrow(chosendata()[chosendata()$deletedA == 0,])
  })
  
  #################################################################
  # Summary для указанных параметров и ЦЗ - до и после фильтрации
  ################################################################# 
  
  output$Summary.all <- renderTable({
    if (is.null(input$file1))
      return(NULL)
    #для всех пациентов выбранных цз
    if(!is.null(chosendata()))
    {
      cd = chosendata()[,1:ncol(chosendata()) - 1]
      summary(cd)
    }
    
  })
  
  output$Summary.filtered <- renderTable({
    if (is.null(input$file1))
      return(NULL)
    #для всех пациентов выбранных цз
    if(!is.null(chosendata()))
    {
      cd = chosendata()[chosendata()$deletedA == 0, 1:ncol(chosendata()) - 1]
      summary(cd)
    }
    
  })
  
  ########################################################
  # СТРОИМ 2D-гистограммы для указанных параметров и ЦЗ
  # !!! РИСУНКИ В ПАПКУ ПОКА НЕ СОХРАНЯЮТСЯ !!!
  ######################################################## 
  output$plot2D <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    
    #для всех пациентов выбранных цз
    parameters = input$ParametersNames
    
    lapply(parameters, function(par)
    { 
      if(is.numeric(chosendata()[, par]))
      {
        output[[par]]<-renderPlot({
          
          #jpeg(paste0(output.folder(),"/", par, "HistBeforeAndAfter.png"))
          
          par(mfrow=c(1,3))
          # гистограммы для выбр параметров для всех пациентов
          hist(chosendata()[, par], breaks = input[[paste0("slider",par)]], xlab = "Все", ylab = "Количество записей", main = par, col = 2)
          
          # гистограммы для выбр параметров БЕЗ НЕКОРРЕКТНЫХ
          hist(chosendata()[chosendata()$deletedA <= 1+2+4+8+16, par], breaks = input[[paste0("slider",par)]], xlab = "Без некорректных", ylab = "Количество записей", main = par, col = 5, add = F)
          
          # гистограммы для выбр параметров БЕЗ НЕКОРРЕКТНЫХ и БЕЗ ПОВТОРОВ
          hist(chosendata()[chosendata()$deletedA == 0, par], breaks = input[[paste0("slider",par)]], xlab = "Без некорректных и повторов", ylab = "Количество записей", main = par, col = 3, add = F)
          #dev.off()
        })
        fluidPage(
          plotOutput(par), 
          sliderInput(paste0("slider",par), "Количество интервалов",min = 3, max = 100, value = 80)
        )
      }#of if
    }
    )
  })
  
  ########################################################
  # ОПРЕДЕЛЯЕМ, КАКУЮ ФИЛЬТРАЦИЮ ХОТИМ
  ######################################################## 
  
  want <- reactive({
    want.limits.filter = TRUE
    want.clones.filter = TRUE
    
    
    if(is.null(input$fileLimits))# если файл с границами не задан
      want.limits.filter = FALSE
    
    if(input$LimitsFilter == FALSE)# если граничную фильтрацию не хотим
      want.limits.filter = FALSE
    
    if(!is.null(input$fileLimits))
      if(is.null(LimitsFilterNamesList()$data.par.names)) #если никакие параметры для фильтрации не выбраны
        want.limits.filter = FALSE
    
    
    if(input$ClonesFilter == FALSE)# если фильтрацию повторов не хотим
      want.clones.filter = FALSE
    
    
    #файл с границами должен содержать диапазон возрастов input$Age[1]:input$Age[2].
    #Проверим это:
    if(!is.null(input$fileLimits) & want.limits.filter == TRUE)
      if ((limits()[1,1]>input$Age[1]) | (limits()[nrow(limits()),1]<input$Age[2]))
      {
        print("файл с границами не содержит выбранного диапазона возрастов")
        print(paste("файл содержит граничные значения:[",limits()[1,1], ";",limits()[nrow(limits()),1], 
                    "],а выбранный диапазон возрастов равен:[",input$Age[1], ";",input$Age[2], "]"))
        want.limits.filter = FALSE 
      }
    
    
    want = data.frame(want.limits.filter, want.clones.filter)
    names(want) = c("limits.filter", "clones.filter")
    return(want) 
    
  })
  
  ########################################################
  # СЧИТЫВАЕМ ФАЙЛ С ГРАНИЦАМИ
  ######################################################## 
  
  limits = reactive({
    inFile <- input$fileLimits
    
    if (is.null(inFile)) # считываем границы по умолчанию
      read.csv('границы общие (дети c LMS).csv', header=input$headerLimits, sep=input$sepLimits, dec=input$decLimits, 
               quote=input$quoteLimits)
    
      
    read.csv(inFile$datapath, header=input$headerLimits, sep=input$sepLimits, dec=input$decLimits, 
             quote=input$quoteLimits)
    
    
  })
  
  output$TableLims <- renderDataTable({
    
    limits()
    
    
  })
  
  ########################################################
  # КАКИЕ ГРАНИЦЫ ХОТИМ РИСОВАТЬ НА ГРАФИКЕ?
  ########################################################
  
  output$showlimits <- renderUI({
    if (is.null(input$fileLimits))
      return(NULL)
    checkboxGroupInput("showlimits", "Для каких параметров нарисовать границы", names(limits())[2:ncol(limits())], selected = names(limits())[2:ncol(limits())])
    
  }) 
  
  ########################################################
  # РИСУЕМ КАРТИНКУ С ГРАНИЦАМИ
  ######################################################## 
  
  output$plotLims <- renderPlot({
    
    
    if (is.null(input$fileLimits))
      return(NULL) 
    
    ltys=c() #стили линий
    cols = c() # цвета линий
    
    plot(1,xlim = c(input$AgeLims[1], input$AgeLims[2]), ylim = c(input$ParametersLims[1], input$ParametersLims[2]), type="l", xlab = "Возраст", ylab = "Значения параметров")
    palette("default") 
    if (ncol(limits()) > 1) #если параметров больше нуля, т е в файле больше 1 колонки
    {
      i = 0
      for (limname in input$showlimits)# для всех выбранных параметров
      {
        
        i = i + 1
        if (i %% 2 == 1)# если выбран пар.мин, устанавливаем одинаковый стиль линии для мин и для макс
        {
          ltys[i] = i
          ltys[i+1] = i
          cols[i] = trunc((i+1)/2)
          cols[i+1] = trunc((i+1)/2)
        }
        
        lines(limits()[[1]], limits()[[limname]], type="l", col=cols[i], lty = ltys[i])
        
      } 
    }
    if((input$LegendLims == TRUE) & (length(input$showlimits) > 1))
    { 
      legend(input$AgeLims[2]-20, input$ParametersLims[2], input$showlimits, lty = ltys, col=cols) 
    }
  })
  
  #####################################################################
  # ИМЕНА ПАРАМЕТРОВ ДЛЯ ФИЛЬТРАЦИИ ПО ГРАНИЦАМ - интерфейс и список
  #####################################################################
  
  output$LimitsFilterNames <- renderUI({
    if(is.null(input$fileLimits))# если файл с границами не пустой
      return(NULL)
    lapply(seq(from=1, to=ncol(limits())-1, by = 2), function(i) {
      
      nl = names(limits())[2:ncol(limits())]
      fluidRow(
        column(4, checkboxInput(paste0('parLimits', i), nl[i]),
               checkboxInput(paste0('parLimits', i+1), nl[i+1])
        ),
        column(4, selectInput(paste0('par', i), label=" ", choices=datanames(), selected = gsub("min.","",nl[i])))
      )#of FluidRow
    })
    
  })
  
  LimitsFilterNamesList <- reactive({
    if(is.null(input$fileLimits))# если файл с границами не пустой
      return(NULL)
    
    #список параметров, выбранных для фильтрации
    
    lim.par.names.min=c() # их имена в таблице границ
    lim.par.names.max=c() # их имена в таблице границ
    data.par.names = c() # их имена в исходной базе
    
    for (i in seq(1, ncol(limits()) - 1, by = 2)) #идем по всем параметрам в файле с границами
    {
      
      flag = FALSE # для этого параметра не установлены мин и макс границы
      if(input[[paste0('parLimits', i)]] == TRUE)
      {
        lim.par.names.min = c(lim.par.names.min, names(limits())[i+1])
        flag = TRUE #для этого параметра установлены мин границы
        data.par.names = c(data.par.names, input[[paste0('par', i)]])
        
      } 
      if(input[[paste0('parLimits', i+1)]] == TRUE)
      {
        lim.par.names.max = c(lim.par.names.max, names(limits())[i+2])
        if (flag == FALSE)
        {
          lim.par.names.min = c(lim.par.names.min, NA)
          data.par.names = c(data.par.names, input[[paste0('par', i)]])
        } 
        
      }
      else
      {
        if (flag == TRUE)
        {
          lim.par.names.max = c(lim.par.names.max, NA)
        } 
      }
      
    }
    table = data.frame(lim.par.names.min, lim.par.names.max, data.par.names, stringsAsFactors=FALSE)
    
    table
    
  })
  
  ##########################################################
  # ИМЕНА ПАРАМЕТРОВ ДЛЯ ВТОРИЧНОЙ ФИЛЬТРАЦИИ
  ##########################################################
  
  output$CloneParametersNames <-renderUI({
    selectInput("clone.par.names", "", c("R50"), selected = "R50", multiple = FALSE, selectize = TRUE)
  })
  
  ###################################################
  # ЕСЛИ ПОЛЬЗОВАТЕЛЬ НАЖИМАЕТ "Фильтрация"
  ###################################################
  
  filtered.data.A <- eventReactive(input$TotalFilter,
                                   {
                                     # если загружена уже размеченная база, заново ничего размечать не нужно!
                                     if(input$AlreadyFiltrated == TRUE)
                                       return(dataFirstFilter())
                                     else
                                     {
                                     # проверим, откуда в файле с границами начинается интересующий нас возрастной диапазон
                                     if (want()$limits.filter == TRUE)
                                       for (i in c(1:nrow(limits()))) 
                                         if(limits()[i,1]==input$Age[1])
                                         {
                                           shift = i - input$Age[1]
                                           break
                                         }
                                     ############################################################
                                     # собственно фильтрация:
                                     ############################################################
                                     
                                     #если фильтрация не требуется... 
                                     if (want()$limits.filter == FALSE & want()$clones.filter == FALSE)
                                       return(dataFirstFilter())
                                     else #если какая-нибудь фильтрация требуется...
                                     {
                                       all.data = dataFirstFilter() # берем таблицу данных нужного пола и возрастного диапазона
                                       
                                       # фильтрация по границам
                                       if (want()$limits.filter == TRUE)
                                       { 
                                         
                                         #идем по всей базе и помечаем, будет ли этот пациент удален
                                         #есди да из-за некорректного параметра,
                                         #представляем код удаления в виде 0100011...100000,
                                         #где единицы - номера некорректных параметров у данного пациента 
                                         #нумерация справа налево, с нуля, пять левых битов всегда нули
                                         
                                         #если да из-за повтора
                                         #представляем код удаления в виде 000...00011111
                                         #первая справа единица - резерв 
                                         #где вторая единица (номер 1 считая справа налево) - эмуляция измерения ((R >= 387.000) & (R <= 391.000) & (Xc >= 38.000) & (Xc <= 48.000)) 
                                         #третья справа (номер 2) - эмуляция R50 = 555.5 или 556 или 444
                                         #четвертая справа (номер 3) - измерение одного и того же подряд (соседние по времени отличающиеся по R50 менее чем на 1%)
                                         #пятая справа (номер 4) - измерение одного и того же в одном ЦЗ (совпадающие в одном ЦЗ по R50 и Xc50)
                                         
                                         #print("Start Limits Filtration!")#1 параметр для 1000 пациентов - 1 сек
                                         start = (Sys.time()) #засекаем время начала
                                         
                                         age.col.name = input$AgeColName
                                         # сортируем по возрасту 
                                         all.data = all.data[order(all.data[[age.col.name]]), ] 
                                         #идем по всем выбранным параметрам
                                         for(i in c(1:length(LimitsFilterNamesList()$data.par.names))) #для каждого выбранного для фильтрации параметра 
                                         {
                                           #устанавливаем прогресс-бар
                                           progress <- shiny::Progress$new(min=1, max=floor(nrow(all.data)/5000))
                                           on.exit(progress$close()) 
                                           progress$set(message = 'Фильтрация некорректных\n')
                                           
                                           
                                           # номер столбца в базе, в котором лежит текущий параметр
                                           col.num = (LimitsFilterNamesList()$data.par.names)[i]
                                          
                                           # номер столбца в таблице границ, в котором лежит мин граница текущего параметра
                                           col.num.lim.min = (LimitsFilterNamesList()$lim.par.names.min)[i]
                                           if(is.na(col.num.lim.min))
                                             col.num.lim.min = numeric(0)
                                           
                                           col.num.lim.max = (LimitsFilterNamesList()$lim.par.names.max)[i]
                                           if(is.na(col.num.lim.max))  
                                               col.num.lim.max = numeric(0)
                                           
                                           # есть ли ограничение на текущий параметр снизу (сверху) 
                                           flag.isna.min = is.na((LimitsFilterNamesList()$lim.par.names.min)[i])
                                           flag.isna.max = is.na((LimitsFilterNamesList()$lim.par.names.max)[i])
                                           
                                           #задаем возраст первого пациента и ограничения для него на текущий параметр сверху и снизу
                                           age.prev = round(all.data[1,input$AgeColName]) + shift
                                           lim.min = (limits()[age.prev, col.num.lim.min])
                                           lim.max = (limits()[age.prev, col.num.lim.max])
                                           
                                           # двоичный код i-го параметра
                                           pow = 2^(i+4)  
                                           
                                           for(patient in c(1:nrow(all.data))) #для каждого пациента
                                           {
                                             p = all.data[patient, col.num] #текущий параметр
                                             
                                             #если параметр NA, измерение точно некорректно
                                             if(is.na(p))
                                               all.data$'deletedA'[patient] = bitwOr(pow, all.data$'deletedA'[patient])
                                             else
                                             { 
                                               # возраст пациента 
                                               age = round(all.data[patient, age.col.name]) + shift
                                               
                                               if (age != age.prev) #если возраст пациента поменялся по сравнению с предыдущим
                                               {
                                                 # придется установить новые границы для параметров
                                                 lim.min = (limits()[age, col.num.lim.min])
                                                 lim.max = (limits()[age, col.num.lim.max])
                                               }
                                               
                                               c = TRUE # пациент корректен по текущему параметру
                                               
                                               
                                               if(!flag.isna.min)
                                               {
                                                 c = c & (p >= lim.min)
                                               }
                                               if(!flag.isna.max)
                                               {
                                                 c = c & (p <= lim.max)
                                               }
                                               if (!c)
                                               {
                                                 all.data$'deletedA'[patient] = bitwOr(pow, all.data$'deletedA'[patient])
                                               }
                                             }#of else, p is not na 
                                             
                                             if(patient %% 5000 == 0)
                                             {
                                               progress$set(value = round(patient/5000),
                                                            detail = paste(col.num, 'Просматриваем запись №', patient, 'из', nrow(all.data)))
                                               
                                             }
                                             age.prev = age 
                                           }# of patient
                                           progress$close()
                                         }# of i --- parameter
                                         print(Sys.time() - start)
                                       }# of if want.limits.filtration 
                                       
                                       # фильтрация по повторам
                                       if (want()$clones.filter == TRUE)
                                       {
                                         # сортируем по ФО, затем по региону, затем по ЦЗ, затем по времени
                                         
                                         if (!is.null(all.data[[input$RegColName]]) & !is.null(all.data[[input$LPUColName]]) & !is.null(all.data[[input$TimeColName]]))
                                         {   
                                             all.data[[input$TimeColName]] = as.POSIXct(all.data[[input$TimeColName]])
                                             all.data = all.data[order(all.data[[input$FOColName]],all.data[[input$RegColName]],all.data[[input$LPUColName]], all.data[[input$TimeColName]]), ] 
                                             
                                         }
                                         #print("Start CLones Filtration!")#1 параметр для 1000 пациентов - 1 сек
                                         start = (Sys.time()) #засекаем время начала
                                         
                                         #устанавливаем прогресс-бар
                                         progress <- shiny::Progress$new(min=1, max=floor(nrow(all.data)/5000))
                                         on.exit(progress$close()) 
                                         progress$set(message = 'Фильтрация повторов\n')
                                         
                                         
                                         #обрабатываем первого пациента отдельно
                                         lpu.prev = all.data[1,input$LPUColName]
                                         if(is.null(lpu.prev))
                                           lpu.prev = NA
                                         R.prev = all.data[1,input$clone.par.names[1]]
                                         Xc.prev = all.data[1,'Xc50']
                                         Rtime.prev = all.data[1,input$TimeColName] 
                                         #if(is.na(R.prev)) 
                                         #   all.data$'deletedA'[1] = bitwOr(1, all.data$'deletedA'[1])
                                         
                                         
                                         clone.par.name = input$clone.par.names[1] # какой параметр не должен совпадать у подряд идущих
                                         rtime.name = input$TimeColName
                                           
                                         cent.between.clones = input$cent.between.clones / 100
                                         LPUColName = input$LPUColName
                                         
                                         #print(all.data$RTime)
                                         #для каждого пациента:
                                         for(patient in c(2:nrow(all.data))) 
                                         {
                                           R = round(all.data[patient,clone.par.name],3)
                                           Rtime = all.data[patient,rtime.name]
                                           Xc = round(all.data[patient,'Xc50'],3)
                                           
                                           lpu = all.data[patient, LPUColName]
                                           
                                           
                                           #если R50 не указан, удаляем
                                           #if(is.na(R)) 
                                           #   all.data$'deletedA'[patient] = bitwOr(1, all.data$'deletedA'[patient])
                                           #else
                                           #{ 
                                              
                                              #удаляем R50 в промежутке от 387-391 Ohm и Хс50 42-48 Ohm вне зависимости от ЛПУ - это калибратор!
                                           
                                           
                                             if ((R >= 387.000) & (R <= 391.000) & (Xc >= 38.000) & (Xc <= 48.000))
                                             {
                                                 all.data$'deletedA'[patient] = bitwOr(all.data$'deletedA'[patient], 1)
                                             }
                                             
                                             #удаляем R50 = 555.500 и 444.000 вне зависимости от ЛПУ - это калибратор!
                                             if(round(R,3) == 444.000)
                                             {
                                               all.data$'deletedA'[patient] = bitwOr(all.data$'deletedA'[patient], 2)
                                             }
                                             if (round(R,3) == 555.500) | ((R == 556) & (Xc == 55.5))
                                             {
                                               all.data$'deletedA'[patient] = bitwOr(all.data$'deletedA'[patient], 2)
                                             }
                                             
                                             lpu = all.data[patient,LPUColName]
                                             if(is.null(lpu))
                                                 lpu = NA
                                               
                                             #если ЛПУ у пациента не указан
                                             if(is.na(lpu))
                                             {
                                                 #если ЛПУ у предыдущего перед ним не указано, а регионы совпадают, считаем их из одного ЛПУ 
                                                 if(is.na(lpu.prev))
                                                 {
                                                   #... и Rtime отличаются на 90 сек
                                                   if((as.numeric(difftime(Rtime, Rtime.prev, units="secs")) <= 90))
                                                   {
                                                     #print(as.numeric(difftime(Rtime, Rtime.prev, units="secs")))
                                                     all.data$'deletedA'[patient] = bitwOr(4, all.data$'deletedA'[patient])
                                                     if(patient > 1) all.data$'deletedA'[patient-1] = bitwOr(4, all.data$'deletedA'[patient-1])
                                                   }
                                                   #... и R50 отличаются на 1 %, а также Xc50 отличаются на 7% считаем их клонами
                                                   if((abs(R - R.prev)/R < cent.between.clones) & (abs(Xc - Xc.prev)/Xc < 7*cent.between.clones))
                                                   {
                                                     all.data$'deletedA'[patient] = bitwOr(8, all.data$'deletedA'[patient])
                                                     if(patient > 1) all.data$'deletedA'[patient-1] = bitwOr(8, all.data$'deletedA'[patient-1])
                                                     
                                                   }
                                                 }
                                             }
                                             #если ЛПУ у пациента указан и у предыдущего указан
                                             else if(!is.na(lpu.prev))
                                             {
                                                 # и пациенты, идущие подряд, из одного ЛПУ
                                                 if(lpu == lpu.prev)
                                                 { 
                                                   #... и Rtime отличаются на 90 сек
                                                   
                                                   if((as.numeric(difftime(Rtime, Rtime.prev, units="secs")) <= 90))
                                                   {
                                                     all.data$'deletedA'[patient] = bitwOr(4, all.data$'deletedA'[patient])
                                                     if(patient > 1) all.data$'deletedA'[patient-1] = bitwOr(4, all.data$'deletedA'[patient-1])
                                                   }
                                                   
                                                   #... и отличаются на 1 %, а также Xc50 отличаются на 7%, считаем их клонами
                                                   if((abs(R - R.prev)/R < cent.between.clones) & (abs(Xc - Xc.prev)/Xc < 7*cent.between.clones))
                                                   {
                                                     all.data$'deletedA'[patient] = bitwOr(8, all.data$'deletedA'[patient])
                                                     if(patient > 1) all.data$'deletedA'[patient-1] = bitwOr(8, all.data$'deletedA'[patient-1])
                                                     
                                                   }
                                                 }
                                             }
                                               
                                             R.prev = R
                                             Rtime.prev = Rtime
                                             Xc.prev = Xc
                                               
                                             if(!is.na(lpu))
                                                lpu.prev = lpu
                                             else
                                                 lpu.prev = NA
                                               
                                             if(patient %% 5000 == 0)
                                             {
                                                 progress$set(value = round(patient/5000),
                                                              detail = paste('Просматриваем запись №', patient, 'из', nrow(all.data)))
                                             }
                                             
                                           #}# of else - R50 is not NA
                                         }#of for by pacient
                                         progress$close()
                                         
                                         ##################################################################
                                         #        Выкидываем не идущих подряд, но одинаковых пациентов
                                         ##################################################################
                                         unique.regs = unique(as.character(all.data[, input$RegColName]))
                                        
                                         #устанавливаем прогресс-бар
                                         progress <- shiny::Progress$new(min=1, max=length(unique.regs))
                                         on.exit(progress$close()) 
                                         progress$set(message = 'Фильтрация повторов (продолжение)\n')
                                         
                                         for (reg in unique.regs)
                                         {
                                           
                                           LPUs = unique(as.character(all.data[all.data[[input$RegColName]]== reg, input$LPUColName]))
                                           for (lpu in LPUs)
                                           {
                                               # если ЛПУ в регионе указано...
                                               if(!is.na(lpu)) 
                                               {
                                                  #строим гистограмму по R50
                                                  if(length(all.data[(all.data[[input$RegColName]] == reg) & !is.na(all.data[[input$LPUColName]]) & (all.data[[input$LPUColName]] == lpu) & all.data$deletedA == 0, "R50"]) != 0)
                                                  {
                                                      histR50 = hist(all.data[(all.data[[input$RegColName]] == reg) & !is.na(all.data[[input$LPUColName]]) & (all.data[[input$LPUColName]] == lpu) & all.data$deletedA == 0, "R50"], 100, plot = FALSE)
                                                      frecR50 = histR50$counts
                                                   
                                                      if(length(frecR50) > 3)
                                                      # если какой-то столбец  выше соседей более, чем в 1.5 раза и там более 50 пациентов, удаляем всех пациентов из этого столбца
                                                          for (i in c(3:(length(frecR50)-2)))
                                                          {
                                                              if((frecR50[i] > 50) & (frecR50[i] > 1.5*mean(c(frecR50[i-1], frecR50[i-2], frecR50[i+1], frecR50[i+2]))))
                                                              {
                                                                    rows = (all.data[[input$RegColName]] == reg) & !is.na(all.data[[input$LPUColName]]) & (all.data[[input$LPUColName]] == lpu) & (all.data[[input$clone.par.names]]>=histR50$breaks[i]) & (all.data[[input$clone.par.names]]<=histR50$breaks[i+2]) 
                                                                    #print(c(frecR50[i], 1.5*mean(c(frecR50[i-1], frecR50[i-2], frecR50[i+1], frecR50[i+2]))))
                                                                    all.data[rows, "deletedA"] = bitwOr(all.data[rows, "deletedA"], 16)
                                                              }
                                                          }
                                                  }    
                                                    
                                               }
                                               else #если ЛПУ не указан
                                               {
                                                 #строим гистограмму по R50
                                                 if(       length(all.data[(all.data[[input$RegColName]] == reg) & is.na(all.data[[input$LPUColName]]) & all.data$deletedA == 0, "R50"]) != 0)
                                                 {
                                                   histR50 = hist(all.data[(all.data[[input$RegColName]] == reg) & is.na(all.data[[input$LPUColName]]) & all.data$deletedA == 0, "R50"], 100, plot = FALSE)
                                                   frecR50 = histR50$counts
                                                  
                                                   # если какой-то столбец  выше соседей более, чем в 1.5 раза и там более 50 пациентов, удаляем всех пациентов из этого столбца
                                                   if(length(frecR50) > 3)
                                                   for (i in c(3:(length(frecR50)-2)))
                                                     if(frecR50[i] > 50 & frecR50[i] > 1.5*mean(c(frecR50[i-1], frecR50[i-2], frecR50[i+1], frecR50[i+2])))
                                                     {
                                                       rows = (all.data[[input$RegColName]] == reg) & is.na(all.data[[input$LPUColName]]) & (all.data[[input$clone.par.names]]>=histR50$breaks[i]) & (all.data[[input$clone.par.names]]<=histR50$breaks[i+2])
                                                       all.data[rows, "deletedA"] = bitwOr(all.data[rows, "deletedA"], 16)
                                                       
                                                     }
                                                 }
                                               }#of else
                                               
                                           }#of for LPU
                                           progress$set(value = which(unique.regs == reg),
                                                        detail = paste('Просматриваем регион:', reg))
                                           
                                         }#of for reg
                                         
                                         print(Sys.time() - start)
                                       } # of if - фильтрация по повторам
                                       
                                       all.data$deletedA_binary = intToBin(all.data$deletedA)
                                       return(all.data)
                                     }# of else (если какая-нибудь фильтрация требуется)
                                     }  
                                   })# of eventReactive
  
  ####################################################################
  # РАСПЕЧАТЫВАЕМ БАЗУ С ЗАПИСЯМИ, РАЗМЕЧЕННЫМИ ДЛЯ УДАЛЕНИЯ
  ####################################################################
  
  output$FirstFilterDataDeleted <- renderDataTable({
    
    #если никакой фильтрации не было
    if(want()$limits.filter == FALSE & want()$clones.filter == FALSE)
      return(NULL)
    
    # если была
    if(want()$limits.filter == TRUE) 
    {
      filtered.data.A()[filtered.data.A()$'deletedA' != 0, c(input$TimeColName, input$LPUColName, input$AgeColName, LimitsFilterNamesList()$data.par.names, input$clone.par.names, 'deletedA')]
      
    }
    else 
    {
      filtered.data.A()[filtered.data.A()$'deletedA' != 0, c(input$TimeColName, input$LPUColName, input$AgeColName, input$clone.par.names, 'deletedA')]
      
    }
    
  }) 
  
  ####################################################################
  # РАСПЕЧАТЫВАЕМ СВОДНУЮ ТАБЛИЦУ С КОЛИЧЕСТВОМ УДАЛЕННЫХ ЗАПИСЕЙ
  ####################################################################
  
  output$DeletedSummary <- renderTable({
    
    deleted1 =  deleted()
    
    # делаем формат даты не числом, а месяц-год. 4 столбец - Year
    if(input$AlreadySummary != TRUE)
    {  
        deleted1[,4] = as.character(as.yearmon(as.numeric(deleted1[,4])))
    }
    deleted1
    
  })
  
  del.by.par <- function(p, year, fo, reg, lpu) # p- двоичный код параметра фильтрации
  {
              
    if(is.na(lpu))
    {
       del.by.par = sum(as.numeric(as.logical(bitwAnd(filtered.data.A()[(filtered.data.A()[['ObsMonth']] == year) & (filtered.data.A()[[input$FOColName]] == fo) & (filtered.data.A()[[input$RegColName]] == reg) & is.na(filtered.data.A()[[input$LPUColName]]),'deletedA'], p))))
    
    }
    else
    {
       del.by.par = sum(as.numeric(as.logical(bitwAnd(filtered.data.A()[(filtered.data.A()[['ObsMonth']] == year) & (filtered.data.A()[[input$FOColName]] == fo) & (filtered.data.A()[[input$RegColName]] == reg) & !is.na(filtered.data.A()[[input$LPUColName]]) & (filtered.data.A()[[input$LPUColName]] == lpu),'deletedA'], p))))
    }        
    del.by.par
    
  }
  
  # считаем, сколько строк будет в сводной таблице
  row.number <- reactive({
    
    i = 0
    for (fo in FOs())
    {
      unique.regs = unique(as.character(dataFirstFilter()[dataFirstFilter()[[input$FOColName]]== fo, input$RegColName]))
      for (reg in unique.regs)
      {
        LPUs = unique(as.character(dataFirstFilter()[dataFirstFilter()[[input$RegColName]]== reg, input$LPUColName]))
        for (lpu in LPUs)
        {
          for (year in input$ObsYear)
          {
            i = i + 1
          }
        }
      }
    }
    i
  })
  
  #Делаем Общую таблицу со статистикой, чтобы затем выводить нужные ее части
  deleted <- reactive({

    if (input$"AlreadySummary" == TRUE)
    {
      
      deleted = as.data.frame(data())
      return(deleted)
      
    }
    else
    {
       write.table(filtered.data.A()[filtered.data.A()$'deletedA'==0, ],  paste0(output.folder(),'/good.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE, qmethod = "double")
       write.table(filtered.data.A()[filtered.data.A()$'deletedA'> 0, ],  paste0(output.folder(),'/bad.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE, qmethod = "double")
       write.table(filtered.data.A(), paste0(output.folder(),'/all_marked.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE)
      
    #  if(input$Sex == 'Ж')
    #  {
    #      write.table(filtered.data.A()[filtered.data.A()$'deletedA'==0, ],  paste0(output.folder(),'/good_w.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE, qmethod = "double")
    #      write.table(filtered.data.A()[filtered.data.A()$'deletedA'> 0, ],  paste0(output.folder(),'/bad_w.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE, qmethod = "double")
    #      write.table(filtered.data.A(), paste0(output.folder(),'/all_marked_w.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE)
    #  }
    #  if(input$Sex == 'М')
    #  {
    #    write.table(filtered.data.A()[filtered.data.A()$'deletedA'==0, ],  paste0(output.folder(),'/good_m.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE, qmethod = "double")
    #    write.table(filtered.data.A()[filtered.data.A()$'deletedA'> 0, ],  paste0(output.folder(),'/bad_m.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE, qmethod = "double")
    #    write.table(filtered.data.A(), paste0(output.folder(),'/all_marked_m.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE)
    #  }
    
    
    
    
    # устанавливаем прогресс-бар
    progress <- shiny::Progress$new(min=1, max=row.number())
    on.exit(progress$close()) 
    progress$set(message = 'Генерируем сводную таблицу\n')
    
    deleted = NULL # таблица со статистикой удаленных, пока пустая
    i = 0 # номер текущей записи в таблице
    
    #сюда сохраняем очередную строку таблицы
    tmp = c()
    parlist = LimitsFilterNamesList()$data.par.names
    #задаем имена строк
    #идем по федеральным округам
    for (fo in FOs())
    {    
        
        unique.regs = unique(as.character(dataFirstFilter()[dataFirstFilter()[[input$FOColName]]== fo, input$RegColName]))
        for (reg in unique.regs)
        {
            LPUs = unique(as.character(dataFirstFilter()[dataFirstFilter()[[input$RegColName]]== reg, input$LPUColName]))
            for (lpu in LPUs)
            {
              #выбираем месяцы - пересечение тех, которые хочет пользователь и тех, которые есть в данных текущего ЦЗ
               user.dates = c((as.numeric(format(input$ObsDate[1], "%m")) + 12*(as.numeric(format(input$ObsDate[1], "%Y")) - 2010)) : (as.numeric(format(input$ObsDate[2], "%m")) + 12*((as.numeric(format(input$ObsDate[2], "%Y")))-2010)))
               years = intersect(user.dates, unique(dataFirstFilter()[(dataFirstFilter()[[input$RegColName]]== reg) & (dataFirstFilter()[[input$LPUColName]]==lpu), 'ObsMonth']))
                
               for (year in years)
               {
                    i = i + 1
                    progress$set(value = i,
                                 detail = paste('Просматриваем регион:', reg, 'ЛПУ',  lpu, 'Год', year))
                    
                    tmp["LPU"] = lpu
                    tmp["Region"] = reg
                    tmp["FO"] = fo
                    tmp["Year"] = (as.yearmon(2010 + (year-1)/12))
                    
                    
                    if(!is.na(lpu)) # лпу известно
                    { 
                      tmp["Total"] = nrow(filtered.data.A()[(filtered.data.A()[[input$FOColName]] == fo) & (filtered.data.A()[[input$RegColName]] == reg) & (!is.na(filtered.data.A()[[input$LPUColName]]) & filtered.data.A()[[input$LPUColName]] == lpu & filtered.data.A()[['ObsMonth']] == year),])
                    
                    }
                    else
                    {
                      tmp["Total"] = nrow(filtered.data.A()[(filtered.data.A()[[input$FOColName]] == fo) & (filtered.data.A()[[input$RegColName]] == reg) & (is.na(filtered.data.A()[[input$LPUColName]]) & filtered.data.A()[['ObsMonth']] == year),])
                    }
                    
                    for (par in parlist)
                    {
                        tmp[par] = del.by.par(2^(which(parlist == par) + 4), year, fo, reg, lpu)
                        tmp[paste0(par, "_X")] = as.numeric(tmp[par]) / as.numeric(tmp["Total"] )*100
                        
                    }
                    
                    tmp["Эмуляция.данных.R_387_391_Ом__Xc_38_48_Ом"] = del.by.par(1, year, fo, reg, lpu)
                    tmp["Эмуляция.данных.R_387_391_Ом__Xc_38_48_Ом_X"] = as.numeric(tmp["Эмуляция.данных.R_387_391_Ом__Xc_38_48_Ом"]) / as.numeric(tmp["Total"] )*100
                    tmp["Эмуляция.данных.R_444_Ом"] = del.by.par(2, year, fo, reg, lpu)
                    tmp["Эмуляция.данных.R_444_Ом_X"] = as.numeric(tmp["Эмуляция.данных.R_444_Ом"]) / as.numeric(tmp["Total"] )*100
                    tmp["Измерения.подряд.менее.90.сек"] = del.by.par(4, year, fo, reg, lpu)
                    tmp["Измерения.подряд.менее.90.сек_X"] = as.numeric(tmp["Измерения.подряд.менее.90.сек"]) / as.numeric(tmp["Total"] )*100
                    tmp["Clones"] = del.by.par(8 + 16, year, fo, reg, lpu)
                    tmp["Clones_X"] = as.numeric(tmp["Clones"]) / as.numeric(tmp["Total"] )*100
                    tmp["Outliers"] = del.by.par(2^(length(parlist) + 4 + 1)-1 -1-2-4-8-16, year, fo, reg, lpu)
                    tmp["Outliers_X"] = as.numeric(tmp["Outliers"]) / as.numeric(tmp["Total"] )*100
                    tmp["Frauds"] = del.by.par(1+2+4+8+16, year, fo, reg, lpu)
                    tmp["Frauds_X"] = as.numeric(tmp["Frauds"]) / as.numeric(tmp["Total"] )*100
                    tmp["Incorrect"] = del.by.par(2^(length(parlist) + 4 + 1) - 1, year, fo, reg, lpu)
                    tmp["Incorrect_X"] = as.numeric(tmp["Incorrect"]) / as.numeric(tmp["Total"] )*100
                    
                    # добавляем к сводной таблице очередную строку
                    deleted = rbind(deleted, tmp)
                    
               }# of year
              
            }# of lpu
        }# of reg
    }# of fo
    progress$close()
    
    row.names(deleted) = NULL
     
    #if(input$Sex == 'Ж')
    #{
    #  write.table(deleted, paste0(output.folder(),'/deleted_summary_w.csv'), dec = ',', sep = ';', quote = FALSE, row.names = FALSE)
    #  
    #}
    #if(input$Sex == 'М')
    #{
    #write.table(deleted, paste0(output.folder(), '/deleted_summary_m.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    #  
    #}
    
    deleted1 = deleted
    if(input$AlreadySummary != TRUE)
    {  
        deleted1[,4] = as.character(as.yearmon(as.numeric(deleted1[,4])))
    }
    write.table(deleted1, paste0(output.folder(), '/deleted_summary.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    
    #write.table(deleted, paste0(output.folder(), "/summary.csv"), sep = ';', qmethod = "double", row.names = FALSE)
    deleted
    }#of else
    
  }) 
  
  ####################################################################
  # РАСПЕЧАТЫВАЕМ ТАБЛИЦУ С КОЛИЧЕСТВОМ УДАЛЕННЫХ ПО РЕГИОНАМ
  ####################################################################
  
  output$DeletedSummaryByRegions <- renderTable({
    deletedByRegions()
  })
  
  deletedByRegions <- reactive({
    
    d = as.data.frame(deleted(), stringsAsFactors = FALSE)
   
    #все столбцы сводной таблицы превращаем в числовые
    for (i in 4:ncol(d))
        d[[i]] = as.numeric(d[[i]])
    
    #складываем общее количество удаленных для каждого региона (суммируем все года)
    dby = ddply(d,.(Region), summarize, Total = sum(Total), Outliers = sum(Outliers), Frauds = sum(Frauds), Incorrect = sum(Incorrect))
    dby$"Outliers %" = round(dby$Outliers/dby$Total*100,0)
    dby$"Frauds %" = round(dby$Frauds/dby$Total*100,0)
    dby$"Incorrect %" = round(dby$Incorrect/dby$Total*100,0)
   # dby$"Benford" = getSuspects(, chosendata(), by = 'difference', how.many=2) 
   # dby$"Benford %" = round(dby$Benford/dby$Total*100,0)
    # располагаем столбцы в удобном порядке
    dby = dby[order(dby$"Incorrect %"),c("Region","Total", "Outliers", "Outliers %", "Frauds", "Frauds %", "Incorrect", "Incorrect %")]
    
    
    #печатаем в файл
    #if(input$Sex == 'Ж')
    #{
    #  write.table(deleted, paste0(output.folder(), '/deleted_summary_by_regions_w.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    #  
    #}
    #if(input$Sex == 'М')
    #{
    #  write.table(deleted, paste0(output.folder(), '/deleted_summary_by_regions_m.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    #}
    write.table(deleted, paste0(output.folder(), '/deleted_summary_by_regions.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    
    
    dby
    
  }) 
  
  ####################################################################
  # РАСПЕЧАТЫВАЕМ ТАБЛИЦУ С ОБЩИМ КОЛИЧЕСТВОМ ПАЦИЕНТОВ В ФО ПО ГОДАМ
  ####################################################################
  
  output$TotalSummaryByYears <- renderTable({
    totalByYears()
  })
  
  totalByYears <- reactive({
    
    
    d = as.data.frame(deleted(), stringsAsFactors = FALSE)
    
    #все столбцы сводной таблицы превращаем в числовые
    for (i in 5:ncol(d))
    
      
        d[[i]] = as.numeric(d[[i]])
    
    #складываем общее количество измеренных пациентов для каждого региона (суммируем все года)
    dby = ddply(d,.(FO, Year), summarize, Total = sum(Total))
    cast.dby = cast(dby, FO~Year, mean)
    #печатаем в файл
    #write.table(dby, paste0(output.folder(), '/deleted_summary_by_years.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    
    cast.dby
    
  }) 
  ####################################################################
  # РАСПЕЧАТЫВАЕМ ТАБЛИЦУ С % НЕКОРРЕКТНЫХ ПАЦИЕНТОВ В ФО ПО ГОДАМ
  ####################################################################
  
  output$DeletedSummaryByYears <- renderTable({
    deletedByYears()
  })
  
  deletedByYears <- reactive({
    
    
    d = as.data.frame(deleted(), stringsAsFactors = FALSE)
    
    #все столбцы сводной таблицы превращаем в числовые
    for (i in 4:ncol(d))
      d[[i]] = as.numeric(d[[i]])
    
    #складываем общее количество измеренных пациентов для каждого ФО (суммируем все года)
    dby = ddply(d,.(input$FOColName, Year), summarize, Total = sum(Total), Incorrect = sum(Incorrect))
    dby$Incorr.per = round(dby$Incorrect / dby$Total*100,0)
    dby = dby[, c("FO","Year","Incorr.per")]
    cast.dby = cast(dby, FO~Year, mean)
    #печатаем в файл
    write.table(dby, paste0(output.folder(), '/deleted_summary_by_years.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    
    #if(input$Sex == 'Ж')
    #{
    #     write.table(dby, paste0(output.folder(), '/deleted_summary_by_years_w.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    #}
    #if(input$Sex == 'М')
    #{
    #  write.table(dby, paste0(output.folder(), '/deleted_summary_by_years_m.csv'), dec = ',', sep = ';',  qmethod = "double", row.names = FALSE)
    #}
    cast.dby
    
  }) 
  
  ####################################################################
  # ЦВЕТ ДЛЯ КАЖДОГО ФО
  ####################################################################
  FOcolors <- reactive({
    c('#FB5607','#FFBE0B','#82FF9E','#4AC6B7', '#192BC2', '#965F8A', '#FF7070', '#C61951')
    
  })
  ####################################################################
  # РИСУЕМ ГРАФИК С КОЛИЧЕСТВОМ УДАЛЕННЫХ ПО ГОДАМ В ФО
  ####################################################################
  
  output$PlotTotalSummaryByYears <- renderPlotly({
    
    df = as.data.frame(totalByYears())
    #устанавливаем цвета для каждого ФО
    colors <- FOcolors()
    #годы
    x = as.numeric(names(df)[2:ncol(df)])
    
    #количества измерений
    y = df[, 2:ncol(df)]
    #рисуем график для каждого ФО
    p <- plot_ly()
    for (i in 1:ncol(df))
    {
      p <- add_trace(p, x=as.character(as.yearmon(x)), y=as.numeric(y[i,]), name=df[i,1], type='scatter', mode = "lines+markers", line = list(width = 2, color = colors[i]), marker = list(color = colors[i]))
    }
    p <- layout(p, title="Количество записей",
                xaxis = list(title = 'Год обследования'),
                yaxis = list(title = 'Количество записей'))
    p
    
  })
  
  
  ####################################################################
  # РИСУЕМ ГРАФИК С ПРОЦЕНТОМ УДАЛЕННЫХ ПО ГОДАМ В ФО
  ####################################################################
  
  output$PlotDeletedSummaryByYears <- renderPlotly({
    
    df = as.data.frame(deletedByYears())
    #устанавливаем цвета для каждого ФО
    colors <- FOcolors()
    #годы
    x = as.numeric(names(df)[2:ncol(df)])
    
    #количества измерений
    y = df[, 2:ncol(df)]
    #рисуем график для каждого ФО
    p <- plot_ly()
    for (i in 1:ncol(df))
    {
      p <- add_trace(p, x=as.character(as.yearmon(x)), y=as.numeric(y[i,]), name=df[i,1], type='scatter', mode = "lines+markers", line = list(width = 2, color = colors[i]), marker = list(color = colors[i]))
    }
    p <- layout(p, title="Процент некорректных данных по годам обследования",
                xaxis = list(title = 'Год обследования'),
                yaxis = list(title = 'Процент некорректных данных'))
    p
    
  })
  
  
  
  ###################################################################
  # УДАЛЕННЫЕ НА КАРТЕ (ПО РЕГИОНАМ) - вызовы функций
  ###################################################################
  
  output$'RussiaMaps' <- renderUI({
    
    # задаем таблицу с названиями и параметрами карт для всех видов фильтрации
    name = paste("map", 1:4, sep = "")
    #map.par.name = c("Total", "Incorrect", "Incorrect..X", "Outliers", "Outliers..X", "Frauds", "Frauds..X")
    #map.title = c("Все", "Некорректные и повторы", "Процент некорректных данных и повторов", "Количество некорректных данных", "Процент некорректных данных", "Количество повторов", "Процент повторов")
    map.par.name = c("Total", "Outliers", "Frauds", "Incorrect")
    map.title = c("Все", "Ошибки измерения", "Подделки", "Удаленные")
    
    
    map.args = data.frame(name, map.par.name, map.title)
    maps = c(1:4)
    
    # # устанавливаем, какие карты (вид от 1 до 7) рисовать в зависимости от желаемых видов фильтрации
    # if(want()$limits.filter == FALSE & want()$clones.filter == FALSE)
    #   maps = c(1)
    # else if(want()$limits.filter == TRUE & want()$clones.filter == FALSE)
    #   maps = c(1, 4, 5)
    # else if(want()$limits.filter == TRUE & want()$clones.filter == TRUE)
    #   maps = c(1:7)
    # else if(want()$limits.filter == FALSE & want()$clones.filter == TRUE)
    #   maps = c(1, 6, 7)
    
    # рисуем карты в зависимости от желаемых видов фильтрации
    lapply(maps, function(i){
      name = as.character(map.args$name[i])
      output[[name]] <- renderPlot({ 
        showMap(map.par.name = as.character(map.args$map.par.name[i]), #название столбца в СВОДКЕ, который хотим рисовать
                map.title = as.character(map.args$map.title[i])) # заголовок легенды
      })
      
      fluidPage(
        h3(map.args$map.title[i]),
        plotOutput(name)
      )
    }) 
  }) 
  
  ##############################################################################
  # ПОДГОТОВКА ФАЙЛА С ГРАНИЦАМИ РЕГИОНОВ И ПРИВЯЗКА СВОДКИ УДАЛЕННЫХ К НЕМУ 
  ##############################################################################
  
  shape.upd <- reactive({
    # читаем файл с координатами границ регионов
    # Idvar - поле, которое будет использоваться как идентификатор региона
    shape <- readShapePoly('GIS data/Changed/rus_adm1new_Project_Union1', IDvar="ID_1")
    
    #shape <- readShapePoly('GIS/rus_adm1new_Project_Union1', IDvar="ID_1")
    # склеиваем по полю ID (в нашей базе и в файле с координатами границ это поле должно совпадать, по нему регион идентифицируется)
    id <- c(shape$ID_1) #указываем, по какому полю склеиваем
    #добавляем идентификаторы регионов к нашей таблице удаленных
   
    
    Sdata = deletedByRegions()
    
    
     # делаем регионы не элементами, а названиями строк
    row.names(Sdata) = Sdata$Region
    Sdata = subset(Sdata, select = -c(Region) )
    
    
    Sdata$id = rep(0) # пока поле id и численность заполняем нулями
    Sdata$population = rep(0)
    
  
    regnames = read.csv("regionsID.csv", sep = ';', quote = "")# считываем таблицу с id и населениями
    
    for (reg.in.deleted in row.names(Sdata))# для каждого региона (только региона!) в таблице удаленных
    {
      for(reg.in.IDs in regnames[[1]])#для каждого имени региона в таблице идентификаторов регионов 
      { 
        if(reg.in.IDs %in% strsplit(reg.in.deleted, ' ')[[1]]) #нашли имя региона
        {
          Sdata[reg.in.deleted, 'id'] = regnames[regnames$'region' == reg.in.IDs, 'id']
          Sdata[reg.in.deleted, 'population'] = regnames[regnames$'region' == reg.in.IDs, 'population']
          break;
        }
      }
      
    }
    
    
    data1 <- Sdata[match(id, Sdata$id),]
    row.names(data1) <- id
    shape.upd <- spCbind(shape, data1)
    
    shape.upd
    
    # теперь в shape.upd нужная нам база
    
  })
  
  ######################################################
  # УДАЛЕННЫЕ НА КАРТЕ - функция отрисовки
  ###################################################### 
  
  showMap <- function(map.par.name, #название столбца в СВОДКЕ, который хотим рисовать
                      map.title) # заголовок легенды
  {
    
    # устанавливаем нужный нам столбец, 
    #данные из которого мы будем изображать цветом на карте
    parameter = shape.upd()[[map.par.name]]
    
    
    
    # Устанавливаем заголовок легенды
    title = map.title
    
    # #Выбираем цветовую палитру (эта команда рисует всевозможные цветовые палитры вместе с их названиями)
    # display.brewer.all()
    
    # устанавливаем количество цветовых градаций
    lb = 10 # количество градаций
    bins = quantile(na.omit(parameter),seq(0,1,length=lb)) # урны, по которым будут распределяться значения
    
    # выбираем палитру "YlOrRd" (оттенки желтого и красного)
    colpal = brewer.pal(length(bins)-1,"YlOrRd") 
    colors = colpal[findInterval(parameter, bins, rightmost.closed=T)]
    
    # генерируем рисунок, состоящий из трех окон разного размера:
    # 1.карта регионов, 2.гистограмма рапределения значений параметра и 3.легенда
    layout(matrix(c(1,2),1,2), widths=c(1,0.25), heights=1)
    
    #######################################################################
    # 1. Рисуем раскрашенную карту регионов
    par(mar=c(0,0,0,0))# расстояния до краев рисунка
    plot(shape.upd(), col=colors)
    
    
    #######################################################################
    # 2. Рисуем легенду к карте...
    plot(c(0,1),c(0,1),col=NA,axes=F, xlab="", ylab="")
    points(rep(0.1,lb-1), seq(0.1,lb*0.025+0.1,length=lb-1), pch=15, cex=2.5, col=1)
    points(rep(0.1,lb-1), seq(0.1,lb*0.025+0.1,length=lb-1), pch=15, cex=2, col=colpal)
    #... и подписи к легенде
    text(rep(0.6,lb-1), seq(0.1,lb*0.025+0.1,length=lb-1), paste(round(bins[-lb],0),"-",round(bins[-1],0)), cex=1)
    text(0.5, lb*0.025+0.2, title, cex=1, font=2)
    
    
    #######################################################################
    # 3. Рисуем функцию распределения параметра
    op = par(mar=c(1,1,1,1), fig=c(0.75,0.95,0.75,0.95), new = TRUE, las=1)
    plot(density(na.omit(parameter)), xlab="", ylab="", axes=F, lwd=3, main="")
    
    #цикл по всем урнам. 
    for(i in 1:(length(bins)-1)) { polygon(c(bins[i],bins[i+1],bins[i+1],bins[i]),c(0,0,4,4),col=colpal[i],border=NA)}
    #каждая урна (диапазон значений параметра) подкрашивается
    #соответствующим цветом из палитры. На фоне этих цветов
    #рисуем функцию распределения 
    #сначала толстой белой линией lwd=6 
    lines(density(na.omit(parameter)),lwd=6,col='white')
    #а потом тонкой черной
    lines(density(na.omit(parameter)),lwd=3)
    
    axis(1, seq(round(min(na.omit(parameter)),2), round(max(na.omit(parameter)),2), length=2))
    #и красными пунктирами разделяем границы урн 
    abline(v = bins, col="red", lwd=1.5, lty=3)
    par(op)
    
    
  }
  
  #########################################################
  # ИНТЕРАКТИВНАЯ КАРТА
  ##########################################################
  output$'interactiveMap' <- renderLeaflet({
    lpu.coords = read.csv2("postalcodesMukin.csv", dec = ".", stringsAsFactors = FALSE)
    
    m = leaflet(lpu.coords) %>% addTiles() %>% setView(90, 55, zoom = 2) %>% addCircleMarkers(lng=~longitude.new, lat=~latitude.new, popup=~LPUname, radius = 1, color = rgb(0.25, 0.5, 0.5))
    m
  })
  ######################################################
  # ГРАФИКИ ЦЗ в порядке возрастания количества измерений и процента подделок
  ###################################################### 

output$LPUSortByNumber <- renderPlotly({
  df = total.vs.deleted()
  
  df = df[order(df$Total),]
  #в какой центиль попадает ЦЗ по количеству измерений
  df$total.per = 100 - round(c(1:nrow(df))/nrow(df)*100,0)
  df$integral.per = round(unlist(lapply(c(1:nrow(df)), function(i) sum(df$Total[i:nrow(df)])/sum(df$Total[1:nrow(df)])*100)),0)
  
  
  plot_ly(df, y = ~Total, type = 'scatter', mode = 's', fill = 'tozeroy', text = ~paste(Region, '<br>', LPU, '<br>В', total.per, '% ЦЗ', integral.per, '% всех данных')) %>%
    layout(xaxis = list(title = 'Порядковый номер центра здоровья'),
           yaxis = list(title = 'Количество записей'), showlegend = FALSE)
  
})
  

output$LPUSortByDeleted <- renderPlotly({
  df = total.vs.deleted()
  #процент некорректных измерений в ЦЗ
  df$incorrect.per = df$Incorrect/df$Total*100
  
  df = df[order(df$incorrect.per),]
  #в какой центиль попадает ЦЗ по проценту удаленных
  df$total.per = 100 - round(c(1:nrow(df))/nrow(df)*100,0)
  df$integral.per = round(unlist(lapply(c(1:nrow(df)), function(i) sum(df$Incorrect[i:nrow(df)])/sum(df$Incorrect[1:nrow(df)])*100)),0)
  
  
  plot_ly(df, y = ~incorrect.per, type = 'scatter', mode = 's', fill = 'tozeroy', text = ~paste(Region, '<br>', LPU, '<br>В', total.per, '% ЦЗ', integral.per, '% некорректных данных')) %>%
    layout(xaxis = list(title = 'Порядковый номер центра здоровья'),
           yaxis = list(title = 'Процент некорректных данных'), showlegend = FALSE)
  
})

  ######################################################
  # ГРАФИК КАЧЕСТВА ИЗМЕРЕНИЙ В ЛПУ
  ###################################################### 

total.vs.deleted <- reactive({
  
  # выбираем только данные по названиям лпу из таблицы удаленных
  
  d = as.data.frame(deleted(), stringsAsFactors = FALSE)
  
  #все столбцы сводной таблицы превращаем в числовые
  for (i in 4:ncol(d))
    d[[i]] = as.numeric(d[[i]])
  
  #складываем общее количество удаленных для каждого региона (суммируем все года)
  total.vs.deleted = ddply(d,.(FO, Region, LPU), summarize, Total = sum(Total), Incorrect = sum(Incorrect), Frauds = sum(Frauds))
  
  
  #row.names(total.vs.deleted) = row.names(deleted()[is.lpu,])
  #names(total.vs.deleted) = c("Количество", "Процент_удаленных")
  total.vs.deleted
})



output$LPUQualities <- renderPlotly({
  df = total.vs.deleted()
  #для каждого ЦЗ считаем процент некорректных
  df$incorrect.per = df$Incorrect/df$Total*100
  # для каждого ЦЗ считаем процент подделок
  df$frauds.per = df$Frauds/df$Total*100
  
  # на сколько умножаем процент подделок, чтобы получить размер маркера
  slope <- 5
  #вычисляем адекватный размер маркера
  df$size <- sqrt((df$frauds.per+5) * slope)
  
  # цвета для каждого ФО
  colors <- FOcolors()
  
  # рисуем баббл-чат для всех ЦЗ
  p <- plot_ly(df, x = ~Total, y = ~incorrect.per, color = ~FO, size = ~size, colors = colors,
               type = 'scatter', mode = 'markers', sizes = c(min(df$size), max(df$size)),
               marker = list(symbol = 'circle', sizemode = 'diameter',
                             line = list(width = 2, color = '#FFFFFF')),
               text = ~paste(Region, '<br>', LPU)) %>%
    layout(#title = 'Качество данных центров здоровья (размер маркеров пропорционален доле подделок)',
      xaxis = list(title = 'Количество обследованных в центре здоровья',
                   #gridcolor = 'rgb(255, 255, 255)',
                   range = c(min(df$Total)*0.9, max(df$Total)*1.1),
                   zerolinewidth = 1,
                   ticklen = 5,
                   gridwidth = 2),
      yaxis = list(title = 'Процент некорректных данных',
                   #gridcolor = 'rgb(255, 255, 255)',
                   range = c(0, 100),
                   zerolinewidth = 1,
                   ticklen = 5,
                   gridwith = 2),
      #paper_bgcolor = 'rgb(255, 255, 243)',
      #plot_bgcolor = 'rgb(243, 243, 243)',
      showlegend = TRUE)
  
})



  ######################################################
  # БЕНФОРД-АНАЛИЗ - распределения первых цифр
  ###################################################### 
  ben.data <- reactive({
    par =  input$ParametersNames
    benford(chosendata()[, par]^10, 2)
  })


  ben.data.good <- reactive({
    par =  input$ParametersNames
    benford(chosendata()[chosendata()$deletedA == 0, par]^10, 2)
  })
  
  ben.data.bad <- reactive({
    par =  input$ParametersNames
    benford(chosendata()[chosendata()$deletedA > 0, par]^10, 2)
  })
  
  output$BENhistsAll <- renderPlot({
     plot(ben.data())
  })

  output$BENhistsFiltered <- renderPlot({
     plot(ben.data.good())
  })
  
  output$BENhistsDeleted <- renderPlot({
     plot(ben.data.bad())
  })


  
  suspects <- reactive({
    for (reg in input$Regions)
    {
       for (lpu in input$HCname)
       {
          benford(chosendata()[chosendata()[[]]==lpu, par]^10, 2)
          getSuspects(ben.data(), chosendata(), by = 'difference', how.many=2) 

       }   
    }
  })
  
  
  output$BENvsHC <- renderPlotly({
     print(suspects())
     plot_ly(x=1, y=1, ylab = 'HCViewer filtrated', xlab = 'Benford analysis filtrated')
  })

  
  ######################################################
  # GAMLSS-модель. Построение центильных кривых
  ###################################################### 
  
  # выбираем только фильтрованных пациентов
  filteredbase <- reactive({
    filteredbase = filtered.data.A()[filtered.data.A()$deletedA == 0,]
  })
  
  # селектбокс для параметра
  output$ParametersNamesSelect <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput("ParametersNamesSelect", "Выберите параметр:", choices=datanames(), selected = "R50")
  })
  
  # селектбокс для аргумента
  output$AgesNamesSelect <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput("AgesNamesSelect", "Выберите аргумент:", choices=datanames(), selected = input$AgeColName)
  })
  
  
  # строим центильные кривые по gamlss-модели
  output$centiles <- renderPlot({
    two.data.columns = 1
    fd = filteredbase()
    #print(names(fd))
    data.is.not.na = !(is.na(fd[[input$ParametersNamesSelect]])|is.na(fd[[input$AgesNamesSelect]]))
    #оставляем два столбца: аргумент и параметр
    two.data.columns$age = fd[[input$AgesNamesSelect]]
    two.data.columns$parameter = fd[[input$ParametersNamesSelect]]
    
    
    #вычисляем центильные кривые
    mod1<-quote(gamlss(parameter~pb(nage,df=p[1]),sigma.fo = ~pb(nage, p[2]), nu.fo = ~pb(nage, p[3], c.spar = c(-1.5,2.5)), tau.fo = ~pb(nage, p[4], c.spar = c(-1.5,2.5)), data = two.data.columns, family = BCT, control = gamlss.control(trace=FALSE)))	#?
    op = c(input$mu,input$sigma,input$nu,input$tau,input$lambda)		
    two.data.columns$agepower<-two.data.columns$age^op[5]	
    
    mBCT<-gamlss(parameter~pb(agepower, df=op[1]-2), sigma.fo=~pb(agepower, df=op[2]-2),nu.fo=~agepower, tau.fo=~agepower, family=BCT, data=two.data.columns, n.cyc = input$n.cyc)	#  (Box-Cox Transformation)
    save(mBCT, file = paste0(output.folder(),'mBCT.R'))
    cents = as.numeric(unlist(strsplit(input$centiles, ' ')))
   
    result <- centiles.with.report.table(mBCT, ylab= input$ParametersNamesSelect, xvar=two.data.columns$age, xlab=input$AgesNamesSelect, cent=cents, col.centiles = c(rep('black', length(cents) %/% 2), rep('red', 1), rep('black', length(cents) %/% 2)), legend = FALSE, main="", pch = 21, col = "gray", save = TRUE, plot=TRUE)
    
    
  })
})
