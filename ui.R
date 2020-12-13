library(shiny)
library(leaflet)
library(plotly)
library(zoo)

shinyUI(navbarPage(HTML("<font size =5 > HCViewer 1.1  <br /></font size > <font size = 1><p>&copy; OStarunova 2020<p></font size >"),
                   
                   ###################################################
                   #    ЗАГРУЗКА
                   ###################################################
                   tabPanel("Загрузка",
                            sidebarPanel(
                              
                              fluidRow(
                                column(6, ("Размер базы")),
                                column(6, verbatimTextOutput("alldatasize"))
                              ),
                              fileInput('file1', 'Выберите CSV-файл',
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv')),
                              
                              
                              checkboxInput('isDateFormat','Даты измерений в файле уже отформатированы', TRUE),             
                              checkboxGroupInput('Sex', 'Пол',
                                           c('Мужчины'='М',
                                             'Женщины'='Ж'),
                                           c('М', 'Ж'), inline = TRUE),
                              
                              
                              sliderInput("Age", "Возрастной диапазон",
                                          min = 1, max = 100, value = c(5,85)),
                              #checkboxGroupInput("ObsYear", "Годы обследования", 
                              #            c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'), c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'), inline = TRUE),
                              sliderInput("ObsDate",
                                          "Годы обследования",
                                          min = as.Date(as.yearmon('1001', "%y%m")),
                                          max = as.Date(as.yearmon('2012', "%y%m")),
                                          value = c(as.Date(as.yearmon('1001', "%y%m")),as.Date(as.yearmon('2012', "%y%m"))), 
                                          step = 28,
                                          timeFormat = "%b %Y", ),  
                              h4('В исходном файле:'),
                              checkboxInput('header', 'Заголовок', TRUE),
                              fluidRow(
                                column(4, radioButtons('sep', 'Колонки',
                                                       c(';',
                                                         ',',
                                                         'Tab' = '\t'),
                                                       ';')
                                ),
                                column(4, radioButtons('dec', 'Дес.разд.',
                                                       c('.',
                                                         ','),
                                                       ',')
                                ),
                                column(4, radioButtons('quote', 'Кавычки',
                                                       c('None'='',
                                                         '"',
                                                         "'"),
                                                       '')
                                )
                              ),# of fluid row
                              
                              
                              
                              uiOutput('dataColNames') 
                              
                              
                            ),#of sidebar panel
                            mainPanel(
                              tabsetPanel(type = "tabs", 
                                          tabPanel("Таблица", dataTableOutput('shortdata')),
                                          tabPanel("Возрастная структура", 
                                                   sliderInput("AgeBins", "Количество интервалов",min = 3, max = 100, value = 20), 
                                                   plotlyOutput("AgeHist")
                                          )
                                          
                              )#of tabset panel
                            ) #of main panel
                   ),#of tab panel Загрузка
                   
                   ###################################################
                   #    ПЕРВИЧНАЯ ФИЛЬТРАЦИЯ
                   ###################################################
                   
                   tabPanel("Фильтрация",
                            sidebarPanel(
                              checkboxInput("LimitsFilter", label = "Некорректные значения", value = TRUE),
                              checkboxInput("ClonesFilter", label = "Повторяющиеся значения", value = TRUE),
                              checkboxInput("AlreadyFiltrated", label = "Загружена уже размеченная база", value = TRUE),
                              checkboxInput("AlreadySummary", label = "Загружена уже готовая статистика удаленных по годам и ЦЗ", value = FALSE),
                              
                              
                              actionButton("TotalFilter", label = "Фильтровать"),
                              helpText(h4("Параметры первичной фильтрации")),
                              fileInput('fileLimits', h6('Выберите CSV-файл с границами (первая колонка - возраст, каждая следующая пара колонок - мин и макс значения параметров. Учитываются не более 30 параметров)'),
                                        accept=c('text/csv', 
                                                 'text/comma-separated-values,text/plain', 
                                                 '.csv')),
                              
                              checkboxInput('headerLimits', 'Заголовок', TRUE),
                              fluidRow(
                                column(4, radioButtons('sepLimits', h6('Колонки'),
                                                       c(';',
                                                         ',',
                                                         'Tab' = '\t'),
                                                       ';')
                                ),
                                column(4, radioButtons('decLimits', h6('Десятичный'),
                                                       c('.',
                                                         ','),
                                                       '.')
                                ),
                                column(4, radioButtons('quoteLimits', h6('Кавычки'),
                                                       c('None'='',
                                                         '"',
                                                         "'"),
                                                       '')
                                )
                              ),# of fluid row
                              #h3(checkboxInput("LimitsFilter", label = "Применить фильтрацию", value = FALSE)),
                              uiOutput('LimitsFilterNames'),
                              
                              #**************************************************
                              helpText(h4("Параметры вторичной фильтрации")),
                              #**************************************************
                              helpText(h6("Измерения параметров")),
                              uiOutput('CloneParametersNames'),
                              helpText(h6("отличающиеся от предыдущего (по времени) измерения в одном ЦЗ не более чем на")),
                              numericInput("cent.between.clones", "", 1, min = 1, max = 100, width = '35%'),
                              helpText(h6("процентов, считаются повторными измерениями одного пациента, и вся запись удаляется"))
                              
                              
                              
                            ),#of sidebar Panel
                            mainPanel(
                              tabsetPanel(type = "tabs", 
                                          tabPanel("Границы на графике", 
                                                   fluidRow(
                                                     column(9,
                                                            sliderInput("AgeLims", "Шкала возраста", min = 3, max = 100, value = c(5, 75)),
                                                            sliderInput("ParametersLims", "Шкала значений параметров", min = 3, max = 1000, value = c(0, 995)),
                                                            checkboxInput('LegendLims', 'Добавить легенду', TRUE),
                                                            plotOutput("plotLims")
                                                     ),
                                                     column(3,  uiOutput("showlimits"))
                                                     
                                                   )
                                                   
                                                   
                                          ),
                                          tabPanel("Сводка",
                                                  
                                                   dataTableOutput('DeletedSummary'),
                                                   tags$style(type="text/css", "#view tr:last-child {font-weight:bold;}")),
                                          tabPanel("Сводка по годам",
                                                   #fluidRow(column(6, radioButtons('RegsOrLPUs', '', c("Федеральные округа", "Регионы", "ЛПУ"), c("ЛПУ")))
                                                   #),
                                                   #plotOutput('PlotPercentByYears'),
                                                   
                                                   plotlyOutput('PlotTotalSummaryByYears'),
                                                   fluidRow(h4("Общее количество измерений по годам в ФО")),
                                                   dataTableOutput('TotalSummaryByYears'),
                                                   plotlyOutput('PlotDeletedSummaryByYears'),
                                                   fluidRow(h4("Процент некорректных измерений по годам в ФО")),
                                                   dataTableOutput('DeletedSummaryByYears'),
                                                   
                                                   fluidRow(h4("Общее количество некорректных измерений по регионам (в порядке увеличения % некорректных измерений)")),
                                                   tableOutput('DeletedSummaryByRegions')
                                                   #fluidRow(h4("Общий % удаленных по годам")),
                                                   #tableOutput('TotalPercentByYears')
                                                   ),
                                          tabPanel("На карте", h3("Интерактивная карта центров здоровья"), leafletOutput('interactiveMap'), uiOutput('RussiaMaps')),
                                          tabPanel("Качество ЛПУ", #googleChartsInit(),
                                                                   fluidRow(column(6, plotlyOutput('LPUSortByNumber'), uiOutput("sliderLPUsortByNumber")),
                                                                            column(6, plotlyOutput('LPUSortByDeleted'), uiOutput("sliderLPUsortByDeleted"))),
                                                   h3('Качество данных ЦЗ (размер маркеров пропорционален доле подделок)'),
                                                   plotlyOutput('LPUQualities') 
                                                              #click = "LPUQualitiesClick",
                                                              #brush = brushOpts(
                                                              #  id = "LPUQualitiesBrush"
                                                              #)),
                                                                                         
                                          ),
                                          
                                          tabPanel("Таблица границ", dataTableOutput('TableLims')),
                                          tabPanel("Удаленные", dataTableOutput('FirstFilterDataDeleted'))
                                          
                              )#of tabset panel
                            ) #of main panel   
                   ),#of TabPanel 
                   
                   ###################################################
                   #    ИНФОРМАЦИЯ ОБ ИСХОДНЫХ ДАННЫХ
                   ###################################################
                   
                   tabPanel("Информация о базе",
                            sidebarPanel(
                              uiOutput('Regions'),
                              uiOutput('HCname'),
                              
                              fluidRow(
                                column(6, "До фильтрации"),
                                column(6, verbatimTextOutput('chosendatasize.all'))
                              ),# of fluid row
                              
                              fluidRow(
                                column(6, "После фильтрации"),
                                column(6, verbatimTextOutput('chosendatasize.filteredA'))
                              ),# of fluid row
                              
                              uiOutput('ParametersNames')
                              
                            ), #of sidebarPanel
                            
                            mainPanel(
                              tabsetPanel(type = "tabs", 
                                          tabPanel("Бенфорд-анализ", helpText("Все"), plotOutput("BENhistsAll"), helpText("Correct"), plotOutput("BENhistsFiltered"), helpText("Incorrect"), plotOutput("BENhistsDeleted")),
                                          tabPanel("Сравнение методов", plotlyOutput("BENvsHC")),
                                          tabPanel("Сводка", helpText("В нефильтрованной базе"), tableOutput('Summary.all'), helpText("В фильтрованной базе"), tableOutput('Summary.filtered')),
                                          tabPanel("2D-Гистограммы", uiOutput("plot2D"))
                                          
                                          
                                          
                              )#of tabset panel
                            ) #of main panel
                   ),#of tabPanel "Информация о базе"
                   
                          
                   ###################################################
                   #    GAMLSS-модель
                   ###################################################
                   
                   tabPanel("Центильные кривые",
                            sidebarPanel(
                              
                              fluidRow(column(12, actionButton('gamlss', 'Построить'))),
                              
                              fluidRow(
                                column(12, h4("Порядок сплайна для:")),
                              fluidRow(
                                column(6, numericInput("mu", label = ("среднего, mu"), value = 8)),
                                column(6, numericInput("sigma", label = ("дисп., sigma"), value = 6))
                              ),# of fluid row
                              fluidRow(
                                column(6, numericInput("nu", label = ("асимм., nu"), value = 2)),
                                column(6, numericInput("tau", label = ("эксцесса, tau"), value = 2))
                              ),# of fluid row
                              fluidRow(
                                column(6, numericInput("lambda", label = ("Степень, lambda"), value = 0.25)),
                                column(6, numericInput("n.cyc", label = ("К-во итер., n.cyc"), value = 6))
                              ),# of fluid row
                              
                              fluidRow(
                                column(6, textInput("centiles", label = ("Центили lambda"), value = "3 10 25 50 75 90 97")),
                                column(6, numericInput("split", label = ("Погран. возраст"), value = 16))
                              )# of fluid row
                              
                              
                            )
                      ), #of sidebarPanel
                      mainPanel(
                              tabsetPanel(type = "tabs", 
                                          tabPanel('Centiles', 
                                                   fluidRow(
                                                     column(6, uiOutput('ParametersNamesSelect')),
                                                     column(6, uiOutput('AgesNamesSelect'))
                                                   )# of fluid row 
                                                   ,plotOutput('centiles')))
                                          
                                          
                                          
                              )#of tabset panel
                      ) #of main panel
                   )#of tabPanel "Центили"
                   
                            
                   
                  
)