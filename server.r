library(shiny)
library(rCharts)
library(dplyr)
library(stringr)
library(XLConnect)
library(data.table)

shinyServer(function(input, output, session){
  #### Read excel data into server.r
  ###by sheet
  df_1 <- reactive({
    file1 <- input$file
    if(is.null(file1)){return(NULL)} 
    data = readWorksheet(loadWorkbook(file1$datapath), sheet = 1, header = TRUE)
    data$分析報告開始 <- as.Date(data$分析報告開始)
    data
  })
  df_2 <- reactive({
    file1 <- input$file
    if(is.null(file1)){return(NULL)} 
    data = readWorksheet(loadWorkbook(file1$datapath), sheet = 2, header = TRUE)
    data$分析報告開始 <- as.Date(data$分析報告開始)
    data
  })
  
  ### to save associate data into age/device sheet
  df_age <- reactive({
    if("年齡" %in% colnames(df_1())){
      df_age<-df_1()
     }else{
      df_age<-df_2()
    }
    return(df_age)
 })
  df_device <- reactive({
    if("版位" %in% colnames(df_1())){
      df_device<-df_1()
    }else{
      df_device<-df_2()
    }
    return(df_device)
  })
  
  ### save associate data into different df, such as: install, fanpage, post, websiteclick
  ## install df 
  df_age_install <- reactive({
    if("行動應用程式安裝次數" %in% colnames(df_age())){
      df_age_install<-df_age()
      df_age_install$行動應用程式安裝次數<-as.numeric(df_age_install$行動應用程式安裝次數)
      df_age_install$行動應用程式安裝次數[is.na(df_age_install$行動應用程式安裝次數)]<-0
#       colnames(df_age_install)[which(colnames(df_age_install) == '分析報告開始')] <- 'Date'
#       colnames(df_age_install)[which(colnames(df_age_install) == '分析報告結束')] <- 'End_Date'
#       colnames(df_age_install)[which(colnames(df_age_install) == '廣告名稱')] <- 'Ad'
#       colnames(df_age_install)[which(colnames(df_age_install) == '年齡')] <- "Age"
#       colnames(df_age_install)[which(colnames(df_age_install) == '性別')] <- 'Gender'
#       colnames(df_age_install)[which(colnames(df_age_install) == '行')] <- 'Campaign'
#       colnames(df_age_install)[which(colnames(df_age_install) == '廣告組合名稱')] <- 'Ad.Set'
#       colnames(df_age_install)[which(colnames(df_age_install) == '觸及人數')] <- 'Reach'
#       colnames(df_age_install)[which(colnames(df_age_install) == '不重複點擊次數.全部.')] <- 'Clicks'
#       colnames(df_age_install)[which(colnames(df_age_install) == '行動應用程式安裝次數')] <- 'Conversions'
#       colnames(df_age_install)[which(colnames(df_age_install) == '支出金額..USD.')] <- 'Spent'
      setnames(df_age_install,old=c('分析報告開始','分析報告結束','廣告名稱','年齡','性別','行銷活動名稱','廣告組合名稱','觸及人數','不重複點擊次數.全部.','行動應用程式安裝次數'
                                    ,'支出金額..USD.'),new=c('Date','End_Date','Ad',"Age","Gender","Campaign","Ad.Set","Reach","Clicks","Conversions",'Spent'))
      df_age_install
    }else{
      return(NULL)
    }
  })
  df_device_install <- reactive({
    if("行動應用程式安裝次數" %in% colnames(df_device())){
      df_device_install<-df_device()
      df_device_install$行動應用程式安裝次數<-as.numeric(df_device_install$行動應用程式安裝次數)
      df_device_install$行動應用程式安裝次數[is.na(df_device_install$行動應用程式安裝次數)]<-0
#       colnames(df_device_install)[which(colnames(df_device_install) == '分析報告開始')] <- 'Date'
#       colnames(df_device_install)[which(colnames(df_device_install) == '分析報告結束')] <- 'End_Date'
#       colnames(df_device_install)[which(colnames(df_device_install) == '廣告名稱')] <- 'Ad'
#       colnames(df_device_install)[which(colnames(df_device_install) == '版位')] <- 'Placement'
#       colnames(df_device_install)[which(colnames(df_device_install) == '瀏覽次數裝置')] <- 'Device'
#       colnames(df_device_install)[which(colnames(df_device_install) == '行銷活動名稱')] <- 'Campaign'
#       colnames(df_device_install)[which(colnames(df_device_install) == '廣告組合名稱')] <- 'Ad.Set'
#       colnames(df_device_install)[which(colnames(df_device_install) == '觸及人數')] <- 'Reach'
#       colnames(df_device_install)[which(colnames(df_device_install) == '不重複點.全部.')] <- 'Clicks'
#       colnames(df_device_install)[which(colnames(df_device_install) == '行動應用程式安裝次數')] <- 'Conversions'
#       colnames(df_device_install)[which(colnames(df_device_install) == '支出金額..USD.')] <- 'Spent'
      setnames(df_device_install,old=c('分析報告開始','分析報告結束','廣告名稱','版位','瀏覽次數裝置','行銷活動名稱','廣告組合名稱','觸及人數','不重複點擊次數.全部.','行動應用程式安裝次數'
                                       ,'支出金額..USD.'),new=c("Date","End_Date","Ad","Placement","Device","Campaign","Ad.Set","Reach","Clicks","Conversions","Spent"))
      df_device_install
    }else{
      return(NULL)
    }
  })

  # websiteclick df
    df_age_websiteclick <- reactive({
      if("連結點擊次數" %in% colnames(df_age())){
        df_age_websiteclick<-df_age()
        setnames(df_age_websiteclick,old = c('分析報告開始','行銷活動名稱','廣告組合名稱','廣告名稱','年齡','性別','成果','觸及人數','點擊次數.全部.'
                                        ,'支出金額..USD.'),new = c('Date','Campaign','Ad.Set','Ad','Age','Gender','Link_Click','Reach','Clicks','Spent'))
        df_age_websiteclick
      }else{
        return(NULL)
      }
    })
    df_device_websiteclick <- reactive({
      if("連結點擊次數" %in% colnames(df_device())){
        df_device_websiteclick<-df_device()
        setnames(df_device_websiteclick,old = c('分析報告開始'
            ,'廣告組合名稱','廣告名稱','版位','瀏覽次數裝置','成果','觸及人數','點擊次數.全部.'
            ,'行銷活動名稱','支出金額..USD.'), new = c('Date','Ad.Set','Ad','Placement','Device','Link_Click','Reach','Clicks','Campaign','Spent'))
        df_device_websiteclick
      }else{
        return(NULL)
      }
    })
  
comment<-{  
#   ## fanpage df
#   df_age_fanpage <- reactive({
#     if("粉絲專頁的讚" %in% colnames(df_age())){
#       df_age_fanpage<-df_age()
#       df_age_fanpage
#     }else{
#       return(NULL)
#     }
#   })
#   df_device_fanpage <- reactive({
#     if("粉絲專頁的讚" %in% colnames(df_device())){
#       df_device_fanpage<-df_device()
#       df_device_fanpage
#     }else{
#       return(NULL)
#     }
#   })
#   
#   ## post df
#   df_age_post <- reactive({
#     if("貼文互動" %in% colnames(df_age())){
#       df_age_post<-df_age()
#       df_age_post
#     }else{
#       return(NULL)
#     }
#   })
#   df_device_post <- reactive({
#     if("貼文互動" %in% colnames(df_device())){
#       df_device_post<-df_device()
#       df_device_post
#     }else{
#       return(NULL)
#     }
#   })
#   
#   
}
    
  observe({
    updateSelectInput(session,  "y_input",
      choices=unique(df_1()$廣告組合名稱))
  })
  observe({
    updateDateRangeInput(session, "date_range",
                         start = min(df_1()$分析報告開始),
                         end = max(df_1()$分析報告開始),
                         min = min(df_1()$分析報告開始),
                         max = max(df_1()$分析報告開始))
  })
  
  df_1_creative<- reactive({
    data<-df_1()
    x<-dim(data)[2]+1
    data<-cbind(data,str_split_fixed(data$廣告名稱, "0", 2)[,1])
    colnames(data)[x]<-c("Creative_Set")
    data$Creative_Set<-as.character(data$Creative_Set)
    data
  })
  df_3 <- reactive({
    file1 <- input$file
    if(is.null(file1)){return(NULL)} 
    data = readWorksheet(loadWorkbook(file1$datapath), sheet = 1, header = TRUE)
    x<-dim(data)[2]+1
    data<-cbind(data,str_split_fixed(data$廣告名稱, "0", 2)[,1])
    colnames(data)[x]<-c("Creative_Set")
    data<-cbind(data,str_split_fixed(data$廣告組合名稱, "_", 6)[,1])
    colnames(data)[x+1]<-c("Creative_Set_2")
    data$Creative_Set<-as.character(data$Creative_Set)
    data$Creative_Set_2<-as.character(data$Creative_Set_2)
    data
  })
  
  observe({
    updateSelectInput(session,  "creative_input",
      choices=unique(df_3()$Creative_Set_2))
  })
  ###################################廣告組合related###########################################
  ####Install####
  ###Table
  #generate
  data_selected_adset_table <-reactive({
    data <- df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    selected_adset<- input$y_input
    data <- data %>% 
       filter(Ad.Set==selected_adset) %>%
       filter(Date <= max_date) %>% 
       filter(Date >= min_date) %>%
      group_by(Ad.Set) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cpc=Total_Spent/Total_Clicks, 
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Cvr=Total_Conversions/Total_Clicks)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #output the table in the ui
  output$adset_table <- renderDataTable({
    data_selected_adset_table()
  })
  
  ###Plot
  ##Basic Plot(selected)
  #generate a df for the plot
  data_basic_selected_Date<-reactive({
    data <- df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    selected_adset<- input$y_input
    data <- data %>% 
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Date) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cpc=Total_Spent/Total_Clicks, 
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Cvr=Total_Conversions/Total_Clicks)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data<-transform(data, Date = as.character(Date))
    data
  })
  #use the df to draw the basic plots
  output$h5 <- renderChart2({
    if(is.null(df_age_install())){return ()}
    h5 <- Highcharts$new()
    h5$xAxis(categories = data_basic_selected_Date()$Date)
    h5$yAxis(list(list(title = list(text = 'Conversions')), 
                  list(title = list(text = 'CPI'), opposite = TRUE)))
    h5$series(name = 'Conversions', type = 'column', color = '#4572A7',
              data = data_basic_selected_Date()$Total_Conversions)
    h5$series(name = 'CPI', type = 'spline', color = '#006600',
              data = data_basic_selected_Date()$Total_Cpi,
              yAxis = 1)
    h5$title(text =("Conversion/CPI vs Date"))
    return(h5)
  })
  output$h6 <- renderChart2({
    if(is.null(df_age_install())){return ()}
    
    h6 <- Highcharts$new()
    h6$xAxis(categories = data_basic_selected_Date()$Date)
    h6$yAxis(list(list(title = list(text = 'Clicks')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h6$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_basic_selected_Date()$Total_Clicks)
    h6$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_basic_selected_Date()$Total_Cpc,
              yAxis = 1)
    h6$title(text =("Clicks/CPC vs Date"))
    return(h6)
  })
  output$h7 <- renderChart2({
    if(is.null(df_age_install())){return ()}
    
    h7 <- Highcharts$new()
    h7$xAxis(categories = data_basic_selected_Date()$Date)
    h7$yAxis(list(list(title = list(text = 'CPI')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h7$series(name = 'CPI', type = 'spline', color = '#006600',
              data = data_basic_selected_Date()$Total_Cpi)
    h7$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_basic_selected_Date()$Total_Cpc,
              yAxis = 1)
    h7$title(text =("CPI/CPC vs Date"))
    return(h7)
  })
  
  ##Age Plot(selected)
  #generate a age df
  data_selected_age<- reactive({
    data<-df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    selected_adset<- input$y_input
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Age) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot age
  output$h16 <- renderChart2({
    h16 <- Highcharts$new()
    h16$xAxis(categories = data_selected_age()$Age)
    h16$yAxis(list(list(title = list(text = 'Conversions'))
                  ,list(title = list(text = 'CVR'), opposite = TRUE)
                  ,list(title = list(text = 'CPI'), opposite = TRUE)))
    h16$series(name = 'Conversions', type = 'column', color = '#4572A7',
              data = data_selected_age()$Total_Conversions)
    h16$series(name = 'CVR', type = 'spline', color = '#33CC00',
              data = data_selected_age()$Total_Cvr,
              yAxis = 1)
    h16$series(name = 'CPI', type = 'spline', color = '#006600',
              data = data_selected_age()$Total_Cpi,
              yAxis = 2)
    h16$title(text =("Conversions/CVR/CPI vs Age"))
    return(h16)
  })
  output$h17 <- renderChart2({
    h17 <- Highcharts$new()
    h17$xAxis(categories = data_selected_age()$Age)
    h17$yAxis(list(list(title = list(text = 'Clicks'))
                  ,list(title = list(text = 'CTR'), opposite = TRUE) 
                  ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h17$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_selected_age()$Total_Clicks)
    h17$series(name = 'CTR', type = 'spline', color = '#FF9655',
              data = data_selected_age()$Total_Ctr,
              yAxis = 1)
    h17$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_selected_age()$Total_Cpc,
              yAxis = 2)
    h17$title(text =("Clicks/CTR/CPC vs Age"))
    return(h17)
  })
  
  ##Gender Plot(selected)
  #generate a gender df
  data_selected_gender<- reactive({
    data<-df_age_install()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot gender
  output$h20 <- renderChart2({
    h20 <- Highcharts$new()
    h20$xAxis(categories = data_selected_gender()$Gender)
    h20$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE))) 
    h20$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_selected_gender()$Total_Conversions)
    h20$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_selected_gender()$Total_Cvr,
               yAxis = 1)
    h20$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_selected_gender()$Total_Cpi,
               yAxis = 2)
    h20$title(text =("Conversions/CVR/CPI vs Gender"))
    return(h20)
  })
  output$h21 <- renderChart2({
    h21 <- Highcharts$new()
    h21$xAxis(categories = data_selected_gender()$Gender)
    h21$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h21$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_gender()$Total_Clicks)
    h21$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_selected_gender()$Total_Ctr,
               yAxis = 1)
    h21$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_selected_gender()$Total_Cpc,
               yAxis = 2)
    h21$title(text =("Clicks/CTR/CPC vs Gender"))
    return(h21)
  })
  
  ##Gender Age Plot(selected)
  #generate a gender age df
  data_selected_gender_age<- reactive({
    data<-df_age_install()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender, Age) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) 
    data
  })
  #plot gender age
  output$n3 <- renderChart2({
    n3 <- nPlot(Total_Conversions ~ Gender, group = "Age", data = data_selected_gender_age(), type = "multiBarChart")
    #n1$set(width = 400, height = 400) # mk changed width to 800 and height to 500
    n3$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
    n3$set(title = "Conversions---Gender vs Age")
    n3$addParams(dom="n3")
    return(n3)
  })
  output$n4 <- renderChart2({
    n4 <- nPlot(Total_Clicks ~ Gender, group = "Age", data = data_selected_gender_age(), type = "multiBarChart")
    n4$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
    n4$set(title = "Clicks---Gender vs Age")
    n4$addParams(dom="n4")
    return(n4)
  })
  
  ##Creative Plot(selected)
  #generate a creative df
  data_selected_creative<- reactive({
    data<-df_age_install()
    selected_adset<- input$y_input
    x<-dim(data)[2]+1
    y<-dim(data)[2]+7
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Campaign, "_", 5)[,2:3])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,5:6])
    data<-cbind(data,str_split_fixed(data$Ad, "0", 2)[,1])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,1])
    colnames(data)[x:y]<-c("App","OS","TA","Baha_Category","Bid_Type","Creative_Set","Creative_Set_2")
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Ad) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot creative
  output$h18 <- renderChart2({
    h18 <- Highcharts$new()
    h18$xAxis(categories = data_selected_creative()$Ad)
    h18$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE)))
    h18$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_selected_creative()$Total_Conversions)
    h18$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_selected_creative()$Total_Cvr,
               yAxis = 1)
    h18$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_selected_creative()$Total_Cpi,
               yAxis = 2)
    h18$title(text =("Conversions/CVR/CPI vs Creative"))
    return(h18)
  })
  output$h19 <- renderChart2({
    h19 <- Highcharts$new()
    h19$xAxis(categories = data_selected_creative()$Ad)
    h19$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h19$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_creative()$Total_Clicks)
    h19$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_selected_creative()$Total_Ctr,
               yAxis = 1)
    h19$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_selected_creative()$Total_Cpc,
               yAxis = 2)
    h19$title(text =("Clicks/CTR/CPC vs Creative"))
    return(h19)
  })
  
  ##Placement Plot(selected)
  #generate a placement df
  data_selected_placement<- reactive({
    data<-df_device_install()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    #     data2<-cbind(data,Placement_Type=data$Placement)
    #     data2$Placement_Type<-as.character(data2$Placement_Type)
    #     data2$Placement_Type[data2$Placement_Type=="行動裝置上的動態消息"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="行動裝置的Instagram"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="第三方行動應用程式上的行動廣告聯播網"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="桌面電腦的動態消息"] <-c("桌面電腦")
    #     data2$Placement_Type[data2$Placement_Type=="桌面電腦的右欄廣告"] <-c("桌面電腦")
    #     data2$Placement_Type[data2$Placement_Type=="桌面版電腦的首頁右欄廣告"] <-c("桌面電腦")
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Placement) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot placement
  output$h27 <- renderChart2({
    h27 <- Highcharts$new()
    h27$xAxis(categories = data_selected_placement()$Placement)
    h27$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE))) 
    h27$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_selected_placement()$Total_Conversions)
    h27$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_selected_placement()$Total_Cvr,
               yAxis = 1)
    h27$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_selected_placement()$Total_Cpi,
               yAxis = 2)
    h27$title(text =("Conversions/CVR/CPI vs Placement"))
    return(h27)
  })
  output$h28 <- renderChart2({
    h28 <- Highcharts$new()
    h28$xAxis(categories = data_selected_placement()$Placement)
    h28$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h28$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_placement()$Total_Clicks)
    h28$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_selected_placement()$Total_Ctr,
               yAxis = 1)
    h28$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_selected_placement()$Total_Cpc,
               yAxis = 2)
    h28$title(text =("Clicks/CTR/CPC vs Placement"))
    return(h28)
  })
  
  ##Device Plot
  #generate a device df
  data_selected_device<- reactive({
    data<-df_device_install()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Device) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot device
  output$h29 <- renderChart2({
    h29 <- Highcharts$new()
    h29$xAxis(categories = data_selected_device()$Device)
    h29$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE))) 
    h29$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_selected_device()$Total_Conversions)
    h29$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_selected_device()$Total_Cvr,
               yAxis = 1)
    h29$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_selected_device()$Total_Cpi,
               yAxis = 2)
    h29$title(text =("Conversions/CVR/CPI vs Device"))
    return(h29)
  })
  output$h30 <- renderChart2({
    h30 <- Highcharts$new()
    h30$xAxis(categories = data_selected_device()$Device)
    h30$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h30$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_device()$Total_Clicks)
    h30$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_selected_device()$Total_Ctr,
               yAxis = 1)
    h30$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_selected_device()$Total_Cpc,
               yAxis = 2)
    h30$title(text =("Clicks/CTR/CPC vs Device"))
    return(h30)
  })
  
  ##############WebsiteClick###################
  ###Table
  #generate a adset df (selected)
  data_selected_adset_table_websiteclick<-reactive({
    data <- df_age_websiteclick()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data <- data %>% 
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  
  #output the table in the ui
  output$adset_table_websiteclick <- renderDataTable({
    data_selected_adset_table_websiteclick()
  })
  ###Plot
  ##Bacis Plot(selected)
  #generate a df for the plot
  data_basic_selected_Date_websiteclick<-reactive({
    data <- df_age_websiteclick()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data <- data %>% 
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Date) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data<-transform(data, Date = as.character(Date))
    data
  })
  #use the df to draw the basic plots
  output$h46 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h46 <- Highcharts$new()
    h46$xAxis(categories = data_basic_selected_Date_websiteclick()$Date)
    h46$yAxis(list(list(title = list(text = 'Reach')), 
                   list(title = list(text = 'CPM'), opposite = TRUE)))
    h46$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_basic_selected_Date_websiteclick()$Total_Reach)
    h46$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_basic_selected_Date_websiteclick()$Total_Cpm,
               yAxis = 1)
    h46$title(text =("Reach/CPM vs Date"))
    return(h46)
  })
  output$h47 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    
    h47 <- Highcharts$new()
    h47$xAxis(categories = data_basic_selected_Date_websiteclick()$Date)
    h47$yAxis(list(list(title = list(text = 'Clicks')), 
                   list(title = list(text = 'CPC'), opposite = TRUE)))
    h47$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_basic_selected_Date_websiteclick()$Total_Clicks)
    h47$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_basic_selected_Date_websiteclick()$Total_Cpc,
               yAxis = 1)
    h47$title(text =("Clicks/CPC vs Date"))
    return(h47)
  })
  output$h48 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    
    h48 <- Highcharts$new()
    h48$xAxis(categories = data_basic_selected_Date_websiteclick()$Date)
    h48$yAxis(list(list(title = list(text = 'CPM')), 
                   list(title = list(text = 'CPC'), opposite = TRUE)))
    h48$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_basic_selected_Date_websiteclick()$Total_Cpm)
    h48$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_basic_selected_Date_websiteclick()$Total_Cpc,
               yAxis = 1)
    h48$title(text =("CPM/CPC vs Date"))
    return(h48)
  })
  
  ##Age Plot(selected)
  #generate a age df
  data_selected_age_websiteclick<- reactive({
    data<-df_age_websiteclick()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Age) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot age
  output$h49 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h49 <- Highcharts$new()
    h49$xAxis(categories = data_selected_age_websiteclick()$Age)
    h49$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE)))
    h49$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_selected_age_websiteclick()$Total_Reach)
    h49$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_selected_age_websiteclick()$Total_Cpm,
               yAxis = 1)
    h49$title(text =("Reach/CPM vs Age"))
    return(h49)
  })
  output$h50 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h50 <- Highcharts$new()
    h50$xAxis(categories = data_selected_age_websiteclick()$Age)
    h50$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE) 
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h50$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_age_websiteclick()$Total_Clicks)
    h50$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_selected_age_websiteclick()$Total_Ctr,
               yAxis = 1)
    h50$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_selected_age_websiteclick()$Total_Cpc,
               yAxis = 2)
    h50$title(text =("Clicks/CTR/CPC vs Age"))
    return(h50)
  })
  
  ##Gender Plot(selected)
  #generate a gender df
  data_selected_gender_websiteclick<- reactive({
    data<-df_age_websiteclick()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot gender
  output$h51 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h51 <- Highcharts$new()
    h51$xAxis(categories = data_selected_gender_websiteclick()$Gender)
    h51$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE))) 
    h51$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_selected_gender_websiteclick()$Total_Reach)
    h51$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_selected_gender_websiteclick()$Total_Cpm,
               yAxis = 1)
    h51$title(text =("Reach/CPM vs Gender"))
    return(h51)
  })
  output$h52 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h52 <- Highcharts$new()
    h52$xAxis(categories = data_selected_gender_websiteclick()$Gender)
    h52$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h52$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_gender_websiteclick()$Total_Clicks)
    h52$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_selected_gender_websiteclick()$Total_Ctr,
               yAxis = 1)
    h52$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_selected_gender_websiteclick()$Total_Cpc,
               yAxis = 2)
    h52$title(text =("Clicks/CTR/CPC vs Gender"))
    return(h52)
  })
  
  ##Gender Age Plot(selected)
  #generate a gender age df
  data_selected_gender_age_websiteclick<- reactive({
    data<-df_age_websiteclick()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender, Age) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) 
    data
  })
  #plot gender age
  output$n7 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    n7 <- nPlot(Total_Reach ~ Gender, group = "Age", data = data_selected_gender_age_websiteclick(), type = "multiBarChart")
    #n1$set(width = 400, height = 400) # mk changed width to 800 and height to 500
    n7$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
    n7$set(title = "Reach---Gender vs Age")
    n7$addParams(dom="n7")
    return(n7)
  })
  output$n8 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    n8 <- nPlot(Total_Clicks ~ Gender, group = "Age", data = data_selected_gender_age_websiteclick(), type = "multiBarChart")
    n8$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
    n8$set(title = "Clicks---Gender vs Age")
    n8$addParams(dom="n8")
    return(n8)
  })
  
  ##CreativeSet Plot(selected)
  #generate a creativeset df
  data_selected_creative_websiteclick<- reactive({
    data<-df_age_websiteclick()
    selected_adset<- input$y_input
    x<-dim(data)[2]+1
    y<-dim(data)[2]+3
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,1:2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,5])
    colnames(data)[x:y]<-c("Creative_Set","TA","Bid_Type")
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Creative_Set) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot creativeset
  output$h53 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h53 <- Highcharts$new()
    h53$xAxis(categories = data_selected_creative_websiteclick()$Creative_Set)
    h53$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE)))
    h53$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_selected_creative_websiteclick()$Total_Reach)
    h53$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_selected_creative_websiteclick()$Total_Cpm,
               yAxis = 1)
    h53$title(text =("Reach/CPM vs CreativeSet"))
    return(h53)
  })
  output$h54 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h54 <- Highcharts$new()
    h54$xAxis(categories = data_selected_creative_websiteclick()$Creative_Set)
    h54$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h54$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_creative_websiteclick()$Total_Clicks)
    h54$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_selected_creative_websiteclick()$Total_Ctr,
               yAxis = 1)
    h54$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_selected_creative_websiteclick()$Total_Cpc,
               yAxis = 2)
    h54$title(text =("Clicks/CTR/CPC vs CreativeSet"))
    return(h54)
  })
  
  ##Placement Plot(selected)
  #generate a placement df
  data_selected_placement_websiteclick<- reactive({
    data<-df_device_websiteclick()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    #     data2<-cbind(data,Placement_Type=data$Placement)
    #     data2$Placement_Type<-as.character(data2$Placement_Type)
    #     data2$Placement_Type[data2$Placement_Type=="行動裝置上的動態消息"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="行動裝置的Instagram"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="第三方行動應用程式上的行動廣告聯播網"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="桌面電腦的動態消息"] <-c("桌面電腦")
    #     data2$Placement_Type[data2$Placement_Type=="桌面電腦的右欄廣告"] <-c("桌面電腦")
    #     data2$Placement_Type[data2$Placement_Type=="桌面版電腦的首頁右欄廣告"] <-c("桌面電腦")
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Placement) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot placement
  output$h55 <- renderChart2({
    if(is.null(df_device_websiteclick())){return ()}
    h55 <- Highcharts$new()
    h55$xAxis(categories = data_selected_placement_websiteclick()$Placement)
    h55$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE))) 
    h55$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_selected_placement_websiteclick()$Total_Reach)
    h55$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_selected_placement_websiteclick()$Total_Cpm,
               yAxis = 1)
    h55$title(text =("Reach/CPM vs Placement"))
    return(h55)
  })
  output$h56 <- renderChart2({
    if(is.null(df_device_websiteclick())){return ()}
    h56 <- Highcharts$new()
    h56$xAxis(categories = data_selected_placement_websiteclick()$Placement)
    h56$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h56$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_selected_placement_websiteclick()$Total_Clicks)
    h56$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_selected_placement_websiteclick()$Total_Ctr,
               yAxis = 1)
    h56$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_selected_placement_websiteclick()$Total_Cpc,
               yAxis = 2)
    h56$title(text =("Clicks/CTR/CPC vs Placement"))
    return(h56)
  })
  
  ##Device Plot(selected)
  #generate a device df
  data_selected_device_websiteclick<- reactive({
    data<-df_device_websiteclick()
    selected_adset<- input$y_input
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Ad.Set==selected_adset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Device) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot device
  output$h57 <- renderChart2({
    if(is.null(df_device_websiteclick())){return ()}
    h57 <- Highcharts$new()
    h57$xAxis(categories = data_device_websiteclick()$Device)
    h57$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE))) 
    h57$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_device_websiteclick()$Total_Reach)
    h57$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_device_websiteclick()$Total_Cpm,
               yAxis = 1)
    h57$title(text =("Reach/CPM vs Device"))
    return(h57)
  })
  output$h58 <- renderChart2({
    if(is.null(df_device_websiteclick())){return ()}
    h58 <- Highcharts$new()
    h58$xAxis(categories = data_device_websiteclick()$Device)
    h58$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h58$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_device_websiteclick()$Total_Clicks)
    h58$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_device_websiteclick()$Total_Ctr,
               yAxis = 1)
    h58$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_device_websiteclick()$Total_Cpc,
               yAxis = 2)
    h58$title(text =("Clicks/CTR/CPC vs Device"))
    return(h58)
  })
  
  
  #######################################Campaign#######################################
  ##############Install###################
  ###generate a campaign df 
  data_campaign_table<-reactive({
    data <- df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data <- data %>% 
       filter(Date <= max_date) %>% 
       filter(Date >= min_date) %>%
      group_by(Campaign) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cpc=Total_Spent/Total_Clicks, 
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Cvr=Total_Conversions/Total_Clicks)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  
  #output the table in the ui
  output$campaign_table <- renderDataTable({
    data_campaign_table()
  })
  
  ##############WebsiteClick###################
#   ###generate a campaign df 
  data_campaign_table_websiteclick<-reactive({
    data <- df_age_websiteclick()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data <- data %>% 
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Campaign) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  
  #output the table in the ui
  output$campaign_table_websiteclick <- renderDataTable({
    data_campaign_table_websiteclick()
  })
  
  ######################################Summary########################################
  ##############Install###################
  ###Table
  #generate a summary df 
  data_summary_table<-reactive({
    data <- df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data <- data %>% 
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cpc=Total_Spent/Total_Clicks, 
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Cvr=Total_Conversions/Total_Clicks)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                              data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                              data$Total_Spent, data$Total_Cpc)
    data
  })
  
  #output the table in the ui
  output$summary_table <- renderDataTable({
    data_summary_table()
  })
  ###Plot
  ##Bacis Plot
  #generate a df for the plot
  data_basic_Date<-reactive({
    data <- df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data <- data %>% 
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Date) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions, 
             Total_Cpc=Total_Spent/Total_Clicks, 
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Cvr=Total_Conversions/Total_Clicks)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data<-transform(data, Date = as.character(Date))
    data
  })
  #use the df to draw the basic plots
  output$h1 <- renderChart2({
    if(is.null(df_age_install())){return ()}
    h1 <- Highcharts$new()
    h1$xAxis(categories = data_basic_Date()$Date)
    h1$yAxis(list(list(title = list(text = 'Conversions')), 
                  list(title = list(text = 'CPI'), opposite = TRUE)))
    h1$series(name = 'Conversions', type = 'column', color = '#4572A7',
              data = data_basic_Date()$Total_Conversions)
    h1$series(name = 'CPI', type = 'spline', color = '#006600',
              data = data_basic_Date()$Total_Cpi,
              yAxis = 1)
    h1$title(text =("Conversion/CPI vs Date"))
    return(h1)
  })
  output$h2 <- renderChart2({
    if(is.null(df_age_install())){return ()}
    
    h2 <- Highcharts$new()
    h2$xAxis(categories = data_basic_Date()$Date)
    h2$yAxis(list(list(title = list(text = 'Clicks')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h2$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_basic_Date()$Total_Clicks)
    h2$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_basic_Date()$Total_Cpc,
              yAxis = 1)
    h2$title(text =("Clicks/CPC vs Date"))
    return(h2)
  })
  output$h3 <- renderChart2({
    if(is.null(df_age_install())){return ()}
    
    h3 <- Highcharts$new()
    h3$xAxis(categories = data_basic_Date()$Date)
    h3$yAxis(list(list(title = list(text = 'CPI')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h3$series(name = 'CPI', type = 'spline', color = '#006600',
              data = data_basic_Date()$Total_Cpi)
    h3$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_basic_Date()$Total_Cpc,
              yAxis = 1)
    h3$title(text =("CPI/CPC vs Date"))
    return(h3)
  })
  
  ##Age Plot
  #generate a age df
  data_age<- reactive({
    data<-df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Age) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot age
  output$h8 <- renderChart2({
    h8 <- Highcharts$new()
    h8$xAxis(categories = data_age()$Age)
    h8$yAxis(list(list(title = list(text = 'Conversions'))
                  ,list(title = list(text = 'CVR'), opposite = TRUE)
                  ,list(title = list(text = 'CPI'), opposite = TRUE)))
    h8$series(name = 'Conversions', type = 'column', color = '#4572A7',
              data = data_age()$Total_Conversions)
    h8$series(name = 'CVR', type = 'spline', color = '#33CC00',
              data = data_age()$Total_Cvr,
              yAxis = 1)
    h8$series(name = 'CPI', type = 'spline', color = '#006600',
              data = data_age()$Total_Cpi,
              yAxis = 2)
    h8$title(text =("Conversions/CVR/CPI vs Age"))
    return(h8)
  })
  output$h9 <- renderChart2({
    h9 <- Highcharts$new()
    h9$xAxis(categories = data_age()$Age)
    h9$yAxis(list(list(title = list(text = 'Clicks'))
                  ,list(title = list(text = 'CTR'), opposite = TRUE) 
                  ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h9$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_age()$Total_Clicks)
    h9$series(name = 'CTR', type = 'spline', color = '#FF9655',
              data = data_age()$Total_Ctr,
              yAxis = 1)
    h9$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_age()$Total_Cpc,
              yAxis = 2)
    h9$title(text =("Clicks/CTR/CPC vs Age"))
    return(h9)
  })
  
  ##Gender Plot
  #generate a gender df
  data_gender<- reactive({
    data<-df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot gender
  output$h14 <- renderChart2({
    h14 <- Highcharts$new()
    h14$xAxis(categories = data_gender()$Gender)
    h14$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE))) 
    h14$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_gender()$Total_Conversions)
    h14$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_gender()$Total_Cvr,
               yAxis = 1)
    h14$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_gender()$Total_Cpi,
               yAxis = 2)
    h14$title(text =("Conversions/CVR/CPI vs Gender"))
    return(h14)
  })
  output$h15 <- renderChart2({
    h15 <- Highcharts$new()
    h15$xAxis(categories = data_gender()$Gender)
    h15$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h15$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_gender()$Total_Clicks)
    h15$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_gender()$Total_Ctr,
               yAxis = 1)
    h15$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_gender()$Total_Cpc,
               yAxis = 2)
    h15$title(text =("Clicks/CTR/CPC vs Gender"))
    return(h15)
  })
  
  ##Gender Age Plot
  #generate a gender age df
  data_gender_age<- reactive({
    data<-df_age_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender, Age) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) 
    data
  })
  #plot gender age
  output$n1 <- renderChart2({
    n1 <- nPlot(Total_Conversions ~ Gender, group = "Age", data = data_gender_age(), type = "multiBarChart")
    #n1$set(width = 400, height = 400) # mk changed width to 800 and height to 500
    n1$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
    n1$set(title = "Conversions---Gender vs Age")
    n1$addParams(dom="n1")
    return(n1)
  })
  output$n2 <- renderChart2({
    n2 <- nPlot(Total_Clicks ~ Gender, group = "Age", data = data_gender_age(), type = "multiBarChart")
    n2$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
    n2$set(title = "Clicks---Gender vs Age")
    n2$addParams(dom="n2")
    return(n2)
  })
  
  ##CreativeSet Plot
  #generate a creativeset df
  data_creative<- reactive({
    data<-df_age_install()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+7
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Campaign, "_", 5)[,2:3])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,5:6])
    data<-cbind(data,str_split_fixed(data$Ad, "0", 2)[,1])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,1])
    colnames(data)[x:y]<-c("App","OS","TA","Baha_Category","Bid_Type","Creative_Set","Creative_Set_2")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Creative_Set_2) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot creativeset
  output$h10 <- renderChart2({
    h10 <- Highcharts$new()
    h10$xAxis(categories = data_creative()$Creative_Set_2)
    h10$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE)))
    h10$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_creative()$Total_Conversions)
    h10$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_creative()$Total_Cvr,
               yAxis = 1)
    h10$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_creative()$Total_Cpi,
               yAxis = 2)
    h10$title(text =("Conversions/CVR/CPI vs CreativeSet"))
    return(h10)
  })
  output$h11 <- renderChart2({
    h11 <- Highcharts$new()
    h11$xAxis(categories = data_creative()$Creative_Set_2)
    h11$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h11$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_creative()$Total_Clicks)
    h11$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_creative()$Total_Ctr,
               yAxis = 1)
    h11$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_creative()$Total_Cpc,
               yAxis = 2)
    h11$title(text =("Clicks/CTR/CPC vs CreativeSet"))
    return(h11)
  })
  
  ##Placement Plot
  #generate a placement df
  data_placement<- reactive({
    data<-df_device_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
#     data2<-cbind(data,Placement_Type=data$Placement)
#     data2$Placement_Type<-as.character(data2$Placement_Type)
#     data2$Placement_Type[data2$Placement_Type=="行動裝置上的動態消息"] <-c("行動裝置")
#     data2$Placement_Type[data2$Placement_Type=="行動裝置的Instagram"] <-c("行動")
#     data2$Placement_Type[data2$Placement_Type=="第三方行動應用程式上的行動廣告聯播網"] <-c("行動裝置")
#     data2$Placement_Type[data2$Placement_Type=="桌面電腦的動態消息"] <-c("桌面電腦")
#     data2$Placement_Type[data2$Placement_Type=="桌面電腦的右欄廣告"] <-c("桌面電腦")
#     data2$Placement_Type[data2$Placement_Type=="桌面版電腦的首頁右欄廣告"] <-c("桌面電腦")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Placement) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot placement
  output$h23 <- renderChart2({
    h23 <- Highcharts$new()
    h23$xAxis(categories = data_placement()$Placement)
    h23$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE))) 
    h23$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_placement()$Total_Conversions)
    h23$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_placement()$Total_Cvr,
               yAxis = 1)
    h23$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_placement()$Total_Cpi,
               yAxis = 2)
    h23$title(text =("Conversions/CVR/CPI vs Placement"))
    return(h23)
  })
  output$h24 <- renderChart2({
    h24 <- Highcharts$new()
    h24$xAxis(categories = data_placement()$Placement)
    h24$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h24$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_placement()$Total_Clicks)
    h24$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_placement()$Total_Ctr,
               yAxis = 1)
    h24$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_placement()$Total_Cpc,
               yAxis = 2)
    h24$title(text =("Clicks/CTR/CPC vs Placement"))
    return(h24)
  })
  
  ##Device Plot
  #generate a device df
  data_device<- reactive({
    data<-df_device_install()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Device) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot device
  output$h25 <- renderChart2({
    h25 <- Highcharts$new()
    h25$xAxis(categories = data_device()$Device)
    h25$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE))) 
    h25$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_device()$Total_Conversions)
    h25$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_device()$Total_Cvr,
               yAxis = 1)
    h25$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_device()$Total_Cpi,
               yAxis = 2)
    h25$title(text =("Conversions/CVR/CPI vs Device"))
    return(h25)
  })
  output$h26 <- renderChart2({
    h26 <- Highcharts$new()
    h26$xAxis(categories = data_device()$Device)
    h26$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h26$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_device()$Total_Clicks)
    h26$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_device()$Total_Ctr,
               yAxis = 1)
    h26$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_device()$Total_Cpc,
               yAxis = 2)
    h26$title(text =("Clicks/CTR/CPC vs Device"))
    return(h26)
  })
  
  ##############WebsiteClick###################
  ###Table
  #generate a summary df 
  data_summary_table_websiteclick<-reactive({
    data <- df_age_websiteclick()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data <- data %>% 
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  
  #output the table in the ui
  output$summary_table_websiteclick <- renderDataTable({
    data_summary_table_websiteclick()
  })
  ###Plot
  ##Bacis Plot
  #generate a df for the plot
  data_basic_Date_websiteclick<-reactive({
    data <- df_age_websiteclick()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data <- data %>% 
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Date) %>%
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data<-transform(data, Date = as.character(Date))
    data
  })
  #use the df to draw the basic plots
  output$h33 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h33 <- Highcharts$new()
    h33$xAxis(categories = data_basic_Date_websiteclick()$Date)
    h33$yAxis(list(list(title = list(text = 'Reach')), 
                  list(title = list(text = 'CPM'), opposite = TRUE)))
    h33$series(name = 'Reach', type = 'column', color = '#4572A7',
              data = data_basic_Date_websiteclick()$Total_Reach)
    h33$series(name = 'CPM', type = 'spline', color = '#006600',
              data = data_basic_Date_websiteclick()$Total_Cpm,
              yAxis = 1)
    h33$title(text =("Reach/CPM vs Date"))
    return(h33)
  })
  output$h34 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    
    h34 <- Highcharts$new()
    h34$xAxis(categories = data_basic_Date_websiteclick()$Date)
    h34$yAxis(list(list(title = list(text = 'Clicks')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h34$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_basic_Date_websiteclick()$Total_Clicks)
    h34$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_basic_Date_websiteclick()$Total_Cpc,
              yAxis = 1)
    h34$title(text =("Clicks/CPC vs Date"))
    return(h34)
  })
  output$h35 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    
    h35 <- Highcharts$new()
    h35$xAxis(categories = data_basic_Date_websiteclick()$Date)
    h35$yAxis(list(list(title = list(text = 'CPM')), 
                  list(title = list(text = 'CPC'), opposite = TRUE)))
    h35$series(name = 'CPM', type = 'spline', color = '#006600',
              data = data_basic_Date_websiteclick()$Total_Cpm)
    h35$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_basic_Date_websiteclick()$Total_Cpc,
              yAxis = 1)
    h35$title(text =("CPM/CPC vs Date"))
    return(h35)
  })
  
  ##Age Plot
  #generate a age df
  data_age_websiteclick<- reactive({
    data<-df_age_websiteclick()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Age) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot age
  output$h36 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h36 <- Highcharts$new()
    h36$xAxis(categories = data_age_websiteclick()$Age)
    h36$yAxis(list(list(title = list(text = 'Reach'))
                  ,list(title = list(text = 'CPM'), opposite = TRUE)))
    h36$series(name = 'Reach', type = 'column', color = '#4572A7',
              data = data_age_websiteclick()$Total_Reach)
    h36$series(name = 'CPM', type = 'spline', color = '#006600',
              data = data_age_websiteclick()$Total_Cpm,
              yAxis = 1)
    h36$title(text =("Reach/CPM vs Age"))
    return(h36)
  })
  output$h37 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h37 <- Highcharts$new()
    h37$xAxis(categories = data_age_websiteclick()$Age)
    h37$yAxis(list(list(title = list(text = 'Clicks'))
                  ,list(title = list(text = 'CTR'), opposite = TRUE) 
                  ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h37$series(name = 'Clicks', type = 'column', color = '#4572A7',
              data = data_age_websiteclick()$Total_Clicks)
    h37$series(name = 'CTR', type = 'spline', color = '#FF9655',
              data = data_age_websiteclick()$Total_Ctr,
              yAxis = 1)
    h37$series(name = 'CPC', type = 'spline', color = '#AA4643',
              data = data_age_websiteclick()$Total_Cpc,
              yAxis = 2)
    h37$title(text =("Clicks/CTR/CPC vs Age"))
    return(h37)
  })
  
  ##Gender Plot
  #generate a gender df
  data_gender_websiteclick<- reactive({
    data<-df_age_websiteclick()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot gender
  output$h38 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h38 <- Highcharts$new()
    h38$xAxis(categories = data_gender_websiteclick()$Gender)
    h38$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE))) 
    h38$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_gender_websiteclick()$Total_Reach)
    h38$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_gender_websiteclick()$Total_Cpm,
               yAxis = 1)
    h38$title(text =("Reach/CPM vs Gender"))
    return(h38)
  })
  output$h39 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h39 <- Highcharts$new()
    h39$xAxis(categories = data_gender_websiteclick()$Gender)
    h39$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h39$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_gender_websiteclick()$Total_Clicks)
    h39$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_gender_websiteclick()$Total_Ctr,
               yAxis = 1)
    h39$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_gender_websiteclick()$Total_Cpc,
               yAxis = 2)
    h39$title(text =("Clicks/CTR/CPC vs Gender"))
    return(h39)
  })
  
  ##Gender Age Plot
  #generate a gender age df
  data_gender_age_websiteclick<- reactive({
    data<-df_age_websiteclick()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Gender, Age) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) 
    data
  })
  #plot gender age
  output$n5 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    n5 <- nPlot(Total_Reach ~ Gender, group = "Age", data = data_gender_age_websiteclick(), type = "multiBarChart")
    #n1$set(width = 400, height = 400) # mk changed width to 800 and height to 500
    n5$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
    n5$set(title = "Reach---Gender vs Age")
    n5$addParams(dom="n5")
    return(n5)
  })
  output$n6 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    n6 <- nPlot(Total_Clicks ~ Gender, group = "Age", data = data_gender_age_websiteclick(), type = "multiBarChart")
    n6$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle_styled.html"
    n6$set(title = "Clicks---Gender vs Age")
    n6$addParams(dom="n6")
    return(n6)
  })
  
  ##CreativeSet Plot
  #generate a creativeset df
  data_creative_websiteclick<- reactive({
    data<-df_age_websiteclick()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+3
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,1:2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,5])
    colnames(data)[x:y]<-c("Creative_Set","TA","Bid_Type")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Creative_Set) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot creativeset
  output$h40 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h40 <- Highcharts$new()
    h40$xAxis(categories = data_creative_websiteclick()$Creative_Set)
    h40$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE)))
    h40$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_creative_websiteclick()$Total_Reach)
    h40$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_creative_websiteclick()$Total_Cpm,
               yAxis = 1)
    h40$title(text =("Reach/CPM vs CreativeSet"))
    return(h40)
  })
  output$h41 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    h41 <- Highcharts$new()
    h41$xAxis(categories = data_creative_websiteclick()$Creative_Set)
    h41$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h41$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_creative_websiteclick()$Total_Clicks)
    h41$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_creative_websiteclick()$Total_Ctr,
               yAxis = 1)
    h41$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_creative_websiteclick()$Total_Cpc,
               yAxis = 2)
    h41$title(text =("Clicks/CTR/CPC vs CreativeSet"))
    return(h41)
  })
  
  ##Placement Plot
  #generate a placement df
  data_placement_websiteclick<- reactive({
    data<-df_device_websiteclick()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    #     data2<-cbind(data,Placement_Type=data$Placement)
    #     data2$Placement_Type<-as.character(data2$Placement_Type)
    #     data2$Placement_Type[data2$Placement_Type=="行動裝置上的動態消息"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="行動裝置的Instagram"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="第三方行動應用程式上的行動廣告"] <-c("行動裝置")
    #     data2$Placement_Type[data2$Placement_Type=="桌面電腦的動態消息"] <-c("桌面電腦")
    #     data2$Placement_Type[data2$Placement_Type=="桌面電腦的右欄廣告"] <-c("桌面電腦")
    #     data2$Placement_Type[data2$Placement_Type=="桌面版電腦的首頁右欄廣告"] <-c("桌面電腦")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Placement) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot placement
  output$h42 <- renderChart2({
    if(is.null(df_device_websiteclick())){return ()}
    h42 <- Highcharts$new()
    h42$xAxis(categories = data_placement_websiteclick()$Placement)
    h42$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE))) 
    h42$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_placement_websiteclick()$Total_Reach)
    h42$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_placement_websiteclick()$Total_Cpm,
               yAxis = 1)
    h42$title(text =("Reach/CPM vs Placement"))
    return(h42)
  })
  output$h43 <- renderChart2({
    if(is.null(df_device_websiteclick())){return ()}
    h43 <- Highcharts$new()
    h43$xAxis(categories = data_placement_websiteclick()$Placement)
    h43$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h43$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_placement_websiteclick()$Total_Clicks)
    h43$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_placement_websiteclick()$Total_Ctr,
               yAxis = 1)
    h43$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_placement_websiteclick()$Total_Cpc,
               yAxis = 2)
    h43$title(text =("Clicks/CTR/CPC vs Placement"))
    return(h43)
  })
  
  ##Device Plot
  #generate a device df
  data_device_websiteclick<- reactive({
    data<-df_device_websiteclick()
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Device) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot device
  output$h44 <- renderChart2({
    if(is.null(df_device_websiteclick())){return ()}
    h44 <- Highcharts$new()
    h44$xAxis(categories = data_device_websiteclick()$Device)
    h44$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE))) 
    h44$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_device_websiteclick()$Total_Reach)
    h44$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_device_websiteclick()$Total_Cpm,
               yAxis = 1)
    h44$title(text =("Reach/CPM vs Device"))
    return(h44)
  })
  output$h45 <- renderChart2({
    if(is.null(df_device_websiteclick())){return ()}
    h45 <- Highcharts$new()
    h45$xAxis(categories = data_device_websiteclick()$Device)
    h45$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h45$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_device_websiteclick()$Total_Clicks)
    h45$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_device_websiteclick()$Total_Ctr,
               yAxis = 1)
    h45$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_device_websiteclick()$Total_Cpc,
               yAxis = 2)
    h45$title(text =("Clicks/CTR/CPC vs Device"))
    return(h45)
  })
  
  ######################################受眾########################################
  ##############Install###################
  ###Table
  #generate a table
  data_ta_table<- reactive({
    data<-df_age_install()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+6
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Campaign, "_", 5)[,2:3])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,5:6])
    data<-cbind(data,str_split_fixed(data$Ad, "0", 2)[,1])
    colnames(data)[x:y]<-c("App","OS","TA","Baha_Category","Bid_Type","Creative_Set")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(TA) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot table
  output$ta_table <- renderDataTable({
    data_ta_table()
  })
  
  ###Plot
  ##TA vs date
  data_ta_date<- reactive({
    data<-df_age_install()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+6
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Campaign, "_", 5)[,2:3])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,5:6])
    data<-cbind(data,str_split_fixed(data$Ad, "0", 2)[,1])
    colnames(data)[x:y]<-c("App","OS","TA","Baha_Category","Bid_Type","Creative_Set")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(TA,Date) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data<-transform(data, Date = as.character(Date))
    data
  })
  #use the dataframe to plot out the basic plots
  output$h4 <- renderChart2({
    if(is.null(df_age_install())){return ()}
    
    h4 <- Highcharts$new()
    h4 <- hPlot(x = "Date", y = "Total_Conversions",
                data = data_ta_date(), type = "line", group = "TA")
    h4$title(text =("Conversions vs Date (by Category)"))
    return(h4)
  })
  
  output$h22 <- renderChart2({
    if(is.null(df_age_install())){return ()}
    
    h22 <- Highcharts$new()
    h22 <- hPlot(x = "Date", y = "Total_Clicks",
                 data = data_ta_date(), type = "line", group = "TA")
    h22$title(text =("Clicks vs Date (by Category)"))
    return(h22)
  })
  
  ##############WebsiteClick###################
  ###Table
  #generate a table
  data_ta_table_websiteclick<- reactive({
    data<-df_age_websiteclick()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+3
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,1:2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,5])
    colnames(data)[x:y]<-c("Creative_Set","TA","Bid_Type")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(TA) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot table
  output$ta_table_websiteclick <- renderDataTable({
    data_ta_table_websiteclick()
  })
  
  ###Plot
  ##TA vs date
  data_ta_date_websiteclick<- reactive({
    data<-df_age_websiteclick()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+3
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,1:2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,5])
    colnames(data)[x:y]<-c("Creative_Set","TA","Bid_Type")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(TA, Date) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data<-transform(data, Date = as.character(Date))
    data
  })
  #use the dataframe to plot out the basic plots
  output$h59 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    
    h59 <- Highcharts$new()
    h59 <- hPlot(x = "Date", y = "Total_Reach",
                data = data_ta_date_websiteclick(), type = "line", group = "TA")
    h59$title(text =("Reach vs Date (by Category)"))
    return(h59)
  })
  
  output$h60 <- renderChart2({
    if(is.null(df_age_websiteclick())){return ()}
    
    h60 <- Highcharts$new()
    h60 <- hPlot(x = "Date", y = "Total_Clicks",
                 data = data_ta_date_websiteclick(), type = "line", group = "TA")
    h60$title(text =("Clicks vs Date (by Category)"))
    return(h60)
  })
  
  
  #######################################素材#######################################
  ##############Install###################
  ###Table
  #generate a table
  data_creative_table<- reactive({
    data<-df_age_install()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+7
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Campaign, "_", 5)[,2:3])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,5:6])
    data<-cbind(data,str_split_fixed(data$Ad, "0", 2)[,1])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,1])
    colnames(data)[x:y]<-c("App","OS","TA","Baha_Category","Bid_Type","Creative_Set","Creative_Set_2")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Creative_Set_2) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot table
  output$creative_table <- renderDataTable({
    data_creative_table()
  })
  
  ###Plot
  ##separate each creativeset in a plot dynamically
  #generate a df 
  data_creative_ad<- reactive({
    data<-df_age_install()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+7
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    selected_creativeset<- input$creative_input
    data<-cbind(data,str_split_fixed(data$Campaign, "_", 5)[,2:3])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,5:6])
    data<-cbind(data,str_split_fixed(data$Ad, "0", 2)[,1])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 6)[,1])
    colnames(data)[x:y]<-c("App","OS","TA","Baha_Category","Bid_Type","Creative_Set","Creative_Set_2")
    data<- data %>%
      filter(Creative_Set_2==selected_creativeset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Ad) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Conversions=sum(Conversions),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpi=Total_Spent/Total_Conversions,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Cvr=Total_Conversions/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach)
    data$Total_Cpi <- ifelse(data$Total_Cpi == "Inf", 
                             data$Total_Spent, data$Total_Cpi)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot it dynamically
  
  output$h31<-renderChart2({
    h31 <- Highcharts$new()
    h31$xAxis(categories = data_creative_ad()$Ad)
    h31$yAxis(list(list(title = list(text = 'Conversions'))
                   ,list(title = list(text = 'CVR'), opposite = TRUE)
                   ,list(title = list(text = 'CPI'), opposite = TRUE))) 
    h31$series(name = 'Conversions', type = 'column', color = '#4572A7',
               data = data_creative_ad()$Total_Conversions)
    h31$series(name = 'CVR', type = 'spline', color = '#33CC00',
               data = data_creative_ad()$Total_Cvr,
               yAxis = 1)
    h31$series(name = 'CPI', type = 'spline', color = '#006600',
               data = data_creative_ad()$Total_Cpi,
               yAxis = 2)
    h31$title(text =("Conversions/CVR/CPI vs Ad"))
    return(h31)
  })
  
  output$h32 <- renderChart2({
    h32 <- Highcharts$new()
    h32$xAxis(categories = data_creative_ad()$Ad)
    h32$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h32$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_creative_ad()$Total_Clicks)
    h32$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_creative_ad()$Total_Ctr,
               yAxis = 1)
    h32$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_creative_ad()$Total_Cpc,
               yAxis = 2)
    h32$title(text =("Clicks/CTR/CPC vs Ad"))
    return(h32)
  })
  
  ##############WebsiteClick###################
  ###Table
  #generate a table
  data_creative_table_websiteclick<- reactive({
    data<-df_age_websiteclick()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+3
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,1:2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,5])
    colnames(data)[x:y]<-c("Creative_Set","TA","Bid_Type")
    data<- data %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Creative_Set) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot table
  output$creative_table_websiteclick <- renderDataTable({
    data_creative_table_websiteclick()
  })
  
  ###Plot
  ##separate each creativeset in a plot dynamically
  #generate a df 
  data_creative_ad_websiteclick<- reactive({
    data<-df_age_websiteclick()
    x<-dim(data)[2]+1
    y<-dim(data)[2]+3
    min_date<-input$date_range[1]
    max_date<-input$date_range[2]
    selected_creativeset<- input$creative_input
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,1:2])
    data<-cbind(data,str_split_fixed(data$Ad.Set, "_", 5)[,5])
    colnames(data)[x:y]<-c("Creative_Set","TA","Bid_Type")
    data<- data %>%
      filter(Creative_Set==selected_creativeset) %>%
      filter(Date <= max_date) %>% 
      filter(Date >= min_date) %>%
      group_by(Ad) %>% 
      summarize(Total_Reach=sum(Reach),
                Total_Clicks=sum(Clicks),
                Total_Link_Click=sum(Link_Click),
                Total_Spent=sum(Spent)) %>%
      mutate(Total_Cpm=Total_Spent*1000/Total_Reach,
             Total_Cpc=Total_Spent/Total_Clicks,
             Total_Ctr=Total_Clicks/Total_Reach,
             Total_Link_Ctr=Total_Link_Click/Total_Reach,
             Total_Link_Cpc=Total_Spent/Total_Link_Click)
    data$Total_Link_Cpc <- ifelse(data$Total_Link_Cpc == "Inf", 
                                  data$Total_Spent, data$Total_Link_Cpc)
    data$Total_Cpc <- ifelse(data$Total_Cpc == "Inf", 
                             data$Total_Spent, data$Total_Cpc)
    data
  })
  #plot it dynamically
  
  output$h61<-renderChart2({
    h61 <- Highcharts$new()
    h61$xAxis(categories = data_creative_ad_websiteclick()$Ad)
    h61$yAxis(list(list(title = list(text = 'Reach'))
                   ,list(title = list(text = 'CPM'), opposite = TRUE))) 
    h61$series(name = 'Reach', type = 'column', color = '#4572A7',
               data = data_creative_ad_websiteclick()$Total_Reach)
    h61$series(name = 'CPM', type = 'spline', color = '#006600',
               data = data_creative_ad_websiteclick()$Total_Cpm,
               yAxis = 1)
    h61$title(text =("Reach/CPM vs Ad"))
    return(h61)
  })
  
  output$h62 <- renderChart2({
    h62 <- Highcharts$new()
    h62$xAxis(categories = data_creative_ad_websiteclick()$Ad)
    h62$yAxis(list(list(title = list(text = 'Clicks'))
                   ,list(title = list(text = 'CTR'), opposite = TRUE)
                   ,list(title = list(text = 'CPC'), opposite = TRUE)))
    h62$series(name = 'Clicks', type = 'column', color = '#4572A7',
               data = data_creative_ad_websiteclick()$Total_Clicks)
    h62$series(name = 'CTR', type = 'spline', color = '#FF9655',
               data = data_creative_ad_websiteclick()$Total_Ctr,
               yAxis = 1)
    h62$series(name = 'CPC', type = 'spline', color = '#AA4643',
               data = data_creative_ad_websiteclick()$Total_Cpc,
               yAxis = 2)
    h62$title(text =("Clicks/CTR/CPC vs Ad"))
    return(h62)
  })
  
  
  
  #######################################tb area#######################################
  output$tb <- renderUI({
    if(is.null(df_1())){
      h4("歡迎歡迎!!"
      ,br()
      ,br()
      ,br()      
      ,br()
      ,br()
      ,h5("目前支援檔案類型：")
      ,h5("1. 應用程式安裝")
      ,h5("2. 網站點擊")
      )
    }
    else{
      tabsetPanel(tabPanel("組合",
                           dataTableOutput(outputId=paste(
                             if(!is.null(df_age_install())){"adset_table"
                             }else if(!is.null(df_age_websiteclick())){"adset_table_websiteclick"})),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h5"
                             }else if(!is.null(df_age_websiteclick())){"h46"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h6"
                             }else if(!is.null(df_age_websiteclick())){"h47"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h7"
                             }else if(!is.null(df_age_websiteclick())){"h48"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h16"
                             }else if(!is.null(df_age_websiteclick())){"h49"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h17"
                             }else if(!is.null(df_age_websiteclick())){"h50"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h20"
                             }else if(!is.null(df_age_websiteclick())){"h51"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h21"
                             }else if(!is.null(df_age_websiteclick())){"h52"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h18"
                             }else if(!is.null(df_age_websiteclick())){"h53"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h19"
                             }else if(!is.null(df_age_websiteclick())){"h54"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h27"
                             }else if(!is.null(df_age_websiteclick())){"h55"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h28"
                             }else if(!is.null(df_age_websiteclick())){"h56"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h29"
                             }else if(!is.null(df_age_websiteclick())){"h57"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h30"
                             }else if(!is.null(df_age_websiteclick())){"h58"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"n3"
                             }else if(!is.null(df_age_websiteclick())){"n7"})
                             , "nvd3"),
                           br(),
                           br(),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"n4"
                             }else if(!is.null(df_age_websiteclick())){"n8"})
                             , "nvd3")
                           ),
                  tabPanel("活動",
                           dataTableOutput(outputId=paste(
                             if(!is.null(df_age_install())){"campaign_table"
                             }else if(!is.null(df_age_websiteclick())){"campaign_table_websiteclick"})
                            )
                           ),
                  tabPanel("總覽",
                           dataTableOutput(outputId=paste(
                             if(!is.null(df_age_install())){"summary_table"
                             }else if(!is.null(df_age_websiteclick())){"summary_table_websiteclick"})),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h1"
                               }else if(!is.null(df_age_websiteclick())){"h33"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h2"
                             }else if(!is.null(df_age_websiteclick())){"h34"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h3"
                             }else if(!is.null(df_age_websiteclick())){"h35"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h8"
                             }else if(!is.null(df_age_websiteclick())){"h36"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h9"
                             }else if(!is.null(df_age_websiteclick())){"h37"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h14"
                             }else if(!is.null(df_age_websiteclick())){"h38"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h15"
                             }else if(!is.null(df_age_websiteclick())){"h39"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h10"
                             }else if(!is.null(df_age_websiteclick())){"h40"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h11"
                             }else if(!is.null(df_age_websiteclick())){"h41"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h23"
                             }else if(!is.null(df_age_websiteclick())){"h42"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h24"
                             }else if(!is.null(df_age_websiteclick())){"h43"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h25"
                             }else if(!is.null(df_age_websiteclick())){"h44"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"h26"
                             }else if(!is.null(df_age_websiteclick())){"h45"})
                             , "highcharts"),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"n1"
                             }else if(!is.null(df_age_websiteclick())){"n5"})
                             , "nvd3"),
                           br(),
                           br(),
                           showOutput(paste(
                             if(!is.null(df_age_install())){"n2"
                             }else if(!is.null(df_age_websiteclick())){"n6"})
                             , "nvd3")
                            ),
                   tabPanel("受眾",
                            dataTableOutput(outputId=paste(
                              if(!is.null(df_age_install())){"ta_table"
                              }else if(!is.null(df_age_websiteclick())){"ta_table_websiteclick"})),
                            showOutput(paste(
                              if(!is.null(df_age_install())){"h4"
                              }else if(!is.null(df_age_websiteclick())){"h59"})
                              , "highcharts"),
                            showOutput(paste(
                              if(!is.null(df_age_install())){"h22"
                              }else if(!is.null(df_age_websiteclick())){"h60"})
                              , "highcharts")
                            ),
                   tabPanel("素材",
                            dataTableOutput(outputId=paste(
                              if(!is.null(df_age_install())){"creative_table"
                              }else if(!is.null(df_age_websiteclick())){"creative_table_websiteclick"})),
                            showOutput(paste(
                              if(!is.null(df_age_install())){"h31"
                              }else if(!is.null(df_age_websiteclick())){"h61"})
                              , "highcharts"),
                            showOutput(paste(
                              if(!is.null(df_age_install())){"h32"
                              }else if(!is.null(df_age_websiteclick())){"h62"})
                              , "highcharts")
                            )
      )}
  })
  

})