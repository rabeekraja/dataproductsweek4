#
# This server downloads automatically currentdate-1 from 
# European Centre for Disease Prevention and Control and store in data folder.
# This server processing is dynamic and not optimized and expect some slowness
# in processing.
# It follows reactive model and displays dropdown based on continet input
# and then basd on country selected it displays three set of data.
# 1. Covid Summary
# 2. Sample data for country
# 3. Plotly interactive plot.

library(shiny)
library(data.table)
library(dplyr)
library(openxlsx)
library(R.cache)
#library(foreach)
#library(doParallel)
previousday <- Sys.Date()-1
documentdate <- format(previousday, "%Y-%m-%d")
xlsfile <-paste0("http://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",documentdate,".xlsx")
URL<-paste0(xlsfile)
filename<-paste0("data\\covidcases_",documentdate,".xlsx")
if (!file.exists(filename))
  download.file(URL,
                destfile=filename,
                mode="wb")
colClass <- c("date","integer","integer","integer","integer","integer","character","character","chracter","integer","character")
covidcases<- read.xlsx(filename,sheet=1)
#Convert to Datesas.character(covidcases$dateRep)
covidcases$dateRep<-as.Date(covidcases$dateRep-25569, origin="1970-01-01")
#Fix needed post May 16th Data as date format changed
#covidcases$dateRep<-as.Date(covidcases$dateRep,"%d/%m/%Y")
#print(covidcases$dateRep)
df_countrydata <- covidcases[c(7,11)]
df_pre <- covidcases[c(1,5,6,7,10,11)]
#Skipping Other data to make it simple
df <- df_pre %>%filter(!continentExp=="Other")
df_final<- df %>% group_by(date=dateRep,countryCode=countriesAndTerritories,continent=continentExp)
country_list <- unique(df_final[,8:length(df_final)]) %>% group_by(continent)
continents <- unique(country_list$continent)
#Prepare continent and country list
for(row in continents){
  cont <- row
  #print(cont)
  cntry <- country_list %>% filter(continent==cont)
  xlistBycol <- as.list(cntry)
  key <- list(cont);
  saveCache(xlistBycol,key)
}


# Define server logic required extract country information for a continent
shinyServer(function(input, output) {
  data <- reactiveVal()
 
  # Dynamic dropdown with countries under continent input
  observeEvent(input$submit1, ignoreNULL=T, ignoreInit=T, {
    runif(!is.null(input$continentInput) && is.null(input$covidcountry))
    #show_spinner()
    shinyjs::disable("submit2")
    shinyjs::hide("submit2")
    output$covidSumry <- renderUI({
      HTML("")
    })
    key <- list(input$continentInput)
    data <- loadCache(key, removeOldCache=TRUE)
    
    if (!is.null(data)) {
      cat("Loaded cached cntry\n")
      xlistBycol<- data;
    }
    # }else{
    # df_cont <- df_countrydata %>% filter(continentExp==input$continentInput) 
    # # Skip other data elements
    # countries <- df_cont[,-c(2)]
    # distinctCountry<- df_cont %>% group_by(countriesAndTerritories) %>% summarise(distinct_values = n_distinct(countriesAndTerritories))
    # # Prepare country list for dropdown
    # xlistBycol <- as.list(distinctCountry)
    # saveCache(xlistBycol,key)
    # }
      output$covidselect <- renderMenu({

              selectInput("covidcountry", "Select Country",
                        choices=xlistBycol,
                        multiple=FALSE,selected = xlistBycol[0]
            )
       
        })
      shinyjs::show("submit2")
      shinyjs::enable("submit2")

 })
  #Display summary,data,plot for given country
  observeEvent(input$submit2, ignoreNULL=T, ignoreInit=T, {
    #print(getCacheRootPath())
    shinyjs::show("text1")
    shinyjs::disable("submit1")
    shinyjs::disable("submit2")

    countrySelected <-input$covidcountry
    countryKey <- list(input$continentInput,countrySelected)
    dataCntry <- loadCache(countryKey, removeOldCache=TRUE)
    if (!is.null(dataCntry)) {
      cat("Loaded cached data\n")
     #print(dataCntry)
     # df_cntry<- dataCntry;
    }else{
   # ncores <- makeCluster(detectCores() - 1)
   # registerDoParallel(cores=ncores)
   
    df_cont <- df_final %>% filter(continent==input$continentInput)
    df_cont_final <- df_cont %>% filter(countryCode==input$covidcountry)
   # foreach(rwo = iter(df_cont,by="row"), .combine=c, .packages="dplyr") %dopar% {
    df_cntry <- df_cont_final %>%  summarise(cases=sum(cases),deaths=sum(deaths),population=mean(popData2018))
    df_cntry[,"Cum_Cases"] <- cumsum(df_cntry$cases)
    df_cntry[,"Cum_Deaths"] <- cumsum(df_cntry$deaths)
    dataCntry <- df_cntry
   # }
    
   # stopCluster(ncores)
    saveCache(dataCntry,countryKey)
    }
    total_cases <- sum(dataCntry$cases)
    total_deaths <- sum(dataCntry$deaths)
    #print(dataCntry)
    output$covidSumry <- renderUI({
      str1 <- paste("<b>Covid-19 Summary for<b>:<u>", input$covidcountry,"</u> As of:<b>",documentdate,"</b>")
      str2 <- paste('Total Cases :<p style="color:Orange;">',total_cases,'</p>')
      str3 <- paste('Deaths:<p style="color:red;">',total_deaths,'</p>')
      HTML(paste(str1, str2,str3, sep = '<br/>'))
    })
    dataCntry$date <- format(dataCntry$date,'%Y-%m-%d')
    #Skip unwanted data
    output$dsplyData <- renderTable({
      dataCntryTab <- dataCntry %>% filter(countryCode==input$covidcountry)
      dataCntryTab[,-c(6)]
      })
    # Prepare Plot Tab
    
    #fit <- lm(Cum_Cases ~ Cum_Deaths, data = df_cntry)
    output$covidPlot <- renderPlotly({
    

      plot_ly(dataCntry, x = ~date, y=~cases, type="scatter",color=I('Green'), mode = "markers",name = '+Ve Cases') %>% add_trace(y = ~deaths,color=I('Orange'), name = 'Deaths') %>%
        add_trace(y = ~Cum_Cases,color=I('Blue'), name = 'Cumulative Cases')%>%
        add_trace(y = ~Cum_Deaths,color=I('DarkRed'), name = 'Cumulative Deaths')%>%
        layout(title = "Covid-19 cases and deaths",
               xaxis = list(title = "Date"),
               yaxis = list( title = "Cases & Deaths"))})
    shinyjs::hide("text1")
    shinyjs::enable("submit1")
    shinyjs::enable("submit2")

  })
  

})
