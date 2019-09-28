#Load required packages and library for building Rshiny dashboard
install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("semantic.dashboard")
install.packages("zipcode")
install.packages("leaflet")
library(shiny)
#library(shinydashboard)
#library(semantic.dashboard)
library(dplyr) #This is required for showing filtering data
library(ggplot2) #This is required for showing plots
library(plotly)
library(zipcode)
library(leaflet)
library(htmltools)
#library(RColorBrewer)

ui<-  fluidPage(
  # Sidebar Layout for inputs ----
  sidebarLayout(
    # Sidebar Panel for inputs ----
    sidebarPanel ( 
      fileInput(
        "file1",
        "Choose Boston property dataset",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      checkboxGroupInput("residential", "Choose Residential Property Type",
                         choices = list("1-family" = "R1","2-family" ="R2","3-family" ="R3","7 or more family" ="A"),
                         selected ="R1"),
      
      conditionalPanel(condition = "input.ts != 'nf' & input.ts != 'nb' ",sliderInput("year", "Choose Year Range",
                  min = 1700, max = 2018,value = c(1900,2018))),
      conditionalPanel(condition = "input.ts != 'ic' & input.ts != 'nf' & input.ts != 'nb' & input.ts != 'ht'",sliderInput("blur", "Change Transparency", min = 0, max = 1,value = 0.5))
       ),
    
    mainPanel(tabsetPanel(id = 'ts',
      # Output layout
      tabPanel("Living Area",value ='la',
               plotlyOutput("la_plot")),
      tabPanel("Interior Condition",value = 'ic',
               plotlyOutput("plot")
               
      ),
      tabPanel("Number of Floors",value = 'nf',
               plotlyOutput("plot2")
               
      ),
      tabPanel("Number of Bedrooms",value = 'nb',
               leafletOutput("plot4")
               
      ),
      tabPanel("Heating Type",value = 'ht',
               plotlyOutput("plot6"),
               plotlyOutput("plot5")
               
      )
    ))))


# Define server logic to read selected file ----
server <- function(input, output, session) {
  property_dataset <-reactive ({
    req(input$file1)
    data <- read.csv(input$file1$datapath )
    
    return(data)})
  
  #filtering data
  property_data_filter <-reactive({
    selected <-c(input$residential)
    property_data = property_dataset()
    property_data_fil <- subset(property_data, property_data$LU%in%selected)
    
    return(property_data_fil)
  })
  #Plotting Interior Condition
  output$plot <- renderPlotly({
    property_data_clean = property_data_filter()
    property_data_clean <- subset(property_data_clean, YR_BUILT > input$year[1] & YR_BUILT < input$year[2])
    property_data_clean_1 <- property_data_clean %>% group_by(R_INT_CND) %>% summarise(MedianAV_BLDG = median(AV_BLDG))
    property_data_clean_1<-property_data_clean_1 %>% 
      mutate(R_INT_CND = case_when(
        R_INT_CND == "P"  ~ "Poor",
        R_INT_CND == "E"  ~ "Excellent",
        R_INT_CND == "A" ~ "Average",
        R_INT_CND == "G" ~ "Good",
        R_INT_CND == "F" ~ "Fair"
      )
      )
    
    property_data_clean_1 <- arrange(property_data_clean_1, MedianAV_BLDG, R_INT_CND) 
    
    #colfunc <- colorRampPalette(c("lightblue", "blue",,"darkblue",steelblue"))
    #pal <- colfunc(max(property_data_clean_1$MedianAV_BLDG))[property_data_clean_1$MedianAV_BLDG]
    
    #marker = list(color =  pal)
    # plot_ly(property_data_clean_1, x = ~MedianAV_BLDG, y = ~reorder(R_INT_CND,-MedianAV_BLDG), type = 'bar',
    #         marker = list(color = c('rgba(128,128,128,0.8)',pal)) ,
    plot_ly(property_data_clean_1, x = ~MedianAV_BLDG, y = ~reorder(R_INT_CND,-MedianAV_BLDG), type = 'bar',
            marker = list(color = c('rgba(255,0,0,0.6)', 'rgba(255,0,0,0.3)',
                                    'rgba(128,128,0,0.4)', 'rgba(0,128,0,0.6)',
                                    'rgba(0,128,0,1)')) ,
            name = 'InteriorCond')%>%
      layout(yaxis = list(title = 'Interior Condition',showgrid =FALSE),
             xaxis = list(title = 'Median Total Assessed Building Value',showgrid =FALSE ), barmode = 'group')
    
  })
  #Plotting Living Area  
  output$la_plot <- renderPlotly({  
    property_data_clean = property_data_filter()
    property_data_clean <- subset(property_data_clean, YR_BUILT > input$year[1] & YR_BUILT < input$year[2])
    property_data_clean_1 <- property_data_clean %>% group_by(AV_BLDG,LU) %>% summarise(MedianLIVINGAREA = median(LIVING_AREA))
    
    family <- c(input$residential)
    
    property_data_clean_1<-property_data_clean_1 %>% 
      mutate(LU_new = case_when(
        LU == "R1"  ~ "1-family",
        LU == "R2"  ~ "2-family",
        LU == "R3" ~ "3-family",
        LU == "A" ~ "7 or more family"
      )
      )
    
    colpal <- c("chocolate1","blue","gold3","darkolivegreen1")
    
    plot_ly(data = property_data_clean_1, x = ~AV_BLDG, y = ~MedianLIVINGAREA,type ='scatter',mode = 'markers',color=property_data_clean_1$LU_new,colors = colpal,symbol = I(1),
            alpha = input$blur)%>%
      layout(yaxis = list(title = 'Living Area',showgrid =FALSE ),
             xaxis = list(title = 'Total Assessed Building Value',type = "log",tickmode ="Linear",dtick="L<f>",showgrid =FALSE ),showlegend = TRUE) #tickmode and dtick to remove axis ticks which were showing 2 and 5 in between major ticks  
  })

  #Number of Floors 
  output$plot2 <- renderPlotly({
    property_data_clean = property_data_filter()
    #property_data_clean <- subset(property_data_clean, YR_BUILT > input$year[1] & YR_BUILT < input$year[2])
    property_data_clean_1 <- property_data_clean %>% group_by(NUM_FLOORS) %>% summarise(MedianAV_BLDG = median(AV_BLDG))
    
    
    
    colfunc <- colorRampPalette(c("lightblue", "blue"))
    pal <- colfunc(max(property_data_clean_1$MedianAV_BLDG))[property_data_clean_1$MedianAV_BLDG]
    
    #property_data_clean_1 <- arrange(property_data_clean_1, MedianAV_BLDG, R_INT_CND) 
    
    plot_ly(property_data_clean_1, x =~reorder(NUM_FLOORS,NUM_FLOORS) ,y =~MedianAV_BLDG , type = 'bar',  
            name = 'NumFloors',marker = list(color =  pal) )%>%
      layout(yaxis = list(title = 'Number of Floors',showgrid =FALSE),
             xaxis = list(title = 'Median Total Assessed Building Value',showgrid =FALSE ), barmode = 'group')
    
  })  
  

    #Number of Bedrooms in leaflet
    data(zipcode)
    MA <- subset(zipcode,state == "MA")
    colnames(MA)[[1]]="ZIPCODE"
    output$plot4 <- renderLeaflet({
      property_data_clean = property_data_filter()
      #property_data_clean <- subset(property_data_clean, YR_BUILT > input$year[1] & YR_BUILT < input$year[2])
      
      #convert zipcode of property data to character type
      property_data_clean$ZIPCODE <- as.character(property_data_clean$ZIPCODE)
      #add leading zeros before zipcode
      property_data_clean$ZIPCODE <- paste0("0", property_data_clean$ZIPCODE)
      property_data_clean_1 <- left_join(property_data_clean,MA,by="ZIPCODE")
      
      
      #property_data_clean_1 <- arrange(property_data_clean_1, MedianAV_BLDG, R_INT_CND) 
      
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = property_data_clean_1$R_BDRMS)
      
      leaflet(property_data_clean_1) %>% 
        addTiles('http://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png', 
                 attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
        setView(-71.057083, 42.361145, zoom = 12) %>% 
        addCircleMarkers(~longitude, ~latitude,
                         radius = sqrt(sqrt(property_data_clean_1$AV_BLDG))/3,
                         color = ~pal(R_BDRMS),
                         popup = ~htmlEscape(city),
                         label = paste(property_data_clean_1$city,
                                       "Building Value:", property_data_clean_1$AV_BLDG),
                         stroke = FALSE, fillOpacity = 0.5) %>% 
        addLegend("bottomright", pal = pal, values = ~R_BDRMS,
                  title = "Number of bedrooms",
                  opacity = 1)
  }) 
    
output$plot5<- renderPlotly({
  property_data_clean = property_data_filter()
  property_data_clean_1 <- property_data_clean %>% group_by(R_HEAT_TYP) %>% summarise(MedianAV_BLDG = median(AV_BLDG))
  property_data_clean_1<-property_data_clean_1 %>% 
    mutate(HeatType = case_when(
      R_HEAT_TYP == "E"  ~ "Electric",
      R_HEAT_TYP == "F"  ~ "Forced Air",
      R_HEAT_TYP == "N" ~ "None",
      R_HEAT_TYP == "O" ~ "Other",
      R_HEAT_TYP == "P" ~ "Heat Pump",
      R_HEAT_TYP == "S" ~ "Space Heater",
      R_HEAT_TYP == "W" ~ "Hot Water"
    )
    )
  
  plot_ly(property_data_clean_1, x = ~HeatType, y = ~MedianAV_BLDG, type = 'bar', 
          marker = list(color = c('rgba(204,204,204,1)','rgba(240,52,52,1)', 'rgba(204,204,204,1)',
                                  'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(240,52,52,1)')),
          name = 'HeatingType')%>%
    layout(yaxis = list(title = 'Median Total Assessed Building Value',showgrid =FALSE),
           xaxis = list(title = 'Heating Type'), barmode = 'group')
  
})    

output$plot6<- renderPlotly({
property_data_clean = property_data_filter()
property_data_clean <- subset(property_data_clean, YR_BUILT > input$year[1] & YR_BUILT < input$year[2])
property_data_clean$YR_BUILT <- as.factor(property_data_clean$YR_BUILT)

property_data_clean<-property_data_clean %>% 
  mutate(HeatType = case_when(
    R_HEAT_TYP == "E"  ~ "Electric",
    R_HEAT_TYP == "F"  ~ "Forced Air",
    R_HEAT_TYP == "N" ~ "None",
    R_HEAT_TYP == "O" ~ "Other",
    R_HEAT_TYP == "P" ~ "Heat Pump",
    R_HEAT_TYP == "S" ~ "Space Heater",
    R_HEAT_TYP == "W" ~ "Hot Water"
  )
  )

plot_ly(property_data_clean, x = ~HeatType, y = ~YR_BUILT,  type="scatter", mode = "markers" ,symbol =I(1),
        marker=list(size=20 , opacity=0.5), color = list(c('rgba(204,204,204,1)','rgba(255,0,0,1)',
                                                            'rgba(204,204,204,1)',
                                                            'rgba(255,0,0,1)', 'rgba(204,204,204,1)',
                                                            'rgba(204,204,204,1)', 'rgba(240,52,52,1)')) )%>% 
layout(yaxis = list(title = 'Year Built'),
       xaxis = list(title = 'Heating Type',showgrid =FALSE), barmode = 'group')
})

}

# Run the app ----
shinyApp(ui, server)