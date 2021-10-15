###Author: Youpei Xu
###Date:1/6/2021
###Student ID:30704855

##Required library
library(ggplot2)
library(shiny)
library(reshape2)
library(ggpubr)
library(leaflet)
library(shinydashboard)
library(dplyr) 
library(geosphere)
library(maps)
library(stringr)

#####Prepafre for stage 1
flight<-read.csv("Flight.csv")
departFre<-read.csv("DepartFre.csv")
arriveFre<-read.csv("ArriveFre.csv")

for(i in 1:nrow(departFre))
  departFre[i,2]<-str_to_title(departFre$word[i])
for(i in 1:nrow(arriveFre))
  arriveFre[i,2]<-str_to_title(arriveFre$word[i])

departFre[,1]<-"Departure"
arriveFre[,1]<-"Destination"
deparrFre<- data.frame(rbind(departFre,arriveFre))
names(deparrFre)[1]<-"Type"

fli<- flight %>% select(-pair)
depart<- fli[1:21,]
arrive<-fli[22:42,]
allAirports<-cbind(depart,arrive[,2:4])
names(allAirports)[2]<-"Depart"
names(allAirports)[5]<-"Arrive"
names(allAirports)[3]<-"Dep_Lat"
names(allAirports)[4]<-"Dep_Lon"
names(allAirports)[6]<-"Arr_Lat"
names(allAirports)[7]<-"Arr_Lon"
for (i in 1:nrow(allAirports))
  for (j in 1:nrow(departFre))
    ifelse(allAirports$Depart[i] == departFre$word[j],allAirports$Departure[i]<-departFre$freq[j],0)

for (i in 1:nrow(allAirports))
  for (k in 1:nrow(arriveFre))
    ifelse(allAirports$Arrive[i]==arriveFre$word[k],allAirports$Destination[i]<-arriveFre$freq[k],0)

allAirportsM<- melt(allAirports,id.vars = c("X","Depart","Dep_Lat","Dep_Lon","Arrive","Arr_Lat","Arr_Lon"))

######Prepare for stage 2
DelCan<-read.csv("Del_Can8.csv")

Ade<- subset(DelCan,LOCATION %in% "Adelaide")
Bri<- subset(DelCan,LOCATION %in% "Brisbane")
Can<- subset(DelCan,LOCATION %in% "Canberra")
Dar<- subset(DelCan,LOCATION %in% "Darwin")
Hob<- subset(DelCan,LOCATION %in% "Hobart")
Mel<- subset(DelCan,LOCATION %in% "Melbourne")
Per<- subset(DelCan,LOCATION %in% "Perth")
Syd<- subset(DelCan,LOCATION %in% "Sydney")

#Relationship between cancel rate and rainfall(total)
CanA<-ggplot(Ade)+geom_bar(aes(x=variable,y=Cancel_rate),stat="identity",fill="red")+scale_x_discrete(limits=unique(Ade$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
CanB<-ggplot(Bri)+geom_bar(aes(x=variable,y=Cancel_rate),stat="identity",fill="orange")+scale_x_discrete(limits=unique(Bri$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
CanC<-ggplot(Can)+geom_bar(aes(x=variable,y=Cancel_rate),stat="identity",fill="yellow")+scale_x_discrete(limits=unique(Can$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
CanD<-ggplot(Dar)+geom_bar(aes(x=variable,y=Cancel_rate),stat="identity",fill="green")+scale_x_discrete(limits=unique(Dar$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
CanH<-ggplot(Hob)+geom_bar(aes(x=variable,y=Cancel_rate),stat="identity",fill="blue")+scale_x_discrete(limits=unique(Hob$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
CanM<-ggplot(Mel)+geom_bar(aes(x=variable,y=Cancel_rate),stat="identity",fill="purple")+scale_x_discrete(limits=unique(Mel$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
CanP<-ggplot(Per)+geom_bar(aes(x=variable,y=Cancel_rate),stat="identity")+scale_x_discrete(limits=unique(Per$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION),color="Red")+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
CanS<-ggplot(Syd)+geom_bar(aes(x=variable,y=Cancel_rate),stat="identity",fill="#009E73")+scale_x_discrete(limits=unique(Syd$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION),color="Red")+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())

image1<-ggarrange(CanA,CanB,CanC,CanD,CanH,CanM,CanP,CanS,ncol=4,nrow=2,labels=c("Adelaide","Brisbane","Canberra","Darwin","Hobart","Melbourne","Perth","Sydney"))

#Relationship between delay rate and rainfall(total)
DelA<-ggplot(Ade)+geom_bar(aes(x=variable,y=Delay_rate),stat="identity",fill="red")+scale_x_discrete(limits=unique(Ade$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
DelB<-ggplot(Bri)+geom_bar(aes(x=variable,y=Delay_rate),stat="identity",fill="orange")+scale_x_discrete(limits=unique(Bri$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
DelC<-ggplot(Can)+geom_bar(aes(x=variable,y=Delay_rate),stat="identity",fill="yellow")+scale_x_discrete(limits=unique(Can$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
DelD<-ggplot(Dar)+geom_bar(aes(x=variable,y=Delay_rate),stat="identity",fill="green")+scale_x_discrete(limits=unique(Dar$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
DelH<-ggplot(Hob)+geom_bar(aes(x=variable,y=Delay_rate),stat="identity",fill="blue")+scale_x_discrete(limits=unique(Hob$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
DelM<-ggplot(Mel)+geom_bar(aes(x=variable,y=Delay_rate),stat="identity",fill="purple")+scale_x_discrete(limits=unique(Mel$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION))+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
DelP<-ggplot(Per)+geom_bar(aes(x=variable,y=Delay_rate),stat="identity")+scale_x_discrete(limits=unique(Per$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION),color="Red")+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())
DelS<-ggplot(Syd)+geom_bar(aes(x=variable,y=Delay_rate),stat="identity",fill="#009E73")+scale_x_discrete(limits=unique(Syd$variable))+geom_point(aes(x=variable,y=value))+geom_path(aes(x=variable,y=value,group=LOCATION),color="Red")+theme(axis.title.x = element_blank())+theme(axis.title.y = element_blank())

image2<-ggarrange(DelA,DelB,DelC,DelD,DelH,DelM,DelP,DelS,ncol=4,nrow=2,labels=c("Adelaide","Brisbane","Canberra","Darwin","Hobart","Melbourne","Perth","Sydney"))

#melt the table DelCanM
DelCanM <- melt(DelCan,id.vars=c("X","LOCATION","variable","value"))
names(DelCanM)[3]<-"Month"
names(DelCanM)[4]<-"Rainfall"
names(DelCanM)[5]<-"RateType"
names(DelCanM)[6]<-"RateValue"
DelCanM[13:24,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[25:36,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[37:48,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[49:60,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[61:72,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[73:84,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[85:96,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[97:108,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[109:120,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[121:132,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[133:144,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[145:156,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[157:168,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[169:180,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)
DelCanM[181:192,1]<-c(1,2,3,4,5,6,7,8,9,10,11,12)

#####Prepare for stage 3
pax <-read.csv("Pax_total11.csv")
tax <- read.csv("Fare_total11.csv")

paxM<-melt(pax,id.vars = c("X","Lat","Lon"))
names(paxM)[1]<-"Location"
names(paxM)[4]<-"Year"
names(paxM)[5]<-"Visitor_Num"
taxM<-melt(tax,id.vars = c("X","Lat","Lon"))
names(taxM)[1]<-"Location"
names(taxM)[4]<-"Year"
names(taxM)[5]<-"Tax_Price"

pal <- colorNumeric(
  palette = "YlGnBu",
  domain = paxM$Visitor_Num
)

pal1 <- colorNumeric(
  palette = "YlGnBu",
  domain = taxM$Tax_Price
)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Data Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction",icon = icon("home")),
      menuItem("Stage 1", tabName = "Stage1",icon=icon("fly")),
      menuItem("Stage 2", tabName = "Stage2",icon =icon("bar-chart-o")),
      menuItem("Stage 3", tabName = "Stage3",icon = icon("map"))
    )),
  dashboardBody(
    #include 4 pages -> 4 tabItem()
    tabItems(
      #Introduction content
      tabItem(
        tabName = "Introduction",
        h3("This is an introduction of data visualization."),br(),br(),"Stage 1.Flights within 8 airports and its information",br(),"Stage 2.Relationship between cancel/delay rate and number of rainfall",br(),"Stage 3. Relationship between number of visitors and flight tax price"
      ),
      #Stage 1 tab content
      tabItem(tabName = "Stage1",  fluidRow(
        headerPanel("Airports information"),
        column(width=9,"Hover on line and see flight information. Click the plane marker and see airport information",leafletOutput("mapflight")),
        column(width = 3,selectInput(inputId = "deparr_fre",
                                     label = "Airports type",
                                     choices = unique(deparrFre$Type)),br(),
               htmlOutput("top3_info"))
      )),
      #Stage 2 tab content
      tabItem(tabName = "Stage2",
              headerPanel("Relationship betw Delay/Cancel Rate and Rainfall"),
              fluidRow(
                column(width = 9,plotOutput("plot2",hover = hoverOpts("plot_hover"),click = "plot_click")),    
                column(width = 3,
                       selectInput(inputId = "airport_name",
                                   label = "Airports:",
                                   choices = 
                                     c("Select one airports",unique(DelCanM$LOCATION))),
                       selectInput(inputId = "rate_RainRel",
                                   label = "Delay/cancel rate:",
                                   choices = unique(DelCanM$RateType)),br(),"Hover on the chart and see delay/cancel rate. Click the point and see number of rainfall",
                       verbatimTextOutput("hov_info"),
                       verbatimTextOutput("cli_info"))
              )
      ),
      
      #Stage 3 tab content
      tabItem(tabName = "Stage3",
              fluidRow(
                column(width = 2,selectInput(inputId = "year",
                                             label = "Year:",
                                             choices = unique(taxM$Year)),
                       br(),"Click the circle and see data"),
                column(width=5,h5("Tax price:"),leafletOutput("mapTax")),
                column(width=5,h5("Number of Visitors:"),leafletOutput("mapPax"))
              )
      )
    )
  )
)

server<-function(input,output,session){
  ####----------------------------------####
  ## This part is for stage 1 .        ##
  ####----------------------------------####
  #filter the data from destination and departure
  data111 <-reactive({
    req(input$deparr_fre)
    df1 <- deparrFre %>% filter(Type %in% input$deparr_fre)
  })
  
  #rank the top three
  output$top3_info <- renderText({
    paste0("<b><p>Top 3 airports:</p></b>", 
           "<p>",data111()[1,2]," ",data111()[1,3],"</p>",
           "<p>",data111()[2,2]," ",data111()[2,3],"</p>",
           "<p>",data111()[3,2]," ",data111()[3,3],"</p>")
  })
  
  #customize the icon
  planeIcon<- makeIcon(
    iconUrl = "./flight_icon.png",
    iconWidth = 40,iconHeight = 40
  )
  
  #plot the map
  output$mapflight<-renderLeaflet({
    m<-leaflet(data = allAirportsM) %>% addTiles() %>%
      addMarkers(~as.numeric(Dep_Lon),
                 ~as.numeric(Dep_Lat),
                 icon = planeIcon,
                 popup = paste0("<b>",allAirportsM$Depart," airport","</b>","<br/>","Number of flights depart/arrive: ",allAirportsM$value ))
    for(i in 1:nrow(allAirportsM))
    {
      ##Retriving from https://stackoverflow.com/questions/32275213/how-do-i-connect-two-coordinates-with-a-line-using-leaflet-in-r
      ##Author: Lili Duan
      m<- m %>% addPolylines(
        lat = c(allAirportsM[i,]$Dep_Lat,allAirportsM[i,]$Arr_Lat),
        lng = c(allAirportsM[i,]$Dep_Lon,allAirportsM[i,]$Arr_Lon),
        color="Black",weight = 2,
        label = paste0(allAirportsM[i,]$Depart," - ",allAirportsM[i,]$Arrive),
        highlightOptions = highlightOptions(bringToFront = TRUE,weight = 8, sendToBack = FALSE, color = "orange"))
    }
    m
  })
  
  ####----------------------------------####
  ## This part is for stage 2.        ##
  ####----------------------------------####
  #filter the data by rate type
  data <-reactive({
    req(input$rate_RainRel)
    req(input$airport_name)
    df <- DelCanM %>% filter(RateType %in% input$rate_RainRel & LOCATION %in% input$airport_name)
  })
  
  #plot the diagram
  output$plot2<-renderPlot({
    if(input$airport_name =="Select one airports" & input$rate_RainRel =="Cancel_rate")
      image1
    else if(input$airport_name =="Select one airports" & input$rate_RainRel =="Delay_rate")
      image2
    else
      ggplot(data())+geom_bar(aes(x=Month,y=RateValue),stat="identity",fill="#009E73")+scale_x_discrete(limits=unique(DelCanM$Month))+geom_point(aes(x=Month,y=Rainfall))+geom_path(aes(x=Month,y=Rainfall,group=LOCATION))
  })
  
  #show hover information about rate number
  ##Retriving from https://stackoverflow.com/questions/27965931/tooltip-when-you-mouseover-a-ggplot-on-shiny
  ##Author: Solvi
  output$hov_info<-renderPrint({
    req(input$plot_hover)
    hover = input$plot_hover
    dist = sqrt((hover$x-data()$X)^2+(hover$y-data()$RateValue)^2)
    if(min(dist)<1){
      cat("Number of rate:\n",data()$RateValue[which.min(dist)])
    }
  })
  
  #show click information about rainfall
  ##Retriving from https://stackoverflow.com/questions/27965931/tooltip-when-you-mouseover-a-ggplot-on-shiny
  ##Author: Solvi
  output$cli_info<-renderPrint({
    req(input$plot_click)
    click = input$plot_click
    dist1 = sqrt((click$x-data()$X)^2+(click$y-data()$Rainfall)^2)
    if(min(dist1)<1){
      cat("Number of rainfall:\n",data()$Rainfall[which.min(dist1)])
    }
  })
  
  ####----------------------------------####
  ## This part is for stage 3.        ##
  ####----------------------------------####
  #filter the data by year for tax & visitors
  dataT<- reactive({
    req(input$year)
    df <- taxM %>% filter(Year %in% input$year)
  })
  dataP<-reactive({
    req(input$year)
    df <- paxM %>% filter(Year %in% input$year)
  })
  
  #plot the graph
  output$mapTax<- renderLeaflet({
    leaflet(dataT()) %>% 
      addTiles() %>%
      addCircles(~as.numeric(Lon),
                 ~as.numeric(Lat),
                 popup = paste0("<b>",dataP()$Location," airport","</b>","<br/>","Tax price: $",dataT()$Tax_Price),
                 color = ~pal1(Tax_Price),
                 radius = ~Tax_Price*100) %>% 
      addLegend('bottomleft',title = "Tax Price",pal = pal1,    
                labFormat = labelFormat(prefix = "$"),,values = ~Tax_Price,opacity = 1)
  })
  output$mapPax <- renderLeaflet({
    leaflet(dataP()) %>% 
      addTiles() %>%
      addCircles(~as.numeric(Lon),~as.numeric(Lat),
                 popup = paste0("<b>",dataP()$Location," airport","</b>","<br/>","Visitor number: ",dataP()$Visitor_Num ),
                 color = ~pal(Visitor_Num),radius = ~Visitor_Num/100)%>% 
      addLegend('bottomleft',title = "Number of Visitors",
                pal = pal,values = ~Visitor_Num,opacity = 1)
  })
}

shinyApp(ui=ui,server=server)
