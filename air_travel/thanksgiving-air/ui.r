library(shinydashboard)

week<-levels(tday.03.16$WEEKDAY)
airlines<-c("All", unique(tday.03.16$Description))

shinyUI(dashboardPage(
  dashboardHeader(title = "Thanksgiving Flights"),
  dashboardSidebar(
    sidebarUserPanel("Dr. David Letzler", image = "https://shiny.nycdatascience.com/images/student/David%20Letzler.jpg"),
    
    sidebarMenu(
      menuItem("Heatmap (Filtered)", tabName= "heat", icon=icon("map-o")),
      menuItem("Airport Map (Total)", tabName="airport", icon=icon("plane")),
      menuItem("Flights (Filtered)", tabName="table", icon=icon("table")),
      selectInput("airline", "Airline", choices=airlines, selected = "All"),
      selectInput("weekday", "Day(s) of the Week", choices=week, selected=week[1]),
      sliderInput("year", "Year", min=2003, max=2016, value=c(2013, 2016), sep="")
      
    )
  ),

    dashboardBody(
    tabItems(
      tabItem(tabName = "heat",fluidRow(box(leafletOutput("heat")))),
      tabItem(tabName = "airport",fluidRow(box(leafletOutput("airport")))),
      tabItem(tabName = "table", fluidRow(box(DT::dataTableOutput("table")))))
      
    ))
)