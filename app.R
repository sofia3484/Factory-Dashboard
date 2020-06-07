
# Factory Database
# Final Exam
# Sofia Nicklaus S.
# Department of Business statistics, Matana University (Tangerang)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Complete <- read.csv("Complete.csv")

library(rworldmap)
library(tibble)
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(rintrojs)
library(ggplot2)

ui <- dashboardPage(skin="red", 
                    
                    
                    dashboardHeader(title = "Sofia Factory",titleWidth = 200,
                                    tags$li(class="dropdown", tags$img(src="Matana.png", height= 45, width= 45)), 
                                    tags$li(introjsUI(),
                                        class = "dropdown",
                                        style = "margin-top: 7px; margin-right: 5px;",
                                        actionButton(icon = icon("question"), "help", "", title = "Start a tour of the dashboard"))),
                    
                    # sidebar
                    dashboardSidebar(
                            sidebarMenu(
                                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                menuItem("Customer Map", tabName = "map", icon = icon("map")),
                                menuItem("App Source Code", icon = icon("code"), href = "https://github.com/sofia3484/Factory-Dashboard"),
                                menuItem("About Me", icon = icon("linkedin"), href = "https://www.linkedin.com/in/sofia-ns/"),
                                menuItem("Video", icon = icon("youtube"), href = "https://youtu.be/77vK63JTyuo")
                            )
                        ),
                        
                        # body
                        dashboardBody( gitlink::ribbon_css("https://github.com/sofia3484/Factory-Dashboard", 
                                                           position = "right", 
                                                           parent_css = list("z-index" = "1040", "pointer-events" = "none"),
                                                           "pointer-events" = "auto"),
                            
                            tabItems(
                                tabItem(tabName = "dashboard",
                                        fluidRow(
                                            
                                                                                # row one
                                        frow1 <- fixedRow(
                                            valueBoxOutput("value1", width = 3),
                                            valueBoxOutput("value2", width = 3),
                                            valueBoxOutput("value3", width = 3)),
                                                                                    
                                        
                                        # row 2
                                        frow2 <- fluidRow( 
                                            box(
                                                title = "Summary"
                                                ,id = "summary"
                                                ,status = "primary"
                                                ,solidHeader = FALSE 
                                                ,collapsible = TRUE 
                                                , height = "500px"
                                                ,verbatimTextOutput("summary")
                                            )
                                            
                                            ,box(
                                                title = "Product Name"
                                                ,id = "box1"
                                                ,status = "primary"
                                                ,solidHeader = FALSE 
                                                ,collapsible = TRUE 
                                                ,plotOutput("bar", height = "500px")),
                                
                                         frow3 <- fluidRow(
                                            box(title = "Package"
                                                ,id = "box2"
                                                ,status = "primary"
                                                ,solidHeader = FALSE 
                                                ,collapsible = TRUE 
                                                ,plotOutput("bar2", height = "500px"))
                                            
                                            ,box(title = "Country"
                                                 ,id = "box3"
                                                 ,status = "primary"
                                                 ,solidHeader = FALSE 
                                                 ,collapsible = TRUE 
                                                 ,plotOutput("bar3", height = "500px")),
                                              
                                                                       )))),
                                    
                                            
                                        # map tab
                                        tabItem(tabName = "map",
                                                frow5 <- fluidRow(
                                                  box(
                                                    title = "Customer  by Location",
                                                    id = "box4",
                                                    status = "primary",
                                                    solidHeader = TRUE,
                                                    width = 12,
                                                    plotOutput("map", height = "750px"))))
                                                  
                                                
                                        #row
                                            )))

                  
                    # D. server function----
                    server <- function(input, output, session) {
                      
                      output$summary <- renderPrint({summary(Complete[,1:4])})
                      
                      output$value1 <- renderValueBox({
                        quantity <- Complete$Quantity
                        avg_quantity <- round(mean(na.rm = TRUE, quantity), 4)
                        valueBox(
                          avg_quantity
                          ,paste('Average Quantity')
                          ,icon = icon("pallet",lib='glyphicon')
                          ,color = "teal")  })
                      
                      output$value2 <- renderValueBox({
                        max_unitp <- Complete$UnitPrice
                        max_unitprice <- round(max(na.rm = TRUE, max_unitp), 4)
                        valueBox(
                          max_unitprice
                          ,paste('Max Unit Price')
                          ,icon = icon("tags",lib='glyphicon')
                          ,color = "fuchsia")  })       
                      
                      output$value3 <- renderValueBox({
                        max_quantity <- Complete$Quantity
                        max_quantities <- round(max(na.rm = TRUE, max_quantity), 4)
                        valueBox(
                          max_quantities
                          ,paste('Max Quantities')
                          ,icon = icon("shopping-cart",lib='glyphicon')
                          ,color = "olive")  })       
                      
                      
                      
                      output$bar <- renderPlot({
                        productname <- Complete %>% 
                          group_by(ProductName) %>% tally
                        
                        productname1 <- productname %>% top_n(10, n)
                        productname2 <- as.data.frame(productname1)
                        
                       print( ggplot(productname2, aes(x= ProductName, y = n))+
                          geom_bar(stat = "identity")+ labs(x="Product Name", y = "Value") +
                          coord_flip()+theme_bw())})
                      
                        output$bar2 <- renderPlot({
                          package <- Complete %>%
                            group_by(Package) %>% tally
                          package1 <- package %>% top_n(10,n)
                          package2 <- as.data.frame(package1)
                          
                          ggplot(package2, aes(x= Package, y = n))+
                            geom_bar(stat = "identity")+ labs(x="Package", y = "Value") +
                            coord_flip()+theme_bw()})
                          
                        output$bar3 <- renderPlot({
                          mycountry <- Complete %>%
                            group_by(Country) %>% tally
                          mycountry1 <- mycountry %>% top_n(10,n)
                          mycountry2 <- as.data.frame(mycountry1)
                          
                          ggplot(mycountry2, aes(x= Country, y = n))+
                            geom_bar(stat = "identity")+ labs(x="Country", y = "Value") +
                            coord_flip()+theme_bw()})
                        
                      
                        steps <- reactive(
                            data.frame(
                                element=c(".sidebar-menu", ".main-header", ".sidebar-toggle", ".active", "#help"),
                                intro=c(
                                    "This is a sidebar.",
                                    "This is a header.",
                                    "This is a button that allows to close and open the sidebar.",
                                    "This is the active element of the sidebar.",
                                    "The help button that give you information about this dashboard."),
                                position=c("right", "bottom", "bottom", "right", "top")))
                            
                        observeEvent(input$help,
                                     introjs(session,
                                             options = list(steps=steps(),
                                                            "nextLabel"="Next",
                                                            "prevLabel"="Previous",
                                                            "skipLabel"="Skip"),
                                             events = list("oncomplete"=I('alert("Done")'))))
                        
                        
                        
                        output$map <- renderPlot({
                          
                          countries <- as.data.frame(table(Complete$Country))
                          colnames(countries) <- c("country", "value")
                          
                          matched <- joinCountryData2Map(countries, joinCode = "NAME", nameJoinColumn = "country")
                          mapCountryData(matched, nameColumnToPlot = "value", mapTitle = "Map Complete Factory Sample Data", 
                                         catMethod = "pretty", colourPalette = c("lightblue", "darkblue"))})
                        
                    }

shinyApp(ui, server)
