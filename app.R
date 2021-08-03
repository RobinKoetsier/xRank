#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reactable)
xRank <- readxl::read_excel("Export_TDL_NED_2021.xlsx", 
                  sheet = "Stand") 
xRank<- add_column(xRank, Logo = xRank$Team, .after = 1)
# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
   @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100;300&display=swap');
      
      h1 {
       font-family: 'Roboto', sans-serif;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
      }

    "))
    ),
    includeCSS("www/dark_mode.css"),
   # tags$head(
#        tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")
 #   ),
    # Application title
    h2("Expected Points Eredivisie"),
h1("Expected Points Eredivisie", font = "Helvetica Neue"),
h2("Expected Points Eredivisie", font = "Firo Mono"),
h3("test", font = "Helvetica Neue"),
h3("test",font ="Fira Mono"),
    # Sidebar with a slider input for number of bins 

       

        # Show a plot of the generated distribution
        mainPanel( width=510,
            reactableOutput("dashboard", width = "auto", height = "auto",
                            inline = FALSE)
            )
        )
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet")
  tags$link(href = "https://fonts.googleapis.com/css2?family=Fira+Sans:wght@200&family=Roboto:wght@100&display=swap", rel = "stylesheet")
  
 
     
    output$dashboard <- renderReactable({
        reactable(
            xRank,
            showSortable = TRUE,
            defaultColGroup = colGroup(headerClass = "group-header"),
            columnGroups = list(
                colGroup(name = "League Table", columns = real_cols),
                colGroup(name = "Expected", columns = expected_cols)),
            columns = list(
                Logo = colDef(minWidth = 40,
                              name = "",
                              cell = function(value) {
                                  img_src <- knitr::image_uri(sprintf("Clubs/%s.png", value))
                                  image <- img(src = img_src, height = "22px", alt = value)
                                  tagList(
                                      div(style = list(display = "inline-block", width = "40px"), image)
                                  )
                              }),
                Team= colDef(minWidth = 140,
                             
                             align = "left",
                ),
                xRank = colDef(minWidth = 50,
                               align = "center",
                               cell = function(value,index){
                                   if(xRank$xRank[index] > xRank$Rank[index]){
                                       background <- "#61B861"
                                   }else if(xRank$xRank[index] < xRank$Rank[index]){
                                       background <- "#FC785F"
                                   }else{
                                       
                                       background <- "#FDD297"
                                   }
                                   div(class = "spi-rating", style = list(background = background), value)
                               }),
                P = colDef(
                    style = list(borderLeft = "2px solid rgba(0, 0, 0, 1)")
                ),
                xGF = colDef(
                    style = list(borderLeft = "2px solid rgba(0, 0, 0, 1)")
                ),
                Rank = colDef(minWidth = 60,
                              align = "center",
                              
                              cell = function(value,index){
                                  if(xRank$xRank[index] > xRank$Rank[index]){
                                      background <- "#e32b3b"
                                  }else if(xRank$xRank[index] < xRank$Rank[index]){
                                      background <- "#19cf25"
                                  }else{
                                      
                                      background <- "orange"
                                  }
                                  div(class = "spi-rating", style = list(background = background), value)
                              })
            ),
            
            
            
            defaultColDef = colDef(
                class = "cell", 
                headerClass = "header",
                align = "center",
                minWidth = 50,
                headerStyle = list(background = "#f7f7f8")
            ),
            searchable = FALSE,
            defaultPageSize = 100,
            striped = TRUE,
            highlight = TRUE,
            # bordered = TRUE,
            compact = TRUE,
            theme = reactableTheme(
                borderColor = "#dfe2e5",
                stripedColor = "#f6f8fa",
                highlightColor = "#d7dce0",
                cellPadding = "6px 6px",
                
              
                searchInputStyle = list(width = "100%",height = "100%")
            )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
