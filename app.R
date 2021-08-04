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
library(tibble)
library(htmltools)
xRank <- readxl::read_excel("Export_TDL_NED_2021.xlsx", 
                            sheet = "Stand") 
xRank<- add_column(xRank, Logo = xRank$Team, .after = 1)

real_cols <- c("Rank","Logo","Team","P", "W", "D","L","GF","GA","GD","Pts")
expected_cols <- c("xGF", "xGA", "xGD","xPts","xRank")
group_column <- function(class = NULL, ...) {
  colDef(cell = format_pct, maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
   @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100;300&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      h1 {
       font-family: 'Roboto', sans-serif;
        font-weight: 500;
        line-height: 1.1;
        color: #007523;
      }
h2 {
  font-family: 'Yusei Magic', sans-serif;
}
    "))
  ),
  includeCSS("www/dark_mode.css"),
  # tags$head(
  #        tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")
  #   ),
  # Application title
  
  h1("Expected Points Eredivisie"),
  
  # Sidebar with a slider input for number of bins 
  
  
  
  # Show a plot of the generated distribution
  # mainPanel( width=510,
  mainPanel( width=510,
             reactableOutput("dashboard", width = "auto", height = "auto",
                             inline = FALSE),
             br(),
             br(),
             br()
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
                         if(xRank$xRank[index] < xRank$Rank[index]){
                           background <- "#61B861"
                         }else if(xRank$xRank[index] > xRank$Rank[index]){
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
                          background <- "#61B861"
                        }else if(xRank$xRank[index] < xRank$Rank[index]){
                          background <- "#FC785F"
                        }else{
                          
                          background <- "#FDD297"
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
