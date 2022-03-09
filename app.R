
library(shiny)
library(reactable)
library(tibble)
library(htmltools)
library(magrittr)
library(dplyr)
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
knockout_column <- function(maxWidth = 70, class = NULL, ...) {
  colDef(
    cell = format_pct,
    maxWidth = maxWidth,
    class = paste("cell number", class),
    style = function(value) {
      # Lighter color for <1%
      if (value < 0.01) {
        list(color = "#aaa")
      } else {
        list(color = "#111", background = knockout_pct_color(value))
      }
    },
    ...
  )
}
format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 1) "\u2713"  # checkmark for 100%
  else if (value < 0.01) " <1%"
  else if (value > 0.99) ">99%"
  else formatC(paste0(round(value * 100), "%"), width = 4)
}
data <- readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                         sheet = "Wedstrijden") 
xRank <- readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                            sheet = "Stand") 
players <- readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                           sheet = "Player xG") %>%
  select(name,playername,Minutes_played, Goals, NP_Goals, xG, NPxG, xG.90, NPxG.90, NP_Shots) 

players_two <- readxl::read_excel("Export_TDL_NED_2122.xlsx", 
                              sheet = "Player xG") %>%
  select(name,playername,Minutes_played,Assists,xG_assisted,xA.90,Shots_assisted)%>%
  arrange(-Assists)

xRank<- add_column(xRank, Logo = xRank$Team, .after = 1)
xRank<- add_column(xRank, diff = round(xRank$Pts-xRank$xPts,2) , .after = 1)

real_cols <- c("Rank","diff","Logo","Team","P", "W", "D","L","GF","GA","GD","Pts")
expected_cols <- c("xGF", "xGA", "xGD","xPts","xRank")
group_column <- function(class = NULL, ...) {
  colDef(cell = format_pct, maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}
knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
#function for bar in diff
bar_chart_pos_neg <- function(label, value, max_value = 1, height = "16px",
                              pos_fill = "#61B861", neg_fill = "#FC785F") {
  neg_chart <- div(style = list(flex = "1 1 0"))
  pos_chart <- div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value) * 10, "%")
  
  if (value < 0) {
    bar <- div(style = list(marginLeft = "1px", background = neg_fill, width = width, height = height))
    chart <- div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"), label, bar)
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bar <- div(style = list(marginRight = "1px", background = pos_fill, width = width, height = height))
    chart <- div(style = list(display = "flex", alignItems = "center"), bar, label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }
  
  div(style = list(display = "flex"), neg_chart, pos_chart)
}

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("Expected Points Eredivisie"),
                tags$head(
                  tags$style(HTML("
   @import url('https://fonts.googleapis.com/css2?family=Roboto&display=swap');
     
       font-family: 'Roboto', sans-serif;
        font-weight: 500;
     
        color: #007523;
      }
h1 {
  font-family: 'Yusei Magic', sans-serif;
}
    body {
       font-family: 'Roboto', sans-serif;
        font-weight: 500;
       
       
      }
    "))
                ),
                includeCSS("www/dark_mode.css"),
             
                mainPanel( 
                  tabsetPanel(type = "tabs",
                              tabPanel("xRank", 
                                       width=510,
                           reactableOutput("dashboard", width = "auto", height = "auto",
                                           inline = FALSE),
                           
                           br(),
                           br(),
                           br()
                ),
                tabPanel("Goals", 
                         width=510,
                         downloadButton("downloadData", "Download CSV"),
                         reactableOutput("dashboard_players", width = "auto", height = "auto",
                                         inline = FALSE)
),
tabPanel("Assists", 
         width=510,
         reactableOutput("dashboard_players_two", width = "auto", height = "auto",
                         inline = FALSE)
))))



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("players.csv")
    },
    content = function(file) {
      write.csv(players, file, row.names = FALSE)
    }
  )
  
  
  output$dashboard <- renderReactable({
    reactable(
      xRank,
      details = function(index) {
      df <- data %>% 
        dplyr::filter(HomeName == xRank$Team[index]|
                        AwayName == xRank$Team[index]) %>%
        select(Home_xG,HomeName,HomeGoals,AwayGoals,AwayName,Away_xG)
        tbl <- reactable(df,
                         striped = TRUE,
                         width=600,
                         columns = list(
                           HomeName = colDef(name = "Home"),
                           AwayName = colDef(name = "Away"),
                           HomeGoals = colDef(name = "",align = "center"),
                           AwayGoals = colDef(name = "",align = "center"),
                           Home_xG = colDef(name = "xG",align = "center"),
                           Away_xG = colDef(name = "xG",align = "center")))
       htmltools::div(style = list(margin = "12px 45px"), tbl)
      },
      onClick = "expand",
      #rowStyle = list(cursor = "pointer"),
      showSortable = TRUE,
      width = 1080,
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
                     
                     align = "left"
                       
                     
        ),
        xRank = colDef(minWidth = 60,
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
        diff = colDef(
          name = " ",
          defaultSortOrder = "desc",
          cell = function(value) {
            label <- paste0(round(value * 100), "%")
            label <- ""
            bar_chart_pos_neg(label, value)
          },
          align = "center",
          
         # minWidth = 500
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
  
  output$dashboard_players <- renderReactable({
    reactable(players,
              width = 750,
              searchable = TRUE,
              columns = list(
                name = colDef(minWidth = 40,
                              name = "",
                              cell = function(value) {
                                img_src <- knitr::image_uri(sprintf("Clubs/%s.png", value))
                                image <- img(src = img_src, height = "22px", alt = value)
                                tagList(
                                  div(style = list(display = "inline-block", width = "40px"), image)
                                )
                              }),
                playername = colDef(
                  align="left"
                ),
                Minutes_played = colDef(
                 name = "Minutes",
                 maxWidth = 70,
                 format = colFormat(digits = 0),
                 style = function(value) {
                   normalized <- (value - min(players$Minutes_played)) / (max(players$Minutes_played) - min(players$Minutes_played))
                   color <- knockout_pct_color(normalized)
                   
                   # div(style = list(background = color), value)
                   list(background = color)
                 }
                ),
                Goals = colDef(
                  maxWidth = 55,
                  format = colFormat(digits = 0),
                  style = function(value) {
                  normalized <- (value - min(players$Goals)) / (max(players$Goals) - min(players$Goals))
                  color <- knockout_pct_color(normalized)
                  
                 # div(style = list(background = color), value)
                  list(background = color)
                }),
                NP_Goals = colDef(
                  name = "NP Goals",
                  format = colFormat(digits = 0),
                  maxWidth = 55,
                  style = function(value) {
                  normalized <- (value - min(players$Goals)) / (max(players$Goals) - min(players$Goals))
                  color <- knockout_pct_color(normalized)
                  list(background = color)
                }),
                xG = colDef(
                  maxWidth = 55,
                  style = function(value) {
                    normalized <- (value - min(players$xG)) / (max(players$xG) - min(players$xG))
                    color <- knockout_pct_color(normalized)
                    list(background = color)
                  }),
                NPxG = colDef(
                  maxWidth = 55,
                  style = function(value) {
                    normalized <- (value - min(players$NPxG)) / (max(players$NPxG) - min(players$NPxG))
                    color <- knockout_pct_color(normalized)
                    list(background = color)
                  }),
                xG.90 = colDef(
                  maxWidth = 55,
                  style = function(value) {
                    normalized <- (value - min(players$xG.90)) / (max(players$xG.90) - min(players$xG.90))
                    color <- knockout_pct_color(normalized)
                    list(background = color)
                  }),
                NPxG.90 = colDef(
                  maxWidth = 55,
                  style = function(value) {
                    normalized <- (value - min(players$NPxG.90)) / (max(players$NPxG.90) - min(players$NPxG.90))
                    color <- knockout_pct_color(normalized)
                    list(background = color)
                  }),
                NP_Shots = colDef(
                  format = colFormat(digits = 0),
                  name = "NP Shots",
                  maxWidth = 55,
                  style = function(value) {
                    normalized <- (value - min(players$NP_Shots)) / (max(players$NP_Shots) - min(players$NP_Shots))
                    color <- knockout_pct_color(normalized)
                    list(background = color)
                  })),
              defaultColDef = colDef(
                format = colFormat(digits = 2),
                align="center",
                
              )
              )
  })
  
  output$dashboard_players_two <- renderReactable({
    reactable(players_two,
              width = 700,
              searchable = TRUE,
              columns = list(
                name = colDef(minWidth = 40,
                              name = "",
                              cell = function(value) {
                                img_src <- knitr::image_uri(sprintf("Clubs/%s.png", value))
                                image <- img(src = img_src, height = "22px", alt = value)
                                tagList(
                                  div(style = list(display = "inline-block", width = "40px"), image)
                                )
                              }),
                playername = colDef(
                  align="left"
                ),
                Minutes_played = colDef(
                  name = "Minutes",
                  maxWidth = 70,
                  format = colFormat(digits = 0),
                  style = function(value) {
                    normalized <- (value - min(players_two$Minutes_played)) / (max(players_two$Minutes_played) - min(players_two$Minutes_played))
                    color <- knockout_pct_color(normalized)
                    
                    # div(style = list(background = color), value)
                    list(background = color)
                  }
                ),
                Assists = colDef(
                  maxWidth = 60,
                  format = colFormat(digits = 0),
                  style = function(value) {
                    normalized <- (value - min(players_two$Assists)) / (max(players_two$Assists) - min(players_two$Assists))
                    color <- knockout_pct_color(normalized)
                    
                    # div(style = list(background = color), value)
                    list(background = color)
                  }),
                xG_assisted = colDef(
                  name = "xG Assisted",
                  format = colFormat(digits = 2),
                  maxWidth = 55,
                  style = function(value) {
                    normalized <- (value - min(players_two$xG_assisted)) / (max(players_two$xG_assisted) - min(players_two$xG_assisted))
                    color <- knockout_pct_color(normalized)
                    list(background = color)
                  }),
                xA.90 = colDef(
                  name = "xA/90",
                  maxWidth = 55,
                  style = function(value) {
                    normalized <- (value - min(players_two$xA.90)) / (max(players_two$xA.90) - min(players_two$xA.90))
                    color <- knockout_pct_color(normalized)
                    list(background = color)
                  }),
                Shots_assisted = colDef(
                  name = "Shots Assisted",
                  maxWidth = 55,
                  format = colFormat(digits = 0),
                  style = function(value) {
                    normalized <- (value - min(players_two$Shots_assisted)) / (max(players_two$Shots_assisted) - min(players_two$Shots_assisted))
                    color <- knockout_pct_color(normalized)
                    list(background = color)
                  })),
              defaultColDef = colDef(
                format = colFormat(digits = 2),
                align="center",
                
              )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
