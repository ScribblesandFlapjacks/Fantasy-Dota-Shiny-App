library(shiny)
library(shinyjs)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Fantasy Dota"),
  dashboardSidebar(
           conditionalPanel(condition="input.Panels==0"
                            #radioButtons("Dataset", label = "Include Previous Tournament Data", choices = c("Yes","No"))
                            ),
           conditionalPanel(condition="input.Panels==1",
                            uiOutput("teams"),
                            radioButtons("positionPanel1", label = h3("Position"), choices = c("All","Core","Offlane","Support")),
                            uiOutput("playersPanel1")
           ),
           conditionalPanel(condition = "input.Panels==4",
                            radioButtons("player","Player", c("All","P1","P2","P3"), inline=T),
                            conditionalPanel(condition = "input.player == 'P1' | input.player == 'All'",
                              uiOutput("teams2"),
                              selectInput("positionPanel2", label = h4("Position One"), choices = c("All","Core","Offlane","Support")),
                              uiOutput("playersPanel2")
                            ),
                            conditionalPanel(condition = "input.player == 'P2' | input.player == 'All'",
                              uiOutput("teams3"),
                              selectInput("positionPanel3", label = h4("Position Two"), choices = c("All","Core","Offlane","Support")),
                              uiOutput("playersPanel3")
                            ),
                            conditionalPanel(condition = "input.player == 'P3' | input.player == 'All'",
                              uiOutput("teams4"),
                              selectInput("positionPanel4", label = h4("Position Three"), choices = c("All","Core","Offlane","Support")),
                              uiOutput("playersPanel4")
                            )
           ),
           conditionalPanel(condition = "input.Panels==2",
                            radioButtons("positionPanel4", label = h3("Position"), choices = c("All","Core","Offlane","Support"))
                            )
         ),
  dashboardBody(
    useShinyjs(),
    tabsetPanel(
             tabPanel("About",value=0, includeHTML("About.html")),
             tabPanel("Players",value=1, plotOutput("box"), div(style = 'overflow-x: scroll', tableOutput('statTab'))),
             tabPanel("Matches",value=7, div(style = 'overflow-x: scroll', dataTableOutput('matchStats'))),
             tabPanel("Averages",value=8, div(style = 'overflow-x: scroll', dataTableOutput("avgStats"))),
             tabPanel("By Date", value = 7, div(style = 'overflow-x: scroll', dataTableOutput('byDateTable'))),
             tabPanel("Compare Players", value = 4, fluidRow(box(plotOutput("compareBox")),
                                                    tabBox(tabPanel("Player 1",
                                                                    bonusInfoUI("compPlayer1")),
                                                           tabPanel("Player 2",
                                                                    bonusInfoUI("compPlayer2")),
                                                           tabPanel("Player 3",
                                                                    bonusInfoUI("compPlayer3"))
                                                           )),
                      div(style = 'overflow-x: scroll', tableOutput("adjustedCompTab"))
                      ),
             tabPanel("Top Five", value=2,plotOutput("topFiveBox"), div(style = 'overflow-x: scroll', tableOutput("topFiveTab"))),
             tabPanel("Card Lineup", value=9, fluidRow(
               box(selectInput("card1", label = h4("Core One"), choices = ""),
               selectInput("card2", label = h4("Core Two"), choices = ""),
               selectInput("card3", label = h4("Offlane"), choices = ""),
               selectInput("card4", label = h4("Support One"), choices = ""),
               selectInput("card5", label = h4("Support Two"), choices = "")),
               tabBox(
                 tabPanel("Core 1", bonusInfoUI("lineupPlayer1")),
                 tabPanel("Core 2", bonusInfoUI("lineupPlayer2")),
                 tabPanel("Offlane", bonusInfoUI("lineupPlayer3")),
                 tabPanel("Support 1", bonusInfoUI("lineupPlayer4")),
                 tabPanel("Support 2", bonusInfoUI("lineupPlayer5")))
             ),
             div(style = 'overflow-x: scroll', tableOutput("adjustedLineupTab")),
             textOutput("totalPoints")),
             tabPanel("Heroes", value=6, div(style = 'overflow-x: scroll', dataTableOutput("heroTable"))),
             id="Panels"
           ),
    tags$head(HTML("<script type='text/javascript' src='js/googleAnalytics.js'></script>")))
)
