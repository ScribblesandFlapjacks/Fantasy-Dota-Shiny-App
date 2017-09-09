bonusInfoUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(column(3, selectInput(ns("bonusType1"), label = "Bonus 1", choices = c(""))),
           column(6, sliderInput(ns("bonusPercent1"), label = "", min = 0, max = 0.25, value = 0, step = 0.05))),
    fluidRow(column(3, selectInput(ns("bonusType2"), label = "Bonus 2", choices = c(""))),
           column(6, sliderInput(ns("bonusPercent2"), label = "", min = 0, max = 0.25, value = 0, step = 0.05))),
    fluidRow(column(3, selectInput(ns("bonusType3"), label = "Bonus 3", choices = c(""))),
           column(6, sliderInput(ns("bonusPercent3"), label = "", min = 0, max = 0.25, value = 0, step = 0.05))),
    fluidRow(column(3, selectInput(ns("bonusType4"), label = "Bonus 4", choices = c(""))),
           column(6, sliderInput(ns("bonusPercent4"), label = "", min = 0, max = 0.25, value = 0, step = 0.05))),
    fluidRow(column(3, selectInput(ns("bonusType5"), label = "Bonus 5", choices = c(""))),
           column(6, sliderInput(ns("bonusPercent5"), label = "", min = 0, max = 0.25, value = 0, step = 0.05)))
  )
}

bonusInfo <- function(input, output, session){
  bonusTypes <- c("Kills", "Deaths", "CS", "GPM", "Towers", "Rosh Kills",
                  "Teamfight", "Wards", "Camps", "Runes", "First Blood", "Stuns")
  
  observe({
    updateSelectInput(session, "bonusType1", choices = bonusTypes, selected = "Kills")
    updateSelectInput(session, "bonusType2", choices = bonusTypes, selected = "Deaths")
    updateSelectInput(session, "bonusType3", choices = bonusTypes, selected = "CS")
    updateSelectInput(session, "bonusType4", choices = bonusTypes, selected = "GPM")
    updateSelectInput(session, "bonusType5", choices = bonusTypes, selected = 'Towers')
  })
  
  bonusVector <- reactive({
    temp <- data.frame(Kills = numeric(0),Deaths = numeric(0), CS = numeric(0), GPM = numeric(0),
                       Towers = numeric(0), RoshKills = numeric(0), Teamfight = numeric(0),
                       Wards = numeric(0), Camps = numeric(0), Runes = numeric(0), FirstBlood = numeric(0), Stuns = numeric(0))
    temp[1,] <- c(rep(1,12))
    colnames(temp) <- bonusTypes
    temp[input$bonusType1] <- input$bonusPercent1 + 1
    if(!(input$bonusType2 == input$bonusType1)){
      temp[input$bonusType2] <- input$bonusPercent2 + 1
    }
    if(!(input$bonusType3 %in% c(input$bonusType1,input$bonusType2))){
      temp[input$bonusType3] <- input$bonusPercent3 + 1
    }
    if(!(input$bonusType4 %in% c(input$bonusType1,input$bonusType2,input$bonusType3))){
      temp[input$bonusType4] <- input$bonusPercent4 + 1
    }
    if(!(input$bonusType5 %in% c(input$bonusType1,input$bonusType2,input$bonusType3,input$bonusType4))){
      temp[input$bonusType5] <- input$bonusPercent5 + 1
    }
    
    temp
  })
  
  return(bonusVector)
}