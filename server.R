library(shiny)
library(doBy)
library(shinyjs)
library(httr)
library(jsonlite)
library(plyr)
library(anytime)




shinyServer(function(input, output, session) {
  currentTournament <- 5401
  dataPull <- GET('https://api.opendota.com/api/explorer?sql=SELECT%0Aleagueid%2C%0Amatch_id%2C%0Astart_time%2C%0Aaccount_id%2C%0Anotable_players.name%2C%0Ateam_name%2C%0Afantasy_role%2C%0Alocalized_name%2C%0A0.3%20*%20kills%20%2B%20(3%20-%200.3%20*%20deaths)%20%2B%200.003%20*%20(last_hits%20%2B%20denies)%20%2B%200.002%20*%20gold_per_min%20%2B%20towers_killed%20%2B%20roshans_killed%20%2B%203%20*%20teamfight_participation%20%2B%200.5%20*%20observers_placed%20%2B%200.5%20*%20camps_stacked%20%2B%200.25%20*%20rune_pickups%20%2B%204%20*%20firstblood_claimed%20%2B%200.05%20*%20stuns%20as%20fantasy_points%2C%0Akills%2C%0Adeaths%2C%0A(last_hits%20%2B%20denies)%20as%20CS%2C%0Agold_per_min%2C%0Atowers_killed%2C%0Aroshans_killed%2C%0Ateamfight_participation%2C%0Aobservers_placed%2C%0Acamps_stacked%2C%0Arune_pickups%2C%0Afirstblood_claimed%2C%0Astuns%0Afrom%20matches%0Ajoin%20player_matches%20using(match_id)%0Ajoin%20leagues%20using(leagueid)%0Ajoin%20notable_players%20using(account_id)%0Ajoin%20heroes%20on%20player_matches.hero_id%3Dheroes.id%0Awhere%20leagueid%20%3D%205401%20AND%20(team_id%20IN%20(2163%2C1883502%2C39%2C3331948%2C15%2C1375614%2C2108395%2C1333179%2C1838315%2C2586976%2C5%2C46%2C350190%2C2512249%2C2640025%2C2581813%2C1846548%2C2672298))')

  
  data.raw <- rawToChar(dataPull$content)
  data.list <- fromJSON(data.raw)
  data.complete <- data.list$rows
  data.complete <- subset(data.complete,match_id >= 3351961375)
  data.complete <- data.complete[!is.na(data.complete$fantasy_points),]
  data.complete[data.complete$fantasy_role==1,]$fantasy_role <- "Core"
  data.complete[data.complete$fantasy_role==2,]$fantasy_role <- "Support"
  data.complete[data.complete$fantasy_role==3,]$fantasy_role <- "Offlane"
  data.complete[9:21]<-round(data.complete[,-c(1:8)],2)
  data.complete[3] <- lapply(data.complete[3],anydate)

  
  properColNames <- c("Player","Team","Role","Matches","Fantasy Points", "Kills", "Deaths", "CS", "GPM", "Towers", "Rosh",
                      "Teamfight", "Wards", "Camps", "Runes", "First B", "Stuns")
  
  pointValues <- c(0.3,1,0.003,0.002,1,1,3,.5,.5,.25,4,.05)
  
  bonuses <- c("Kills", "Deaths", "CS", "GPM", "Towers", "Rosh",
               "Teamfight", "Wards", "Camps", "Runes", "First B", "Stuns")

  data.players <- data.complete[c(5,6,7)]
  data.players.unique <- unique(data.players)
  data.players.unique <- data.players.unique[order(data.players.unique$name),]

  data.matchCount <- rle(sort(data.players$name))
  data.agg <- summaryBy(.~name, data=data.complete[-c(1,2,3,4,6,7,8)], FUN=mean)
  data.agg <- cbind(data.players.unique,data.matchCount$lengths,data.agg[2:14])
  
  data.matchCount.heroes <- rle(sort(data.complete[-c(1:7)]$localized_name))
  data.agg.heroes <- summaryBy(.~localized_name, data=data.complete[-c(1:7)], FUN=mean)
  data.agg.heroes <- cbind(data.agg.heroes[1],data.matchCount.heroes$lengths,data.agg.heroes[-c(1)])
  colnames(data.agg.heroes) <- c("Hero",properColNames[-c(1:3)])
  data.agg.heroes[bonuses] <- t(t(data.agg.heroes[bonuses])*pointValues)
  data.agg.heroes <- transform(data.agg.heroes, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
  data.agg.heroes[2:15] <- round(data.agg.heroes[2:15],2)
  
  data.sortedFantasy <- data.complete[with(data.complete,order(-fantasy_points)),]
  dates <- unique(data.sortedFantasy[3])
  dates <- dates[with(dates,order(start_time)),]
  dateTemplate <- setNames(rep(1,length(dates)), dates)
  dateFrame <- data.frame(as.list(dateTemplate),check.names = FALSE)
  
  countedGames <- function(df){
    switch(nrow(df),
           df$fantasy_points,
           sum(df$fantasy_points),
           sum(head(df,2)$fantasy_points),
           sum(df$fantasy_points),
           sum(head(df,4)$fantasy_points),
           sum(head(df,4)$fantasy_points))
  }
  
  datesAsColumns <- function(colname,df){
    if(length(df[df$start_time==colname,]$V1) > 0){
      df[df$start_time==colname,]$V1
    } else {
      0
    }
  }
  
  adjustColumns <- function(df){
    data.frame(as.list(sapply(colnames(dateFrame), datesAsColumns,df)),check.names = FALSE)
  }
  
  scoredMatches <- ddply(data.complete, .(start_time,name), countedGames)
  
  data.byDate <- ddply(scoredMatches,  .(name) ,adjustColumns)

  rownames(data.agg) <- data.agg[,1]
  colnames(data.agg) <- properColNames

  
  getAggData <- reactive({
    data.agg
  })
  
  getHeroAggData <- reactive({
    data.agg.heroes
  })
  
  getByDateData <- reactive ({
    data.byDate
  })
  
  getCompleteData <- reactive({
    data.complete
  })
  

  teams <- reactive({
    c("All", unique(getAggData()[order(getAggData()$Team),]$Team))
  })
  
  output$teams <- renderUI({
    selectInput("teamPanel1", h3("Team"), teams())
  })
  
  output$teams2 <- renderUI({
    selectInput("teamPanel2", h4("Team One"), teams())
  })
  
  output$teams3 <- renderUI({
    selectInput("teamPanel3", h4("Team Two"), teams())
  })
  
  output$teams4 <- renderUI({
    selectInput("teamPanel4", h4("Team Three"), teams())
  })
  
  output$playersPanel1 <- renderUI({
    if(input$teamPanel1 == "All"){
      temp <- getAggData()
    } else {temp <- getAggData()[getAggData()$Team == input$teamPanel1,]}
    if(input$positionPanel1!="All"){
      temp <- temp[temp$Role == input$positionPanel1,]
    }
    selectInput("playersPanel1",h3("Player"),temp$Player)
  })
  
  output$playersPanel2 <- renderUI({
    if(input$teamPanel2 == "All"){
      temp <- getAggData()
    } else {temp <- getAggData()[getAggData()$Team == input$teamPanel2,]}
    if(input$positionPanel2!="All"){
      temp <- temp[temp$Role == input$positionPanel2,]
    }
    selectInput("playersPanel2",h4("Player One"),temp$Player)
  })
  
  output$playersPanel3 <- renderUI({
    if(input$teamPanel3 == "All"){
      temp <- getAggData()
    } else {temp <- getAggData()[getAggData()$Team == input$teamPanel3,]}
    if(input$positionPanel3!="All"){
      temp <- temp[temp$Role == input$positionPanel3,]
    }
    selectInput("playersPanel3",h4("Player Two"),temp$Player)
  })
  
  output$playersPanel4 <- renderUI({
    if(input$teamPanel4 == "All"){
      temp <- getAggData()
    } else {temp <- getAggData()[getAggData()$Team == input$teamPanel4,]}
    if(input$positionPanel4!="All"){
      temp <- temp[temp$Role == input$positionPanel4,]
    }
    selectInput("playersPanel4",h4("Player Three"),c("None",temp$Player))
  })
  
  
  compPlayer1 <- callModule(bonusInfo, "compPlayer1")
  compPlayer2 <- callModule(bonusInfo, "compPlayer2")
  compPlayer3 <- callModule(bonusInfo, "compPlayer3")
  
  lineupPlayer1 <- callModule(bonusInfo, "lineupPlayer1")
  lineupPlayer2 <- callModule(bonusInfo, "lineupPlayer2")
  lineupPlayer3 <- callModule(bonusInfo, "lineupPlayer3")
  lineupPlayer4 <- callModule(bonusInfo, "lineupPlayer4")
  lineupPlayer5 <- callModule(bonusInfo, "lineupPlayer5")
  
  observe({
    updateSelectInput(session, "card1", choices = rownames(getAggData()[getAggData()$Role == "Core",]))
    updateSelectInput(session, "card2", choices = rownames(getAggData()[getAggData()$Role == "Core",]))
    updateSelectInput(session, "card3", choices = rownames(getAggData()[getAggData()$Role == "Offlane",]))
    updateSelectInput(session, "card4", choices = rownames(getAggData()[getAggData()$Role == "Support",]))
    updateSelectInput(session, "card5", choices = rownames(getAggData()[getAggData()$Role == "Support",]))
  })
  
  #####Player Tab
  ##Boxplot
  singlePlayerData <- reactive({
    temp <- getCompleteData()
    temp
  })
  
  output$box <- renderPlot(
    boxplot(singlePlayerData()[singlePlayerData()$name == input$playersPanel1,]$fantasy_points)
  )
  
  ##Single Player Data
  statDat <- reactive({
    temp <- getAggData()[input$playersPanel1,]
    #temp[bonuses] <- t(t(temp[bonuses])*pointValues)
    #temp <- transform(temp, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
    #c(Matches=temp$Matches,round(temp[,-c(1:4)],2))
  })
  
  ##Single Player Table Output
  observe({
    output$statTab <- renderTable(statDat())
  })
  #####
  
  
  #####Compare Tab
  ##Box Data 1
  compareBox <- reactive({
    temp <- data.frame(input$playersPanel2,stringsAsFactors=F)
    temp <- temp[0,]
    tempData <- getCompleteData()
    temp[1] <- tempData[tempData$name == input$playersPanel2,]$fantasy_points
  })
  
  ##Box Data 2
  compareBox2 <- reactive({
    temp <- data.frame(input$playersPanel3,stringsAsFactors=F)
    temp <- temp[0,]
    tempData <- getCompleteData()
    temp[1] <- tempData[tempData$name == input$playersPanel3,]$fantasy_points
  })
  
  compareBox3 <- reactive({
    temp <- data.frame(input$playersPanel4,stringsAsFactors=F)
    temp <- temp[0,]
    tempData <- getCompleteData()
    temp[1] <- tempData[tempData$name == input$playersPanel4,]$fantasy_points
  })
  
  boxesToPlot <- reactive({
    boxPlots <- list(compareBox(),compareBox2())
    if(input$playersPanel4 != "None"){
      boxPlots <- c(boxPlots, list(compareBox3()))
    }
    boxPlots
  })
  
  namesToPlot <- reactive({
    names <- c(input$playersPanel2,input$playersPanel3)
    if(input$playersPanel4 != "None"){
      names <- c(names, input$playersPanel4)
    }
    names
  })
  
  ##Boxplot Output
  output$compareBox <- renderPlot(
    boxplot(boxesToPlot(), names = namesToPlot())
  )
  
  getCompStats <- reactive({
    compStats <- rbind(compPlayer1(),compPlayer2())
    if(input$playersPanel4 != "None"){
      compStats <- rbind(compStats, compPlayer3())
    }
    compStats
  })
  
  ##Table Data
  compareStat <- reactive({
    temp <- getAggData()[namesToPlot(),]
    temp[bonuses] <- t(t(temp[bonuses])*pointValues)
    temp <- transform(temp, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
    temp
  })
  
  ##Bonus Adjusted Table Output
  output$adjustedCompTab <- renderTable(adjustedScores(getCompStats(), compareStat()))
  #####
  
  #####Top Five Tab
  ##Top Five Of A Role
  topFive <- reactive({
    temp <- getAggData()
    temp <- temp[order(-temp$`Fantasy Points`),]
    if(input$positionPanel4=="All"){
      head(temp,5)
    } else {
      head(temp[temp$Role==input$positionPanel4,],5)
    }
  })
  
  ##Table Data
  topFiveData <- reactive({
    temp <- topFive()
    temp[bonuses] <- t(t(temp[bonuses])*pointValues)
    temp <- transform(temp, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
    temp
  })
  
  ##Table Output
  observe({
    output$topFiveTab <- renderTable(topFiveData())
  })
  
  ##Boxplot Data 1
  topFiveBox1 <- reactive({
    player <- topFiveData()[1,]$Player
    temp <- data.frame(player,stringsAsFactors=F)
    temp <- temp[0,]
    tempData <- getCompleteData()
    temp[1] <- tempData[tempData$name == player,]$fantasy_points
  })
  
  ##Boxplot Data 2
  topFiveBox2 <- reactive({
    player <- topFiveData()[2,]$Player
    temp <- data.frame(player,stringsAsFactors=F)
    temp <- temp[0,]
    tempData <- getCompleteData()
    temp[1] <- tempData[tempData$name == player,]$fantasy_points
  })
  
  ##Boxplot Data 3
  topFiveBox3 <- reactive({
    player <- topFiveData()[3,]$Player
    temp <- data.frame(player,stringsAsFactors=F)
    temp <- temp[0,]
    tempData <- getCompleteData()
    temp[1] <- tempData[tempData$name == player,]$fantasy_points
  })
  
  ##Boxplot Data 4
  topFiveBox4 <- reactive({
    player <- topFiveData()[4,]$Player
    temp <- data.frame(player,stringsAsFactors=F)
    temp <- temp[0,]
    tempData <- getCompleteData()
    temp[1] <- tempData[tempData$name == player,]$fantasy_points
  })
  
  ##Boxplot Data 5
  topFiveBox5 <- reactive({
    player <- topFiveData()[5,]$Player
    temp <- data.frame(player,stringsAsFactors=F)
    temp <- temp[0,]
    tempData <- getCompleteData()
    temp[1] <- tempData[tempData$name == player,]$fantasy_points
  })
  
  ##Boplot Output
  output$topFiveBox <- renderPlot(
    boxplot(list(topFiveBox1(),topFiveBox2(),topFiveBox3(),topFiveBox4(),topFiveBox5()),
            names = topFiveData()$Player)
  )
  #####
  
  #####Match Stats Tab
  ##Match Stats Table
  observe({
    temp <- getCompleteData()[-c(1,3,4,8)]
    colnames(temp) <- c("Match Id", properColNames[-4])
    temp[bonuses] <- t(t(temp[bonuses])*pointValues)
    temp <- transform(temp, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
    output$matchStats <- renderDataTable(temp)
  })
  #####
  
  #####Averages Tab
  ##Average Stats Table
  observe({
    temp <- getAggData()
    temp[bonuses] <- t(t(temp[bonuses])*pointValues)
    temp <- transform(temp, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
    temp[5:17]<-round(temp[,-c(1:4)],2)
    output$avgStats <- renderDataTable(temp)
  })
  #####
  
  #####Lineup Tab
  ##Bonus Adjusted Data
  lineupStat <- reactive({
    temp <- getAggData()[c(input$card1,input$card2,input$card3,input$card4,input$card5),]
    temp[bonuses] <- t(t(temp[bonuses])*pointValues)
    temp <- transform(temp, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
    temp
  })
  
  ##Table Output
  observe({
    output$adjustedLineupTab <- renderTable(adjustedScores(
      rbind(lineupPlayer1(),lineupPlayer2(),lineupPlayer3(),lineupPlayer4(),lineupPlayer5()),lineupStat()))
  })
  
  ##Fantasy Point Lineup Sum
  sumLineup <- reactive({
    temp <- adjustedScores(rbind(lineupPlayer1(),lineupPlayer2(),lineupPlayer3(),lineupPlayer4(),lineupPlayer5()),lineupStat())
    round(sum(temp[5]),2)
  })
  
  ##Total Score Text Output
  output$totalPoints <- renderText({
    paste("Expected points per a game played", sumLineup())
  })
  #####
  
  #####Hero Tab
  observe({
    output$heroTable <- renderDataTable(getHeroAggData())
  })
  #####
  
  #####By Date Tab
  observe({
    output$byDateTable <- renderDataTable(getByDateData())
  })
  #####

  #####
  ##Adjusts Data With Bonuses
  adjustedScores <- function(bonusSet,data){
    temp <- data
    temp[,-c(1:5)] <- temp[,-c(1:5)] * bonusSet
    temp[5] <- rowSums(temp[,-c(1:5)])
    return(temp)
  }
  #####
  
  #####UI Logic
  ##Sidebar Show/Hide
  observe({
    if(input$Panels > 4){
      addClass(selector = "body", class = "sidebar-collapse")
    } else {
      removeClass(selector = "body", class = "sidebar-collapse")
    }
  })
  #####

  
  
})
