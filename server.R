library(shiny)
library(doBy)
library(shinyjs)
library(httr)
library(jsonlite)
library(plyr)
library(anytime)




shinyServer(function(input, output, session) {
  dataPull <- GET('https://api.opendota.com/api/explorer?sql=SELECT%0Aleagueid%2C%0Amatch_id%2C%0Astart_time%2C%0Aaccount_id%2C%0Anotable_players.name%2C%0Ateam_name%2C%0Afantasy_role%2C%0Alocalized_name%2C%0A(0.3*kills%2B(3-0.3*deaths)%2B(0.003*(last_hits%2Bdenies))%2B0.002*gold_per_min%2Btowers_killed%2Broshans_killed%2B3*teamfight_participation%2B0.5*observers_placed%2B0.5*camps_stacked%2B0.25*rune_pickups%2B4*firstblood_claimed%2B0.05*stuns)%20as%20fantasy_points%2C%0Akills%2C%0Adeaths%2C%0A(last_hits%2Bdenies)%20AS%20CS%2C%0Agold_per_min%2C%0Atowers_killed%2C%0Aroshans_killed%2C%0Ateamfight_participation%2C%0Aobservers_placed%2C%0Acamps_stacked%2C%0Arune_pickups%2C%0Afirstblood_claimed%2C%0Astuns%0AFROM%20matches%0AJOIN%20player_matches%20using(match_id)%0AJOIN%20leagues%20using(leagueid)%0AJOIN%20notable_players%20using(account_id)%0AJOIN%20heroes%20on%20player_matches.hero_id%20%3D%20heroes.id%0AWHERE%20matches.leagueid%20IN%20(9870%2C%209943)%0AAND%20(team_id%20IN%20(5%2C%2015%2C%2039%2C%2067%2C%202163%2C%20350190%2C%20543897%2C%20726228%2C%201375614%2C%201838315%2C%201883502%2C%202108395%2C%202586976%2C%205026801%2C%205027210%2C%205066616%2C%205228654%2C%205229127)%0AOR%20account_id%20IN%20(124936122%2C311360822%2C94054712))')
  
  data.raw <- rawToChar(dataPull$content)
  data.list <- fromJSON(data.raw)
  data.complete <- data.list$rows
  #data.complete <- subset(data.complete,match_id >= 3351961375)
  data.complete <- data.complete[!is.na(data.complete$fantasy_points),]
  data.complete[data.complete$fantasy_role==1,]$fantasy_role <- "Core"
  data.complete[data.complete$fantasy_role==2,]$fantasy_role <- "Support"
  data.complete[data.complete$fantasy_role==3,]$fantasy_role <- "Offlane"
  
  #Player Team Fixes
  data.complete <- data.complete[data.complete$name!="1437",]
  data.complete <- data.complete[data.complete$name!="MISERY",]
  data.complete[data.complete$name=="Fly",]$team_name <- "Evil Geniuses"
  data.complete[data.complete$name=="s4",]$team_name <- "Evil Geniuses"
  data.complete[data.complete$name=="Resolut1on",]$team_name <- "VGJ Storm"
  data.complete[data.complete$name=="ana",]$team_name <- "OG"
  data.complete[data.complete$name=="Topson",]$team_name <- "OG"
  data.complete[data.complete$name=="Zyd",]$team_name <- "Team Serenity"
  
  data.complete[9:21]<-round(data.complete[,-c(1:8)],2)
  data.complete[3] <- lapply(data.complete[3],anydate, tz = "America/Los_Angeles")

  
  properColNames <- c("Player","Team","Role","Matches","Fantasy Points", "Kills", "Deaths", "CS", "GPM", "Towers", "Rosh",
                      "Teamfight", "Wards", "Camps", "Runes", "First B", "Stuns")
  
  pointValues <- c(0.3,1,0.003,0.002,1,1,3,.5,.5,.25,4,.05)
  
  bonuses <- c("Kills", "Deaths", "CS", "GPM", "Towers", "Rosh",
               "Teamfight", "Wards", "Camps", "Runes", "First B", "Stuns")
  
  data.sortedFantasy <- data.complete[with(data.complete,order(-fantasy_points)),]
  dates <- unique(data.sortedFantasy[3]) 
  dates <- dates[with(dates,order(start_time)),]
  tiStart <- as.Date("2018-8-15")
  
  createDateFrame <- reactive({
    dates <- unique(data.sortedFantasy[3]) 
    dates <- dates[with(dates,order(start_time)),]
    dates <- dates[dates >= tiStart]
    dateTemplate <- setNames(rep(1,length(dates)), dates)
    dateFrame <- data.frame(as.list(dateTemplate),check.names = FALSE)
  })

  
  countedGames <- function(df){
    sum(df$fantasy_points)
    #switch(nrow(df),
    #       df$fantasy_points,
     #      sum(df$fantasy_points),
      #     sum(head(df,2)$fantasy_points),
       #    sum(df$fantasy_points),
        #   sum(head(df,4)$fantasy_points),
         #  sum(head(df,4)$fantasy_points))
  }
  
  datesAsColumns <- function(colname,df){
    if(length(df[df$start_time==colname,]$V1) > 0){
      df[df$start_time==colname,]$V1
    } else {
      0
    }
  }

  getAggData <- reactive({
    req(input$dates)
    data.players <- data.complete[c(5,6,7)][data.complete$start_time >= input$dates[1] & data.complete$start_time <= input$dates[2],]
    data.players.unique <- unique(data.players)
    data.players.unique <- data.players.unique[order(data.players.unique$name),]
    
    data.matchCount <- rle(sort(data.players$name))
    data.agg <- summaryBy(.~name, data=data.complete[-c(1,2,3,4,6,7,8)][data.complete$start_time >= input$dates[1] & data.complete$start_time <= input$dates[2],], FUN=mean)
    data.agg <- cbind(data.players.unique,data.matchCount$lengths,data.agg[2:14])
    rownames(data.agg) <- data.agg[,1]
    colnames(data.agg) <- properColNames
    data.agg
  })
  
  getHeroAggData <- reactive({
    req(input$dates)
    data.matchCount.heroes <- rle(sort(data.complete[data.complete$start_time >= input$dates[1] & data.complete$start_time <= input$dates[2],][-c(1:7)]$localized_name))
    data.agg.heroes <- summaryBy(.~localized_name, data=data.complete[data.complete$start_time >= input$dates[1] & data.complete$start_time <= input$dates[2],][-c(1:7)], FUN=mean)
    data.agg.heroes <- cbind(data.agg.heroes$localized_name,data.matchCount.heroes$lengths,data.agg.heroes[-c(1)])
    colnames(data.agg.heroes) <- c("Hero",properColNames[-c(1:3)])
    data.agg.heroes[bonuses] <- t(t(data.agg.heroes[bonuses])*pointValues)
    data.agg.heroes <- transform(data.agg.heroes, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
    data.agg.heroes[2:15] <- round(data.agg.heroes[2:15],2)
    data.agg.heroes
  })
  
  getByDateData <- reactive ({
    req(input$dates)
    scoredMatches <- ddply(data.complete[data.complete$start_time >= tiStart,], .(start_time,name), countedGames)
    data.byDate <- ddply(scoredMatches,  .(name) , function(x) data.frame(as.list(sapply(colnames(createDateFrame()), datesAsColumns,x)),check.names = FALSE))
    data.byDate
  })
  
  getCompleteData <- reactive({
    req(input$dates)
    data.complete[data.complete$start_time >= input$dates[1] & data.complete$start_time <= input$dates[2],]
  })
  
  output$dateRange <- renderUI({
    sliderInput("dates", "Choose your dates", min = head(dates,1), max = tail(dates,1),
                value = c(head(dates,1), tail(dates,1)))
  })
  

  teams <- reactive({
    c("All", unique(getAggData()[order(getAggData()$Team),]$Team))
  })
  
  output$teamList <- renderUI({
    checkboxGroupInput("includeTeams", "Teams to include",
                       unique(getAggData()[order(getAggData()$Team),]$Team),
                       selected = unique(getAggData()[order(getAggData()$Team),]$Team))
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
    if(input$positionPanel5!="All"){
      temp <- temp[temp$Role == input$positionPanel5,]
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
    temp[bonuses] <- t(t(temp[bonuses])*pointValues)
    temp <- transform(temp, Deaths = ifelse(Deaths > 10, 0, 3-(0.3*Deaths)))
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
    output$avgStats <- renderDataTable(temp[temp$Team %in% input$includeTeams,])
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
    if(input$Panels > 5){
      addClass(selector = "body", class = "sidebar-collapse")
    } else {
      removeClass(selector = "body", class = "sidebar-collapse")
    }
  })
  #####

  
  
})
