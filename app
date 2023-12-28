library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Lahman)
library(shiny)
library(DT)


# This is inspired by Bill James's Similarity Score formula as well as
# "Crafted NBA's" Similarity Feature.

# Add stats like PA, BA, OBP, SLG, BABIP to df.
batting <- battingStats()
# Remove NA and add the age of player
batting <- batting %>%
  replace_na(list(SF = 0, HBP = 0)) %>%
  inner_join(People, by = "playerID") %>%
  mutate(Birthyear = ifelse(birthMonth >= 7,
                            birthYear + 1, birthYear),
         Age = yearID - Birthyear)
# Omit players from too long ago where age wasn't recorded.
batting <- batting %>% drop_na(Age)

# Add col for BA, OBP, SLG, etc.
batting <- batting %>%
  mutate(SO_pct = SO/PA,
         HR_pct = HR/PA,
         BB_pct = BB/PA,
         BBperSO = BB/SO)

# Select only needed cols
batting_stats <- batting %>%
  select(playerID, nameFirst, nameLast, Age, yearID, G, AB, PA, R, H, X2B, X3B,
         HR, RBI, BB, SO, SB, BA, OBP, SlugPct, OPS, BABIP, SO_pct, HR_pct, BB_pct, BBperSO)

# Filter for only players with > 300 PA to eliminate short unwanted seasons.
batting_stats <- batting_stats %>% filter(PA > 300)

# Calculate percentiles for each of the stats
batting_pctls <- batting_stats %>%
  group_by(yearID) %>%
  mutate(AB_pctl = rank(AB)/length(AB) * 100,
         R_pctl = rank(R)/length(R) * 100,
         H_pctl = rank(H)/length(H) * 100,
         X2B_pctl = rank(X2B)/length(X2B) * 100,
         X3B_pctl = rank(X3B)/length(X3B) * 100,
         HR_pctl = rank(HR)/length(HR) * 100,
         RBI_pctl = rank(RBI)/length(RBI) * 100,
         BB_pctl = rank(BB)/length(BB) * 100, 
         SO_pctl = rank(SO)/length(SO) * 100,
         SB_pctl = rank(SB)/length(SB) * 100,
         BA_pctl = rank(BA)/length(BA) * 100,
         OBP_pctl = rank(OBP)/length(OBP) * 100,
         SlugPct_pctl = rank(SlugPct)/length(SlugPct) * 100,
         OPS_pctl = rank(OPS)/length(OPS) * 100,
         BABIP_pctl = rank(BABIP)/length(BABIP) * 100,
         SO_pct_pctl = rank(SO_pct)/length(SO_pct) * 100,
         HR_pct_pctl = rank(HR_pct)/length(HR_pct) * 100,
         BB_pct_pctl = rank(BB_pct)/length(BB_pct) * 100,
         BBperSO_pctl = rank(BBperSO)/length(BBperSO) * 100) %>% 
  mutate(across(where(is.numeric), round, 2))

# Function to compute stats based on player's name and age
computeStats <- function(player_name, year) {
  flnames <- unlist(strsplit(player_name, " "))
  pID <- People %>%
    filter(nameFirst == flnames[1], nameLast == flnames[2]) %>%
    select(playerID)
  
  # Check if playerID is not empty
  if (!is_empty(pID$playerID)) {
    
    # Check if the player has any records in the batting data
    if (any(batting_pctls$playerID == pID$playerID)) {
      player <- batting_pctls %>%
        filter(playerID == pID$playerID, yearID == year) %>%
        select(
          nameFirst, nameLast, yearID, Age, BA_pctl, OBP_pctl,
          SlugPct_pctl, SO_pct_pctl, BB_pct_pctl, HR_pct_pctl)
      
      return(player)
    } else {
      # Return a message indicating that the player has no records in the batting data
      message("Player has no records in the batting data")
      return(data.frame(
        nameFirst = "Player has no records in the batting data",
        nameLast = "",
        Age = NA,
        yearID = year,
        BA_pctl = NA,
        OBP_pctl = NA,
        SlugPct_pctl = NA,
        SO_pct_pctl = NA,
        BB_pct_pctl = NA,
        HR_pct_pctl = NA
      ))
    }
  } else {
    # Return an empty data frame with appropriate columns
    return(data.frame(
      nameFirst = "Invalid player name or year",
      nameLast = "",
      Age = NA,
      yearID = year,
      BA_pctl = NA,
      OBP_pctl = NA,
      SlugPct_pctl = NA,
      SO_pct_pctl = NA,
      BB_pct_pctl = NA,
      HR_pct_pctl = NA
    ))
  }
}

# Function to compute similar players
computeSimilarity <- function(player_name, year) {
  flnames <- unlist(strsplit(player_name, " "))
  pID <- People %>%
    filter(nameFirst == flnames[1], nameLast == flnames[2]) %>%
    select(playerID)
  
  # Check if playerID is not empty
  if (!is_empty(pID$playerID) && (year >= 1873 && year <= 2022 && year != 2020)) {
    # Check if the player has any records in the batting data
    if (any(batting_pctls$playerID == pID$playerID)) {
      player <- batting_pctls %>% filter(playerID == pID$playerID, yearID == year)
      if (nrow(player) > 0) {
        batting_pctls %>%
          filter(playerID != pID$playerID) %>%
          mutate(similarity = 100 -
                   abs(BA_pctl - player$BA_pctl) -
                   abs(OBP_pctl - player$OBP_pctl) -
                   abs(SlugPct_pctl - player$SlugPct_pctl) -
                   abs(SO_pct_pctl - player$SO_pct_pctl) -
                   abs(BB_pct_pctl - player$BB_pct_pctl) -
                   abs(HR_pct_pctl - player$HR_pct_pctl)) %>%
          arrange(desc(similarity)) %>%
          select(nameFirst, nameLast, yearID, Age, similarity) %>%
          head(5)
      } else {
        # Return an empty data frame with appropriate columns
        return(data.frame(
          nameFirst = character(),
          nameLast = character(),
          yearID = numeric(),
          Age = numeric(),
          similarity = numeric()
        ))
      }
    } else {
      # Return an empty data frame with appropriate columns
      return(data.frame(
        nameFirst = character(),
        nameLast = character(),
        yearID = numeric(),
        Age = numeric(),
        similarity = numeric()
      ))
    }
  } else {
    # Return an empty data frame with appropriate columns
    return(data.frame(
      nameFirst = character(),
      nameLast = character(),
      yearID = numeric(),
      Age = numeric(),
      similarity = numeric()
    ))
  }
}

# Define UI
ui <- fluidPage(
  
    mainPanel(
      div(
        style = "margin-left: auto; margin-right: auto; text-align: left; max-width: 800px",

      HTML("<div style='font-size: 40px;'><b>MLB Similarity Calculator:
      How Unique of a Hitter is Someone Like Kyle Schwarber Actually?</b></div>
      <div style='font-size: 20px;'>Nolan Lo</div>
           <div style='font-size: 20px;'>December 27th, 2023</div>
           <br />
           <div style='font-size: 26px;'><b>Introduction</b></div>
           <div style='font-size: 16px;'>
           Bill James created the concept of a similarity score, a score that
            depicts how similar two players are out of 1,000, with a higher score meaning
            more similar. His score would start at 1,000 deduct points for differences in counting
            stats (exact formula in the appendix) and account for aspects such as the
            position of the players. My goal is slightly different. Taking inspiration
            from Crafted NBA's similarity feature, my goal is to find which players
            resemble each other most stylistically in purely batting. For example,
            is there anyone in history who closely resembles Kyle Schwarber's 
            very unique batting stats from his age-29 season? </div>
           <br />"),
      
      DTOutput("stats_table1"),
      
      HTML("<br />
          <div style='font-size: 16px;'>
           Just knowing Schwarber as a hitter, he is unique because of his poor
            batting average, yet decent on base percentage. Where he
            lacks in batting average, he more than makes up for it in walks and power.
            To be able to compare him to other players, I took the stats that I
            believe best characterize a player's batting style: how often they
            get hits, get on base, strike out, walk, hit home runs, and their
            overall slugging percentage. These are just some of the few characteristics
            of a batter that I chose, but one can certainly include more to get even more
            detailed. I took it a step further by calculating the percentile rank in the 
            given year for each of Schwarber's stats. Ex: Schwarber's 98.86 HR% percentile
            means he had a higher HR% than 98.86% of the league in 2022. This
            is important because, across different eras, results like hitting home runs were 
            accomplished at completely different rates. This is the approach Crafted
            NBA used in their similarity feature.</div>
           <br />
           <div style='font-size: 26px;'><b>Calculating the Similarity Score</b></div>
           <div style='font-size: 16px;'>
           To calculate the similarity score, start with a perfect score of 100 and subtract
            the difference between each of the chosen percentile statistics. Using this
            formula, we can take a look at which players in history are most similar
            to Kyle Schwarber. Note: Only players with more than 300 plate appearances
            in their respective seasons are accounted for.
           <br /></div>"),
      
      DTOutput("similarity_table1"),
      
      HTML("<br />
          <div style='font-size: 16px;'>
           We see here that Darryl Strawberry's 1998 season, Joe Hauser's 1928 season,
            and Harmon Killebrew's 1959 season are the most similar to Schwarber's
            2022 season. We can take a closer look to see how each of them ranked
            in the chosen stats in their respective seasons compared to Schwarber.
           <br /><br /></div>"),
      
      DTOutput("stats_table2"),
      
      HTML("<br />
          <div style='font-size: 16px;'>
           As we can see, all four players had poor and nearly identical BA rankings,
            slightly above average OBP rankings, elite slugging, BB%, and
            HR% rankings, and were among the top in SO%. Both Hauser and Killebrew in
            their respective seasons had characteristically  nearly identical seasons
            to Schwarber's 2022 season.</div>
           <br />
           <div style='font-size: 26px;'><b>Similarity Score Calculator</b></div>
           <div style='font-size: 16px;'>
           You can enter your own desired player and year to see which other players
            in history had a season most similar. The calculator will first output
             the five most similar players, then output each of their percentile
             stats to make comparing easy. All players with more than 300
             plate appearances for the given season and seasons from 1873–2022
            (excluding 2020 because of COVID) are valid.</div>
           <br />"),
      
      textInput("playerName", "Enter Player's Name", "Shohei Ohtani"),
      numericInput("year", "Enter Desired Year", 2022, min = 1873),
      
      DTOutput("similarity_table2"),
      HTML("<br />"),
      DTOutput("stats_table3"),
      
      HTML("<br />
      <div style='font-size: 26px;'><b>Appendix</b></div>
           <div style='font-size: 16px;'>
           Bill James's Similarity Score: https://www.baseball-reference.com/about/similarity.shtml</div>
           <div style='font-size: 16px;'>
           Crafted NBA's Similarity Feature: https://craftednba.com/blog/introducing-the-crafted-nba-similarity-app</div>
           <br />
           <br />")
        )
      )
  )

# Define server
server <- function(input, output, session) {
  
  output$stats_table1 <- renderDT({
    player_stats1 <- computeStats("Kyle Schwarber", 2022)
    
    datatable(
      player_stats1,
      options = list(dom = 't', paging = FALSE))
  })
  
  similarity1 <- computeSimilarity("Kyle Schwarber", 2022)
  
  output$similarity_table1 <- renderDT({
    datatable(
      similarity1,
      options = list(dom = 't', paging = FALSE)
    )
  })
  
  
  output$stats_table2 <- renderDT({
    player_stats2 <- bind_rows(
      computeStats("Kyle Schwarber", 2022),
      computeStats("Darryl Strawberry", 1998),
      computeStats("Joe Hauser", 1928),
      computeStats("Harmon Killebrew", 1959))
    
    datatable(
      player_stats2,
      options = list(dom = 't', paging = FALSE))
  })
  
  observe({
    req(input$playerName, input$year)
    
    similarity2 <- computeSimilarity(input$playerName, input$year)
    
    # Check if the data frame is not empty
    if (!is_empty(similarity2)) {
      output$similarity_table2 <- renderDT({
        datatable(
          similarity2,
          options = list(dom = 't', paging = FALSE)
        )
      })
    } else {
      output$similarity_table2 <- renderText("Invalid player name. Please enter a valid player name.")
    }
  })
  
  observe({
    req(input$playerName, input$year)
    
    similarity2 <- as.data.frame(computeSimilarity(input$playerName, input$year)) 
    
    # Check if the data frame is not empty
    if (!is_empty(similarity2)) {
      output$stats_table3 <- renderDT({
        player_stats3 <- bind_rows(
          computeStats(input$playerName, input$year),
          computeStats(paste(similarity2[1,1], similarity2[1,2], sep=" "), similarity2[1,3]),
          computeStats(paste(similarity2[2,1], similarity2[2,2], sep=" "), similarity2[2,3]),
          computeStats(paste(similarity2[3,1], similarity2[3,2], sep=" "), similarity2[3,3]),
          computeStats(paste(similarity2[4,1], similarity2[4,2], sep=" "), similarity2[4,3]),
          computeStats(paste(similarity2[5,1], similarity2[5,2], sep=" "), similarity2[5,3]))
        
        datatable(
          player_stats3,
          options = list(dom = 't', paging = FALSE)
        )
      })
    } else {
      output$stats_table3 <- renderText("Invalid player name. Please enter a valid player name.")
    }
  })
} 

# Run the Shiny app
shinyApp(ui, server)
