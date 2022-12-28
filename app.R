### Required Packages
library(tidyverse)
library(ggrepel)
library(shiny)
library(shinythemes)

# Load data frames: 
# load data and data dictionary.
prem.dat <- read_csv("EPL1819.csv")
prem.dat <- prem.dat[,1:23] %>%
  # Remove features
  select(-Div, -Referee) %>%
  mutate(Week = ceiling(1:380 / 10))


team.stats <- data.frame(Teams = unique(prem.dat$HomeTeam))
for(i in 1:nrow(team.stats)) {
  team.stats$HG[i] <- sum(prem.dat$FTHG[which(prem.dat$HomeTeam == team.stats$Teams[i])])
  team.stats$HA[i] <- sum(prem.dat$FTAG[which(prem.dat$HomeTeam == team.stats$Teams[i])])
  team.stats$AG[i] <- sum(prem.dat$FTAG[which(prem.dat$AwayTeam == team.stats$Teams[i])])
  team.stats$AA[i] <- sum(prem.dat$FTHG[which(prem.dat$AwayTeam == team.stats$Teams[i])])
  # Find the end of season points for each team as well. 
  team.stats$Hpoints[i] <- sum(prem.dat$HomeTeam == team.stats$Teams[i] & prem.dat$FTR == "H")
  team.stats$Apoints[i] <- sum(prem.dat$AwayTeam == team.stats$Teams[i] & prem.dat$FTR == "A")
  team.stats$Dpoints[i] <- sum((prem.dat$HomeTeam == team.stats$Teams[i] | prem.dat$AwayTeam == team.stats$Teams[i]) & team.stats$FTR == "D")
  team.stats$TotalPoints <- (3 * (team.stats$Hpoints + team.stats$Apoints)) + team.stats$Dpoints
}
team.stats <- team.stats %>%
  mutate(TotalScored = HG + AG) %>%
  mutate(TotalAllowed = HA + AA) %>%
  mutate(GoalDifference = TotalScored - TotalAllowed) %>%
  select(-Hpoints, -Apoints, -Dpoints)



# User Interface
user <- fluidPage(
  theme = "spacelab",
  titlePanel("Match Data Finder (2018-19)"),
  sidebarLayout(position = "left", 
                sidebarPanel(
                  # Ask for input.
                  tags$h3("User Input :"),
                  tags$h6("Please choose the two different teams to view."),
                  selectInput("team1", "Team 1 :", choices = c(team.stats$Teams[str_order(unique(team.stats$Teams))]), selected = NULL),
                  selectInput("team2", "Team 2 :", choices = c(team.stats$Teams[str_order(unique(team.stats$Teams), decreasing = TRUE)]), selected = NULL)),
                
                mainPanel(
                  # Show output.
                  h1("Analysis"),
                  textOutput("Textoutput"),
                  
                  fluidRow(
                    column(10, plotOutput("HomevsAway")),
                    column(10, textOutput("match1output")),
                    column(10, plotOutput("AwayvsHome")),
                    column(10, textOutput("match2output")),
                    column(10, plotOutput("Total"))
                  )
                )
  )
)

server <- function(input, output) {
  
  # Retrieve Match information and store it in a data frame.
  matches <- reactive ({
    as.data.frame(rbind(prem.dat[which(prem.dat$HomeTeam == input$team1 & prem.dat$AwayTeam == input$team2),], 
                     prem.dat[which(prem.dat$HomeTeam == input$team2 & prem.dat$AwayTeam == input$team1),])) %>%
      arrange(Week) %>%
      mutate(Match = row_number()) 
      # Make the data longer for graphing later.
  })
    output$Textoutput <- renderText({
      if (input$team1 != input$team2) {
        paste(input$team1, " played ", input$team2, " in weeks ", matches()$Week[1], " and ", matches()$Week[2], ".", sep = "")
      } else {
        paste("Must pick two different teams.")
      }
    })
    
    output$HomevsAway <- renderPlot({
      match1 <- matches()[1,]
      match1 <- pivot_longer(match1, cols = c("FTHG", "FTAG", "HTHG", "HTAG"), names_to = "Side", values_to = "Goals") %>%
        mutate(Period = ifelse(Side == "FTHG" | Side == "FTAG", "Full Time", "Half Time")) %>%
        mutate(Team = ifelse(Side == "FTHG" | Side == "HTHG", HomeTeam, AwayTeam)) 
      
      # Rename values.
      match1$Side[1] <- paste("FT ", match1$HomeTeam[1], sep = "")
      match1$Side[2] <- paste("FT ", match1$AwayTeam[1], sep = "")
      match1$Side[3] <- paste("HT ", match1$HomeTeam[1], sep = "")
      match1$Side[4] <- paste("HT ", match1$AwayTeam[1], sep = "")
      
      ggplot(match1, aes(x = Team, y = Goals, fill = Team)) + 
        geom_bar(stat = "identity") + 
        facet_grid(cols = vars(Period), switch = "x") +
        ggtitle(paste("Week ", match1$Week[1], sep = "")) +
        theme_linedraw() +
        theme(plot.title = element_text(hjust = 0.5)) 

    })
    
    output$match1output <- renderText({
      if (matches()$FTR[1] == "H") {
        paste("In Week", matches()$Week[1], matches()$HomeTeam[1], "beat", matches()$AwayTeam[1], "with a score of", matches()$FTHG[1], "-", matches()$FTAG[1], sep =" ")
      } else if (matches()$FTR[1] == "D") {
        paste("In Week", matches()$Week[1], matches()$HomeTeam[1], "drew against", matches()$AwayTeam[1], "with a score of", matches()$FTHG[1], "-", matches()$FTAG[1], sep =" ")
      } else {
        paste("In Week", matches()$Week[1], matches()$AwayTeam[1], "beat", matches()$HomeTeam[1], "with a score of", matches()$FTAG[1], "-", matches()$FTHG[1], sep =" ")
      }
    })
    
    output$AwayvsHome <- renderPlot({
      match2 <- matches()[2,]
      match2 <- pivot_longer(match2, cols = c("FTHG", "FTAG", "HTHG", "HTAG"), names_to = "Side", values_to = "Goals") %>%
        mutate(Period = ifelse(Side == "FTHG" | Side == "FTAG", "Full Time", "Half Time")) %>%
        mutate(Team = ifelse(Side == "FTHG" | Side == "HTHG", HomeTeam, AwayTeam)) 
      
      # Rename values.
      match2$Side[1] <- paste("FT ", match2$HomeTeam[1], sep = "")
      match2$Side[2] <- paste("FT ", match2$AwayTeam[1], sep = "")
      match2$Side[3] <- paste("HT ", match2$HomeTeam[1], sep = "")
      match2$Side[4] <- paste("HT ", match2$AwayTeam[1], sep = "")
      
      ggplot(match2, aes(x = Team, y = Goals, fill = Team)) + 
        geom_bar(stat = "identity") + 
        facet_grid(cols = vars(Period), switch = "x") +
        ggtitle(paste("Week ", match2$Week[1], sep = "")) +
        theme_linedraw() +
        theme(plot.title = element_text(hjust = 0.5)) 
    })

    output$match2output <- renderText({
      if (matches()$FTR[2] == "H") {
        paste("In Week", matches()$Week[2], matches()$HomeTeam[2], "beat", matches()$AwayTeam[2], "with a score of", matches()$FTHG[2], "-", matches()$FTAG[2], sep =" ")
      } else if (matches()$FTR[2] == "D") {
        paste("In Week", matches()$Week[2], matches()$HomeTeam[2], "drew against", matches()$AwayTeam[2], "with a score of", matches()$FTHG[2], "-", matches()$FTAG[2], sep =" ")
      } else {
        paste("In Week", matches()$Week[2], matches()$AwayTeam[2], "beat", matches()$HomeTeam[2], "with a score of", matches()$FTAG[2], "-", matches()$FTHG[2], sep =" ")
      }
    })
    
    output$Total <- renderPlot({
      reqstats <- team.stats %>% filter(Teams == input$team1 | Teams == input$team2)
      ggplot(team.stats, aes(x = TotalScored, y = TotalAllowed)) +
        geom_point() +
        coord_equal() +
        
        # Add an extra layer to highlight the two selected teams 'red'
        geom_point(data = reqstats, aes(x = TotalScored, y = TotalAllowed), color = "red") +
        geom_text_repel(aes(label = Teams)) + 
        labs(xlab = "Goals Scored", ylab = "Goals Allowed", title = "Goals Scored to Allowed") +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5)) 
    })
    
    
}


shinyApp(ui = user, server = server)
