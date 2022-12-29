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
                    column(10, plotOutput("bothMatches")),
                    column(10, plotOutput("WeekStats")),
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
        paste("In the 2018-2019 season, ", input$team1, " played ", input$team2, " on ", 
              matches()$Date[1], " and ", matches()$Date[2], " ( in weeks ",matches()$Week[1], " and ", matches()$Week[2], " ).", sep = "")
      } else {
        paste("The two selections cannot be the same. Please reselect.")
      }
    })
    
    output$bothMatches <- renderPlot({
      matchgoals <- pivot_longer(matches(), cols = c("FTHG", "FTAG"), names_to = "Side", values_to = "Goals") %>%
        mutate(Team = ifelse(Side == "FTHG", HomeTeam, AwayTeam)) 
      
      # Rename values.
      matchgoals$Side[1] <- paste("FT ", matchgoals$Team[1], sep = "")
      matchgoals$Side[2] <- paste("FT ", matchgoals$Team[2], sep = "")
      matchgoals$Side[3] <- paste("FT ", matchgoals$Team[3], sep = "")
      matchgoals$Side[4] <- paste("FT ", matchgoals$Team[4], sep = "")
      
      
      ggplot(matchgoals, aes(x = Side, y = Goals, fill = Team)) + 
        geom_bar(stat = "identity") + 
        facet_grid(cols = vars(Date), switch = "x") +
        ggtitle("Match Scores") +
        theme(plot.title = element_text(hjust = 0.5)) 

    })
    
    
    output$WeekStats <- renderPlot({
      team1 <- prem.dat %>% 
        filter(HomeTeam == input$team1 | AwayTeam == input$team1) %>%
        mutate(Goals = ifelse(HomeTeam == input$team1, FTHG, FTAG)) %>%
        mutate(Team = input$team1) %>%
        select(Team, Goals, Week)
      team2 <- prem.dat %>% 
        filter(HomeTeam == input$team2 | AwayTeam == input$team2) %>%
        mutate(Goals = ifelse(HomeTeam == input$team2, FTHG, FTAG)) %>%
        mutate(Team = input$team2) %>%
        select(Team, Goals, Week)
      
      weekstats <- as.data.frame(rbind(team1, team2))
        
      # Graph team 1 goals per week.
      ggplot(weekstats, aes(x = Week, y = Goals, color = Team)) + 
        geom_line()+
        ylim(0,8) +
        theme_classic() +
        labs(title = "Week to Week Goals Scored") +
        theme(plot.title = element_text(hjust = 0.5)) 
      
        
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
