

# code for installing packages
# r = getOption("repos")
# r["CRAN"] = "https://jfrog.info53.com/artifactory/rproject-remote/"
# options(repos = r)
# rm(r)


### load libraries

# if necessary packages are not installed, install them
if (!require(shiny)){
    install.packages('shiny')
}
if (!require(dplyr)){
    install.packages('dplyr')
}
if (!require(DT)){
    install.packages('DT')
}
if (!require(data.table)){
    install.packages('data.table')
}
if (!require(lubridate)){
    install.packages('lubridate')
}
if (!require(ggplot2)){
    install.packages('ggplot2')
}
if (!require(stringr)){
    install.packages('stringr')
}
if (!require(shinyjs)){
    install.packages('shinyjs')
}if (!require(shinycssloaders)){
    install.packages('shinycssloaders')
}

library(shiny)
library(dplyr)
library(DT)
library(data.table)
library(lubridate)
library(ggplot2)
library(stringr)
library(shinyjs)
library(shinycssloaders)




# change working directory
setwd("/Users/Jonathan/Desktop/ping_pong_testing")
#setwd("/mnts/shared/consumerdsg/Party_Cats/Users/Jonathan/pp_db")





######### define helper functions: check_conditions, update_match_data, calculate_points (to update standings), standings_bar_chart

# define a function to check conditions to test whether a match entry is valid
check_conditions <- function(p1, p2, match_data){
    
    # check if the rules are met:
    # condition 1: one player can submit at most 2 matches in a day
    # condition 2: one player can submit at most 6 matches in a calendar week
    # condition 3: one player can only submit at most 1 match against the same player in a day
    # condition 4: one player can only submit at most 3 matches against the same player in a calendar week
    
    # match is considered valid until a condition is broken
    valid_match <- TRUE
    broken_condition <- ""
    
    # check condition 1 - a player can't be in either p1 or p2 more than twice in a given day - check twice for p1 and p2 individually
    day_filter <- match_data[date == Sys.Date()]
    
    if (sum(day_filter$p1_name == p1) + sum(day_filter$p2_name == p1) >= 2){
        valid_match <- FALSE
        if (broken_condition == ''){
            broken_condition <- paste0(p1, " has already submitted two matches today.")
        }
    } else if (sum(day_filter$p1_name == p2) + sum(day_filter$p2_name == p2) >= 2){
        valid_match <- FALSE
        if (broken_condition == ''){
            broken_condition <- paste0(p2, " has already submitted two matches today.")
        }
    }
    
    # check condition 2 - a player can't be in either p1 or p2 more than 6 times in a calendar week
    start_of_week <- floor_date(Sys.Date(), "weeks", week_start = 0)
    week_filter <- match_data[date >= start_of_week]
    
    if (sum(week_filter$p1_name == p1) + sum(week_filter$p2_name == p1) >= 6){
        valid_match <- FALSE
        if (broken_condition == ''){
            broken_condition <- paste0(p1, " has already submitted six matches this week.")
        }
    } else if (sum(week_filter$p1_name == p2) + sum(week_filter$p2_name == p2) >= 6){
        valid_match <- FALSE
        if (broken_condition == ''){
            broken_condition <- paste0(p2, " has already submitted six matches matches this week.")
        }
    }
    
    # check condition 3 - one player can only submit at most 1 match against the same player in a day
    if (sum((day_filter$p1_name == p1 | day_filter$p2_name == p1) & (day_filter$p1_name == p2 | day_filter$p2_name == p2)) >= 1){
        valid_match <- FALSE
        if (broken_condition == ''){
            broken_condition <- "This matchup has already been submitted today."
        } else {
            broken_condition <- paste0(broken_condition, " ", "This matchup has already been submitted today.")
        }
    }
    
    # check condition 4 - one player can only submit at most 3 matches against the same player in a calendar week
    if (sum((week_filter$p1_name == p1 | week_filter$p2_name == p1) & (week_filter$p1_name == p2 | week_filter$p2_name == p2)) >= 3){
        valid_match <- FALSE
        if (broken_condition == ''){
            broken_condition <- "This matchup has already been submitted three times this week."
        } else {
            broken_condition <- paste0(broken_condition, " ", "This matchup has already been submitted three times this week.")
        }
    }
    
    return(list(valid = valid_match, condition = broken_condition))
    
}

# define a function to update match data if the row is a valid entry based on the rules of the challenge
update_match_data <- function(p1, p2, p1_score, p2_score, winner, num_games_played, match_data){
    
    # remove the row we use to ensure data table is read in properly
    match_data <- match_data[p1_name != 'DELETE']
    
    # add new row to match data
    match_data <- rbind(match_data, list(date = Sys.Date(), p1_name = p1, p2_name = p2, p1_points = p1_score, p2_points = p2_score,
                                         winner = winner, games_played = num_games_played))
    return(match_data)
}

# define a function to calculate the points of a match
calculate_points <- function(p1, p2, p1_score, p2_score, winner, num_games_played, current_standings){
    
    # calculate values needed for scoring
    p1_current_points <- ifelse(p1 %in% current_standings$name, current_standings[name==p1, points], 0)
    p2_current_points <- ifelse(p2 %in% current_standings$name, current_standings[name==p2, points], 0)
    
    # calculate p1 points for the match
    p1_points <- 10 * ifelse(p1 == winner, 1, 0) +          # winner points
        round(10 * p1_score/(p1_score + p2_score)) +        # match differential points
        min(max(ifelse(p1_current_points < p2_current_points,   # bonus points
                       round(abs(p1_current_points - p2_current_points) *
                                 (p1_score/(p1_score + p2_score) -
                                      p1_current_points/(p1_current_points + p2_current_points))), 0), 0), round(0.5*abs(p1_current_points - p2_current_points)))
    
    # calculate p2 points for the match
    p2_points <- 10 * ifelse(p2 == winner, 1, 0) +          # winner points
        round(10 * p2_score/(p1_score + p2_score)) +        # match differential points
        min(max(ifelse(p2_current_points < p1_current_points,   # bonus points
                       round(abs(p2_current_points - p1_current_points) *
                                 (p2_score/(p1_score + p2_score) -
                                      p2_current_points/(p1_current_points + p2_current_points))), 0), 0), round(0.5*abs(p1_current_points - p2_current_points)))
    
    # create data table with player names and points for the match
    match_results <- data.table(name = c(p1, p2), points = c(p1_points, p2_points))
    
    # return a data table with two rows, name and points for player 1 and player 2
    return(match_results)
    
}

# define function to return bar chart of standings
standings_bar_chart <- function(standings){
    
    # if standings has 0 rows, output blank chart; otherwise, proceed with logic for updating standings graph
    if (nrow(standings) == 0){
        
        # create blank graph
        ggplot() + geom_bar() + theme_minimal() + xlab("Player") + ylab("Points") + coord_flip()
        
    } else{
        
        # create a ranking order for people in standings - rank by points and alphabetical secondary
        standings <- standings[order(-points, name)] %>% mutate(rank = 1:n())
        
        # order the standings dataframe as a factor on name by points to help with coloring for ease of reading
        standings$name <- reorder(as.factor(standings$name), standings$rank)
        
        # create a color variable
        standings$color <- rep(c("#00AF66", "#1D4094"), ceiling(length(standings$rank)/2))[1:length(standings$rank)]
        
        # create graph
        ggplot(standings, aes(x=name, y=points)) + geom_col(aes(fill = name)) +
            geom_text(aes(label=points, y = max(standings$points) + 5), size=5, color = standings$color) +
            theme_minimal() + coord_flip() +
            theme(panel.grid.minor = element_blank()) +
            theme(panel.grid.major = element_blank()) +
            theme(axis.text.x = element_blank()) +
            theme(axis.text.y = element_text(size=13)) +
            xlab("Player") + ylab("Points") +
            scale_fill_manual(values=standings$color) +
            theme(legend.position = 'none') +
            theme(axis.title=element_text(size=14,face="bold")) +
            scale_x_discrete(limits = rev(levels(standings$name)))
        
    }
    
}


# create UI for dashboard
ui <- fluidPage(
    
    # for hiding/showing certain outputs
    shinyjs::useShinyjs(),
    
    # Application title
    h1("Fifth Third Ping Pong League", style = "text-align: center; border-bottom: 1px solid #D3D3D3; padding-bottom: 10px"),
    
    # split up the standings bar chart and the welcome message in a fluid row
    fluidRow(
        column(7,
               # welcome message
               h5("Welcome to the Fifth Third Ping Pong League! Since we have so many people who play across departments in Fifth Third Tower, we wanted to create a place for people to meet others that play ping pong outside of their immediate teams. Overall, we want to encourage people to play more matches and see how they stack up in some friendly competition. Standings will be refreshed quarterly. Anyone is welcome to join, so feel free to challenge anyone in the standings! Good luck!",
                  style = 'border-bottom: 0.5px solid #D3D3D3; padding-bottom: 10px; font-weight: normal'),
               
               # rules and scoring
               h4("Rules:"),
               h5("1. Players can submit a maximum of 2 matches per day and 6 matches per week.",
                  style = "font-weight: normal"),
               h5("2. Players can play against the same player up to 1 time per day and 3 times per week.",
                  style = "border-bottom: 1px solid #D3D3D3; padding-bottom: 10px; font-weight: normal"),
               
               h4("Scoring: "),
               h5("1. Winning: 10 points are awarded for winning.",
                  style = "font-weight: normal"),
               h5("2. Score differential: An additional 10 points are split based on the score of the match.",
                  style = "font-weight: normal"),
               h5("3. Bonus points: Bonus points are awarded based on actual vs. expected performance in a match. Bonus points are only awarded to players with fewer overall points than their opponent, and they are calculated as the difference between the ratio of points won by that player in the match and the ratio of overall points had by that player in the standings between the 2 players. Bonus points are capped at 50% of the standings point differential between 2 players.",
                  style = "font-weight: normal"),
               
               
               # add all the inputs here
               # create a fluid row that has each of the inputs
               h3("Input a New Match", style = "text-align: center; border-top: 1px solid #D3D3D3; padding-top: 20px"),
               h5("To input a match, just fill out all the fields and click submit. Enter both the first and last names of each player. Feel free to submit 1 game or best 2 of 3 games as well: just enter the total points scored for both players across all games as well as the total number of games played.
       A message will display when the match is successfully entered!",
                  style = "font-weight: normal"),
               
               br(),
               
               fluidRow(
                   column(2, textInput(inputId = 'player1_name', label = "Player 1 Full Name")),
                   column(2, textInput(inputId = 'player2_name', label = "Player 2 Full Name")),
                   column(2, textInput(inputId = 'player1_score', label = "P1 Total Score")),
                   column(2, textInput(inputId = 'player2_score', label = "P2 Total Score")),
                   column(2, textInput(inputId = 'winner', label = "Winner Full Name")),
                   column(2, textInput(inputId = 'num_games_played', label = "# of Games Played")),
               ),
               
               # submit button
               fluidRow(
                   column(2, actionButton(inputId = 'submit_button', label = 'Submit')),
                   column(4, shinyjs::hidden(actionButton(inputId = 'confirm_new_player', label = 'Confirm new player')))
                   
               ),
               
               # output message for whether or not the submit was successful
               textOutput(outputId = 'submit_message') %>% withSpinner(color = "#1D4094"),
               
               hr(style = 'border-bottom: 1px solid #D3D3D3'),
               
               # table of daily match submissions
               h4('Daily Match Submissions'),
               DTOutput(outputId ='daily_submissions')
               
               
        ),
        
        # standing plot
        column(5, style = "height:700px",
               h3("Current Standings", style = "text-align: center"),
               plotOutput(outputId = 'standings_plot', height = "100%"))
        
    )
    
    
    
    
)

# create server for dashboard
server <- function(input, output) {
    
    # create reactive list that will have the standings graph, display message, match_data, and standings data
    outputs <- reactiveValues(message = "", standings_chart = standings_bar_chart(fread('standings.csv')),
                              match_data = fread('matches.csv') %>% mutate(date = as.Date(date, format = '%m/%d/%y')), standings = fread('standings.csv'))
    
    observeEvent(input$submit_button, {
        
        # load current match data
        match_data <- outputs$match_data
        #match_data$date <- as.Date(match_data$date, format = '%m/%d/%y')
        
        # load overall standings data
        standings <- outputs$standings
        
        
        # standardize formats of player names and numeric point values
        p1 <- str_trim(tolower(input$player1_name)) %>%
            str_replace_all(" ", "_") %>%
            str_replace_all("\\.", "") %>%
            str_replace_all("-", "")
        p2 <- str_trim(tolower(input$player2_name)) %>%
            str_replace_all(" ", "_") %>%
            str_replace_all("\\.", "") %>%
            str_replace_all("-", "")
        winner <- str_trim(tolower(input$winner)) %>%
            str_replace_all(" ", "_") %>%
            str_replace_all("\\.", "") %>%
            str_replace_all("-", "")
        p1_score <- as.numeric(str_trim(input$player1_score))
        p2_score <- as.numeric(str_trim(input$player2_score))
        num_games <- as.numeric(str_trim(input$num_games_played))
        
        
        
        # VALIDATE INPUTS
        
        # make sure no inputs are blank
        if (any(
            input$player1_name == "",
            input$player2_name == "",
            input$player1_score == "",
            input$player2_score == "",
            input$winner == "",
            input$num_games_played == ""
        )) {
            
            
            outputs$message <- "Error: Make sure to fill out all fields."
            
            # make bar chart as a reactive value that changes based on standings (these won't be updated)
            outputs$standings_chart <- standings_bar_chart(standings)
            
            
            # make sure numeric inputs can be accurately converted to numeric
        } else if (any(
            is.na(as.numeric((input$player1_score))),
            is.na(as.numeric((input$player2_score))),
            is.na(as.numeric((input$num_games_played)))
        )) {
            
            outputs$message <- "Error: Make sure points and number of games played fields are entered as just numbers."
            # make bar chart as a reactive value that changes based on standings (these won't be updated)
            outputs$standings_chart <- standings_bar_chart(standings)
            
            
            # check to make sure winner has more points in matchups where one game is played
        } else if (((winner == p1) & (p1_score <= p2_score) & (num_games == 1)) | ((winner == p2) & (p2_score <= p1_score) & (num_games == 1))) {
            
            outputs$message <- "Error: Winner has fewer points than opponent and one game was played.
            Check to make sure scores are entered correctly for both players."
            
            # make bar chart as a reactive value that changes based on standings (these won't be updated)
            outputs$standings_chart <- standings_bar_chart(standings)
            
            
            
            # winner name must be either p1 or p2
            
        } else if((winner != p1) & (winner != p2)) {
            
            # edit the message that will be displayed
            outputs$message <- "Erorr: Winner name is not in p1 or p2 input. Check to make sure names were typed correctly."
            
            # make bar chart as a reactive value that changes based on standings (these won't be updated)
            outputs$standings_chart <- standings_bar_chart(standings)
            
            
            # check to see if any player is recognized as a new player to the system and confirm that that's what the user wants to input
            
            # if both p1 and p2 are new, ask to confirm them both
            
        } else if ((!(p1 %in% c(match_data$p1_name, match_data$p2_name))) & (!(p2 %in% c(match_data$p1_name, match_data$p2_name)))){
            
            # edit the message that will be displayed
            outputs$message <- paste0("Confirm new players: ", p1, " and ", p2,
                                      " are not recognized in the system. Check the standings to make sure players have not submitted a previous match. If not, click 'confirm new player' to continue if this is their first match. If so, make sure names are spelled the same.")
            
            # make bar chart as a reactive value that changes based on standings (these won't be updated)
            outputs$standings_chart <- standings_bar_chart(standings)
            
            # show the confirm button to allow them to confirm the submission of a new player
            shinyjs::showElement(id = "confirm_new_player")
            
            # if just p1 is new, confirm p1
        } else if (!(p1 %in% c(match_data$p1_name, match_data$p2_name))){
            
            # edit the message that will be displayed
            outputs$message <- paste0("Confirm new player: ", p1,
                                      " is not recognized in the system. Check the standings to make sure this player has not submitted a previous match. If not, click 'confirm new player' to continue if this is their first match. If so, make sure names are spelled the same.")
            
            # make bar chart as a reactive value that changes based on standings (these won't be updated)
            outputs$standings_chart <- standings_bar_chart(standings)
            
            # show the confirm button to allow them to confirm the submission of a new player
            shinyjs::showElement(id = "confirm_new_player")
            
            # if just p2 is new, confirm p2
        } else if (!(p2 %in% c(match_data$p1_name, match_data$p2_name))){
            
            # edit the message that will be displayed
            outputs$message <- paste0("Confirm new player: ", p2,
                                      " is not recognized in the system. Check the standings to make sure this player has not submitted a previous match. If not, click 'confirm new player' to continue if this is their first match. If so, make sure names are spelled the same.")
            
            # make bar chart as a reactive value that changes based on standings (these won't be updated)
            outputs$standings_chart <- standings_bar_chart(standings)
            
            # show the confirm button to allow them to confirm the submission of a new player
            shinyjs::showElement(id = "confirm_new_player")
            
            
            # otherwise execute all the code to determine if a match is valid and if so, enter it into the system
        }  else {
            
            
            # check if it's a valid match
            valid_results <- check_conditions(p1, p2, match_data)
            
            # if it's a valid match, update the match data and the standings
            if (valid_results$valid == TRUE){
                
                # update match data
                match_data <- update_match_data(p1, p2, p1_score, p2_score, winner, num_games, match_data)
                
                # update standings - calculate points for the match and then adjust standings as a result
                new_points <- calculate_points(p1, p2, p1_score, p2_score, winner, num_games, standings)
                
                # get all player names from previous standings or new match in order to calculate all total points
                unique_names <- unique(c(standings$name, new_points$name))
                unique_points <- c()
                for (n in unique_names){
                    name_points <- sum(standings[name==n, points], new_points[name==n, points])
                    unique_points <- append(unique_points, name_points)
                }
                
                # update standings with new rows
                standings <- data.table(name = character(), points = numeric())
                for (i in 1:length(unique_names)){
                    standings <- rbind(standings, list(name = unique_names[i], points = unique_points[i]))
                }
                
                # sort standings by rank
                standings$rank <- frank(-standings$points, ties.method = 'average')
                standings <- standings %>% arrange(rank)
                
                # save out standings and match data
                fwrite(standings, "standings.csv")
                fwrite(match_data, "matches.csv")
                outputs$standings <- standings
                outputs$match_data <- match_data
                
                # make bar chart as a reactive value that changes based on standings
                outputs$standings_chart <- standings_bar_chart(standings)
                
                
                # print a successful message
                outputs$message <- paste0("Your submission was successful. ",
                                          new_points[1, name], " received ", new_points[1, points], " and ",
                                          new_points[2, name], " received ", new_points[2, points], ".")           
                
            } else {
                
                # set error message
                outputs$message <- paste0("Error: ", valid_results$condition)
                
                # make bar chart as a reactive value that changes based on standings (these won't be updated)
                outputs$standings_chart <- standings_bar_chart(standings)
                
            }
            
            
        }
        
        
        
    })
    
    
    # when confirm new player button is pressed, reset it to its original value and execute all the code to input a new match into standings
    observeEvent(input$confirm_new_player, {
        
        # hide the confirm new player button once it's submitted
        shinyjs::hide("confirm_new_player")
        
        
        # load current match data
        match_data <- outputs$match_data
        match_data$date <- as.Date(match_data$date, format = '%m/%d/%y')
        
        # load overall standings data
        standings <- outputs$standings
        
        # standardize formats of player names and numeric point values
        p1 <- str_trim(tolower(input$player1_name)) %>%
            str_replace_all(" ", "_") %>%
            str_replace_all("\\.", "") %>%
            str_replace_all("-", "")
        p2 <- str_trim(tolower(input$player2_name)) %>%
            str_replace_all(" ", "_") %>%
            str_replace_all("\\.", "") %>%
            str_replace_all("-", "")
        winner <- str_trim(tolower(input$winner)) %>%
            str_replace_all(" ", "_") %>%
            str_replace_all("\\.", "") %>%
            str_replace_all("-", "")       
        p1_score <- as.numeric(str_trim(input$player1_score))
        p2_score <- as.numeric(str_trim(input$player2_score))
        num_games <- as.numeric(str_trim(input$num_games_played))
        
        
        # check if it's a valid match
        valid_results <- check_conditions(p1, p2, match_data)
        
        # if it's a valid match, update the match data and the standings
        if (valid_results$valid == TRUE){
            
            # update match data
            match_data <- update_match_data(p1, p2, p1_score, p2_score, winner, num_games, match_data)
            
            # update standings - calculate points for the match and then adjust standings as a result
            new_points <- calculate_points(p1, p2, p1_score, p2_score, winner, num_games, standings)
            
            # get all player names from previous standings or new match in order to calculate all total points
            unique_names <- unique(c(standings$name, new_points$name))
            unique_points <- c()
            for (n in unique_names){
                name_points <- sum(standings[name==n, points], new_points[name==n, points])
                unique_points <- append(unique_points, name_points)
            }
            
            # update standings with new rows
            standings <- data.table(name = character(), points = numeric())
            for (i in 1:length(unique_names)){
                standings <- rbind(standings, list(name = unique_names[i], points = unique_points[i]))
            }
            
            # sort standings by rank
            standings$rank <- frank(-standings$points, ties.method = 'average')
            standings <- standings %>% arrange(rank)
            
            # save out standings and match data
            fwrite(standings, "standings.csv")
            fwrite(match_data, "matches.csv")
            outputs$standings <- standings
            outputs$match_data <- match_data
            
            # make bar chart as a reactive value that changes based on standings
            outputs$standings_chart <- standings_bar_chart(standings)
            
            
            # print a successful message
            outputs$message <- paste0("Your submission was successful. ",
                                      new_points[1, name], " received ", new_points[1, points], " and ",
                                      new_points[2, name], " received ", new_points[2, points], ".")           
            
        } else {
            
            # set error message
            outputs$message <- paste0("Error: ", valid_results$condition)
            
            # make bar chart as a reactive value that changes based on standings (these won't be updated)
            outputs$standings_chart <- standings_bar_chart(standings)
            
        }
        
        
    })
    
    # add text output for whether the entry was successful or not
    output$submit_message <- isolate(renderText(outputs$message))
    
    # bar chart for current standings
    output$standings_plot <- renderPlot(outputs$standings_chart)
    
    
    # table for daily match submissions
    output$daily_submissions <- renderDT({
        
        datatable(outputs$match_data[date == Sys.Date(), .(P1 = p1_name, P2 = p2_name,
                                                           `P1 Score` = p1_points, `P2 Score` = p2_points)],
                  class = "hover nowrap",
                  filter = "none",
                  options = list(dom = 't', pageLength = -1, scrollX = T)
        )
    })
    
    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)


