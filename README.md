# Background
Welcome! This is an RShiny dashboard that was built for the Fifth Third Bank Ping Pong League, but could easily be adapted for other uses. The goal of this project was to design a tool where people could compete in ping pong matches and submit results on their own to a platform that would automatically update player standings as new matches come in, as well as to ensure that those matches abided by the rules of the tournament. This dashboard accepts user input and checks it against a variety of conditions highlighted in the dashboard to ensure matches are accurately recorded.

# Files included:
- app.R - RShiny dashboard (functions, UI, and Server combined into one file) that contains all the code to run the dashboard
- matches.csv - a record of all ping pong matches submitted
- standings.csv - a record of current standing for all players who have submitted a match
Each of the matches.csv and standings.csv files are formatted in such a way to render a blank dashboard upon first use. Any matches submitted through the dashboard will populate into these files.

# Code Structure
- Installing/loading packages and setting current working directory 
  - Need to have app.R, matches.csv, and standings.csv all in the same working directory
- Defining four functions to process data and generate dashboard
  - check_conditions: checks conditions of an entry to the dashboard to make sure it follows the preset rules
  - update_match_data: updates matches.csv for valid matches
  - calculate_points: determine how many points each player should get from a particular match based on a preset points system
  - standings_bar_chart: outputs a bar chart of the current/new standings after a match has been submitted
- UI and Server are then defined to display rules for playing, a table of matches submitted on the current day, and a bar chart showing the current standings; they also accept user input for a particular match (and make sure that input is in an acceptable format)
