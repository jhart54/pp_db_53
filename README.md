# Welcome
Welcome! This is an RShiny dashboard that was built for the Fifth Third Bank Ping Pong League, but could easily be adapted for other uses. Files included here are:
- app.R - RShiny dashboard (functions, UI, and Server combined into one file) that contains all the code to run the dashboard
- matches.csv - a record of all ping pong matches submitted
- standings.csv - a record of current standing for all players who have submitted a match
Each of the matches.csv and standings.csv files are formatted in such a way to render a blank dashboard upon first use. Any matches submitted through the dashboard will populate into these files.

# Code Structure
- Installing/loading packages and setting current working directory 
  - Need to have app.R, matches.csv, and standings.csv all in the same working directory
