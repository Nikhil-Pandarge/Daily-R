# Clear R workspace
rm(list = ls() ) 

# Declare a variable to read and store moviesData  
movies <- read.csv("moviesData.csv")

# View movies data frame 
View(movies)

# Scores by India and Australia in three ODIs
inScore <- c(320, 260, 240)
ausScore <- c(280, 268, 240)

