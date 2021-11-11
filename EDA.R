library(tidyverse)
library(lubridate)

# Importing the dataset
imdb <- read_csv("imdb.csv", 
                       col_types = cols(Date = col_date(format = "%Y"), 
                                        Rate = col_number(), 
                                        Votes = col_number(), 
                                        Duration = col_number()))
imdb_movies <- imdb %>% 
  filter(Type == 'Film')

imdb_movies <- imdb_movies[-c(7,9)]

# Converting datatypes
imdb_movies$Date <- year(imdb_movies$Date)
imdb_movies$Certificate <- as.factor(imdb_movies$Certificate)
imdb_movies$Nudity <- as.factor(imdb_movies$Nudity)
imdb_movies$Violence <- as.factor(imdb_movies$Violence)
imdb_movies$Profanity <- as.factor(imdb_movies$Profanity)
imdb_movies$Alcohol <- as.factor(imdb_movies$Alcohol)
imdb_movies$Frightening <- as.factor(imdb_movies$Frightening)

# Viewing the data
View(imdb_movies)

# Summary Statistics
summary(imdb_movies)

