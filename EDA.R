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

colnames(imdb_movies)

# Summary Statistics
summary(imdb_movies)

released_movies <- imdb_movies %>% 
  filter(Date < '2022') %>%
  drop_na() %>%
  unique()

summary(released_movies)


released_movies %>% 
  ggplot(aes(x = Certificate, y = Rate, col = Certificate)) + geom_boxplot()

released_movies %>% 
  ggplot(aes(x = Certificate, y = Votes, col = Certificate)) + geom_boxplot()

released_movies %>% 
  ggplot(aes(x = Nudity, y = Rate, col = Nudity)) + geom_boxplot()

released_movies %>% 
  ggplot(aes(x = Violence, y = Rate, col = Violence)) + geom_boxplot()

released_movies %>% 
  ggplot(aes(x = Profanity, y = Rate, col = Profanity)) + geom_boxplot()

released_movies %>% 
  ggplot(aes(x = Alcohol, y = Rate, col = Alcohol)) + geom_boxplot()

released_movies %>% 
  ggplot(aes(x = Frightening, y = Rate, col = Frightening)) + geom_boxplot()

released_movies %>% 
  ggplot(aes(Rate)) + 
  geom_histogram(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(Rate)),
             color="blue", 
             linetype="dashed", 
             size=1)

released_movies %>% 
  ggplot(aes(log(Votes))) + 
  geom_histogram(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(log(Votes))),
             color="blue", 
             linetype="dashed", 
             size=1)

released_movies %>% 
  select(c(Date,Rate,Votes,Duration)) %>% 
  drop_na() %>%
  cor()


profanity_movies
