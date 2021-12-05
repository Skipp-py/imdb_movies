library(tidyverse)
library(lubridate)
library(corrplot)

# Importing the dataset
imdb <- read_csv("imdb.csv", 
                       col_types = cols(Date = col_date(format = "%Y"), 
                                        Rate = col_number(), 
                                        Votes = col_number(), 
                                        Duration = col_number()))

# Viewing the data prior to cleaning 
View(imdb)

imdb_movies <- imdb[-c(1,6,31)]

# Filtering for unique released movies (factors)
imdb_movies <- imdb_movies %>% 
  filter(Date < '2022') %>%
  drop_na() %>%
  unique()

imdb_movies$Date <- year(imdb_movies$Date)

# Filtering for unique released movies (numeric)
released_movies <- imdb_movies %>% 
  filter(Date < '2022') %>%
  drop_na() %>%
  unique()

released_wout_name <- released_movies[-1]

lm.test <- lm(Rate~., data = released_wout_name)
summary(lm.test)

qqnorm(released_movies$Rate, pch = 1, frame = FALSE)
qqline(released_movies$Rate, col = "steelblue", lwd = 2)
plot(lm.test, which=1)

released_movies %>% 
  select(-c(Name, Certificate, Nudity, Violence, Profanity, Frightening, Alcohol)) %>% 
  drop_na() %>%
  cor()

released_movies_numeric <- released_movies %>% 
  select(-c(Name, Certificate, Nudity, Violence, Profanity, Frightening, Alcohol)) %>% 
  drop_na() 

corrplot(cor(released_movies_numeric))

# Converting datatypes
imdb_movies$Date <- year(imdb_movies$Date)
imdb_movies$Certificate <- as.factor(imdb_movies$Certificate)
imdb_movies$Nudity <- as.factor(imdb_movies$Nudity)
imdb_movies$Violence <- as.factor(imdb_movies$Violence)
imdb_movies$Profanity <- as.factor(imdb_movies$Profanity)
imdb_movies$Alcohol <- as.factor(imdb_movies$Alcohol)
imdb_movies$Frightening <- as.factor(imdb_movies$Frightening)

imdb_movies$Action <- as.factor(imdb_movies$Action)
imdb_movies$Adventure <- as.factor(imdb_movies$Adventure)
imdb_movies$Animation <- as.factor(imdb_movies$Animation)
imdb_movies$Biography <- as.factor(imdb_movies$Biography)
imdb_movies$Comedy <- as.factor(imdb_movies$Comedy)
imdb_movies$Crime <- as.factor(imdb_movies$Crime)
imdb_movies$Documentary <- as.factor(imdb_movies$Documentary)
imdb_movies$Drama <- as.factor(imdb_movies$Drama)
imdb_movies$Family <- as.factor(imdb_movies$Family)
imdb_movies$Fantasy <- as.factor(imdb_movies$Fantasy)
imdb_movies$`Film-Noir` <- as.factor(imdb_movies$`Film-Noir`)
imdb_movies$History <- as.factor(imdb_movies$History)
imdb_movies$Horror <- as.factor(imdb_movies$Horror)
imdb_movies$Music <- as.factor(imdb_movies$Music)
imdb_movies$Musical <- as.factor(imdb_movies$Musical)
imdb_movies$Mystery <- as.factor(imdb_movies$Mystery)
imdb_movies$Romance <- as.factor(imdb_movies$Romance)
imdb_movies$`Sci-Fi` <- as.factor(imdb_movies$`Sci-Fi`)
imdb_movies$Short <- as.factor(imdb_movies$Short)
imdb_movies$Sport <- as.factor(imdb_movies$Sport)
imdb_movies$Thriller <- as.factor(imdb_movies$Thriller)
imdb_movies$War <- as.factor(imdb_movies$War)
imdb_movies$Western <- as.factor(imdb_movies$Western)

# Filtering for unique released movies
released_movies <- imdb_movies %>% 
  filter(Date < '2022') %>%
  drop_na() %>%
  unique()

# Viewing the data after cleaning
View(released_movies)

# Creating summary statistics for movies
summary(released_movies)

# Creating box plots for categories
released_movies %>% 
  ggplot(aes(x = Certificate, y = Rate, col = Certificate)) + geom_boxplot()

 released_movies %>% 
  ggplot(aes(x = Certificate, y = log(Votes), col = Certificate)) + geom_boxplot()

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
  select(c(Date,Rate,Votes,Duration, Music)) %>% 
  drop_na() %>%
  cor()


