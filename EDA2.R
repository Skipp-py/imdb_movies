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

released_movies <- imdb[-c(1,2,6,31)]
released_movies$Date <- year(released_movies$Date)

# Filtering for unique released movies (numeric)
released_movies_numeric <- released_movies %>% 
  filter(Date < '2022') %>%
  drop_na() %>%
  unique()

# Filtering for unique released movies (factor)
released_movies_factors <- released_movies %>% 
  filter(Date < '2022') %>%
  drop_na() %>%
  unique()

# Converting datatypes
released_movies_factors$Date <- year(released_movies_factors$Date)
released_movies_factors$Certificate <- as.factor(released_movies_factors$Certificate)
released_movies_factors$Nudity <- as.factor(released_movies_factors$Nudity)
released_movies_factors$Violence <- as.factor(released_movies_factors$Violence)
released_movies_factors$Profanity <- as.factor(released_movies_factors$Profanity)
released_movies_factors$Alcohol <- as.factor(released_movies_factors$Alcohol)
released_movies_factors$Frightening <- as.factor(released_movies_factors$Frightening)
released_movies_factors$Action <- as.factor(released_movies_factors$Action)
released_movies_factors$Adventure <- as.factor(released_movies_factors$Adventure)
released_movies_factors$Animation <- as.factor(released_movies_factors$Animation)
released_movies_factors$Biography <- as.factor(released_movies_factors$Biography)
released_movies_factors$Comedy <- as.factor(released_movies_factors$Comedy)
released_movies_factors$Crime <- as.factor(released_movies_factors$Crime)
released_movies_factors$Documentary <- as.factor(released_movies_factors$Documentary)
released_movies_factors$Drama <- as.factor(released_movies_factors$Drama)
released_movies_factors$Family <- as.factor(released_movies_factors$Family)
released_movies_factors$Fantasy <- as.factor(released_movies_factors$Fantasy)
released_movies_factors$`Film-Noir` <- as.factor(released_movies_factors$`Film-Noir`)
released_movies_factors$History <- as.factor(released_movies_factors$History)
released_movies_factors$Horror <- as.factor(released_movies_factors$Horror)
released_movies_factors$Music <- as.factor(released_movies_factors$Music)
released_movies_factors$Musical <- as.factor(released_movies_factors$Musical)
released_movies_factors$Mystery <- as.factor(released_movies_factors$Mystery)
released_movies_factors$Romance <- as.factor(released_movies_factors$Romance)
released_movies_factors$`Sci-Fi` <- as.factor(released_movies_factors$`Sci-Fi`)
released_movies_factors$Short <- as.factor(released_movies_factors$Short)
released_movies_factors$Sport <- as.factor(released_movies_factors$Sport)
released_movies_factors$Thriller <- as.factor(released_movies_factors$Thriller)
released_movies_factors$War <- as.factor(released_movies_factors$War)
released_movies_factors$Western <- as.factor(released_movies_factors$Western)

summary(released_movies_factors)

released_movies_numeric2 <- released_movies %>% 
  select(-c(Certificate, Nudity, Violence, Profanity, Frightening, Alcohol)) %>% 
  drop_na() 

corrplot(cor(released_movies_numeric2))

# Null regression with 
lm.null <- lm(Rate ~ 1, data = released_movies_numeric)

lm.reduced <- lm(Rate ~ Date + Duration + Certificate, data = released_movies_numeric)

lm.AIC <- lm(formula = Rate ~ Date + Action + Adventure + Animation + 
               Biography + Documentary + Drama + Family + Fantasy + Horror + 
               Romance + `Sci-Fi` + Short + Thriller + Duration + Certificate + 
               Nudity + Violence + Frightening, data = released_movies_numeric)
lm.full <- lm(Rate~. -Votes, data = released_movies_numeric)

step(lm.full, scope=list(lower=lm.null, upper=lm.full), direction="backward", test="F")



lm.AIC <- lm(formula = Rate ~ Date + Action + Adventure + Animation + 
     Biography + Documentary + Drama + Family + Fantasy + Horror + 
     Romance + `Sci-Fi` + Short + Thriller + Duration + Certificate + 
     Nudity + Violence + Frightening, data = released_movies_numeric)

anova(lm.null, lm.reduced, lm.AIC, lm.full)

summary(lm.AIC)

summary(lm.test)

qqnorm(released_movies_numeric$Rate, pch = 1, frame = FALSE)
qqline(released_movies_numeric$Rate, col = "steelblue", lwd = 2)
plot(lm.test, which=1)

lm(Rate ~ Date + Duration + Certificate, data = released_movies_factors)










