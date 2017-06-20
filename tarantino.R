# ---------------------------------------------------------------------------------------------------------------------
#' Tarantino Movie Training Model
#' 
#' This is a data set and machine learning regression model on the curse words and deaths
#' in some of Tarantino's featured films
#' 
#' Sources:
#' https://github.com/fivethirtyeight/data/tree/master/tarantino
#' https://www.rottentomatoes.com/celebrity/quentin_tarantino
# ---------------------------------------------------------------------------------------------------------------------

library(rvest)
library(caret)
library(dplyr)
library(ggplot2)

tarantino <- read.csv("/users/alexnakagawa/iXperience/tarantino/tarantino.csv")

# View(tarantino)
# head(tarantino)

# Count how many entries there are for each movie, add movie rating for each one
movies <- count(tarantino, movie)
names(movies)[2] <- "total"
movies$RTrating <- c(88, 89, 87, 85, 84, 94, 90)     # Rotten Tomatoes rating
movies$budget <- c(100, 70, 12, 30, 30, 8.5, 1.2)    # The movie's budget, given in millions USD
movies$boxoffice <- c(425.4, 321.5, 74.7, 180.9, 152.2, 213.9, 2.8)  # Box office, given in millions USD

# View(movies)
# head(movies)

# Separate by number of entries in each category
words <- filter(tarantino, type == 'word')
deaths <- filter(tarantino, type == 'death')
deaths <- deaths[-3]

# Find the total number of deaths and words in each movie
words_total <- group_by(words, movie) %>% summarise(n())
names(words_total)[2] <- "words_per_movie"
deaths_total <- group_by(deaths, movie) %>% summarise(n())
names(deaths_total)[2] <- "deaths_per_movie"

# Combine
movies <- left_join(movies, words_total, by = 'movie')
movies <- left_join(movies, deaths_total, by = 'movie')

# Add new ratio for the number of deaths to curse wordsa
movies$death_word_ratio <- deaths_total$deaths_per_movie / words_total$words_per_movie
new_data <- movies[order(movies$death_word_ratio), ]

# Plot initial results

ggplot(data = words, aes(x = minutes_in)) + theme_minimal() + geom_dotplot(binwidth = 1.0) +
  ggtitle("Distribution of Curse Words for Each Tarantino Film") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(movie ~ ., scales = "free_x") + ylab("Frequency")

ggplot(deaths, aes(x = minutes_in)) + theme_minimal() + geom_dotplot(binwidth = 1.0) +
  ggtitle("Distribution of Deaths for Each Tarantino Film") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  facet_grid(movie ~ ., scales = "free_x") + ylab("Frequency")

ggplot(data = words, aes(x = minutes_in)) + theme_minimal() +
  geom_dotplot(binwidth = 1.0, color = "black") +
  ggtitle("Distribution of Total Curse Words and Deaths in Each Tarantino Film") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(movie ~ ., scales = "free_x") + ylab("Frequency") +
  geom_dotplot(data = deaths, binwidth = 1.0, colour = "red")

# WRONG
ggplot(data = movies, aes(x = budget, y = RTrating)) + geom_point() + theme_minimal() +
  geom_text(aes(label = movie), vjust = 1.6 , hjust = "inward", size = 3)
ggplot(data = movies, aes(x = RTrating, y = words_per_movie)) + geom_point() + theme_minimal() +
  geom_text(aes(label = movie), vjust = -1.0, hjust = "inward", size = 3)

# Use this for model
ggplot(data = movies , aes(x = budget, y = boxoffice)) + geom_point() + theme_minimal() +
  geom_text(aes(label = movie), vjust = 1.6 , hjust = "inward", size = 3)

# Figure out how to scale the x axis freely.

# Train!

fit <- lm(budget ~ boxoffice, movies)
summary(fit)

train(budget ~ boxoffice, data = movies, method = "lm")



# HOORAY!
