if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(dslabs)) install.packages('dslabs')
if (!require(lubridate)) install.packages('lubridate')
if (!require(pdftools)) install.packages('pdftools')
if (!require(rpart)) install.packages('rpart')
if (!require(matrixStats)) install.packages('matrixStats')
if (!require(knitr)) install.packages('knitr')
if (!require(ranger)) install.packages('ranger')
if (!require(rsample)) install.packages('rsample')


library(caret)
library(dslabs)
library(lubridate)
library(tidyverse)
library(pdftools)
library(rpart)
library(matrixStats)
library(knitr)
library(data.table)
library(rsample)
library(ranger)


#################################
# FUNCTIONS 
#################################

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}





################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

## making MovieID as an integer
movielens$movieId <- as.integer(movielens$movieId)

set.seed(1, sample.kind= "Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]  ##  train set
temp <- movielens[test_index,]  ##  test/validation set initial


### original training and validation sets
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)



###  looking for lack of variability in rating  ###

n_ratings <- edx %>% dplyr::group_by(userId) %>% summarize(n =n(), wnRater_rating_avg = mean(rating), wnRater_ratingsd = sd(rating))
n_ratings

n_ratings %>% filter(wnRater_ratingsd == 0)
edx_1 <- edx %>% left_join(n_ratings, by = "userId") 

## identifying raters that rate every movie the same, and filtering out

edx_1 <- edx_1 %>%  filter(wnRater_ratingsd != 0)  



## creating train and validation files for training purposes
set.seed(1, sample.kind= "Rounding")
edx_2 <- edx_1
test_index <- createDataPartition(y = edx_2$rating, times = 1, p = 0.1, list = FALSE)

train <- edx_2[-test_index,]  ##  train set
temp_val_train <- edx_2[test_index,]  ##  test/validation set initial

# Make sure userId and movieId in validation set are also in edx set
# creating FINAL validation dataset ##

validation_train <- temp_val_train %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp_val_train, validation_train)
train <- rbind(train, removed)


# adding a date column: converting timestamp to date/time structure
train <- mutate(train, date = as_datetime(timestamp)) 

validation_train <- mutate(validation_train, date = as_datetime(timestamp)) 

### removing unnecessary files from environment ###

rm(dl, ratings, movies, test_index, temp, movielens, removed, temp_val_train)

## saving the initial edx, validation, and training /validation train files as rds so only have to create the first time ###
saveRDS(edx, "Data/edx.rds")
saveRDS(validation, "Data/validation.rds")

saveRDS(train, "Data/train.rds")
saveRDS(validation_train, "Data/validation_train.rds")

## START HERE AFTER INITIAL RUN TO LOAD THE edx and validation files ###
edx <- readRDS("Data/edx.rds")
validation <- readRDS("Data/validation.rds")


train <- readRDS("Data/train.rds")
validation_train <- readRDS("Data/validation_train.rds")

###########################################
####  DESCRIPTIVES, DATA CLEANING, ETC. ###
###########################################


#edx$userId <- as.character(edx$userId)



### DESCRIPTIVES ######

## RATINGS ##

table(train$rating)  ## frequency of ratings

mu <- mean(train$rating)
mu

sd <- sd(train$rating)
sd

min <- min(train$rating)
max <- max(train$rating)
n <- nrow(train)

ratingdescript <- data_frame(. = "Ratings", N =n, Mean = mu, sd = sd, Min = min, Max = max)

saveRDS(ratingdescript, "Results/Ratings_descriptives.rds")

## looking at effect of rating date on rating 

train %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()


## RATERS ##

length(unique(train$userId)) ### number of unique raters

#within rater summary statistics ##

n_ratings <- train %>%
  dplyr::group_by(userId) %>% 
  summarize(n =n(), 
            wnRater_rating_avg = mean(rating), 
            wnRater_ratingsd = sd(rating))

n_ratings %>% 
  filter(n_ratings$n == min(n_ratings$n))

n_ratings %>% 
  filter(n_ratings$n == max(n_ratings$n))

n_ratings %>% 
  filter(n_ratings$wnRater_ratingsd == 0)



n_ratings_min <- min(n_ratings$n)
n_ratings_max <- max(n_ratings$n)

n_ratings_median<- median(n_ratings$n)

min_wn_rater_ratingavg<- min(n_ratings$wnRater_rating_avg)
max_wn_rater_ratingavg<- max(n_ratings$wnRater_rating_avg)


raterdescript <- data_frame(. = "Raters",
                            Median_N_Ratings = n_ratings_median,
                            Min_N_Ratings = n_ratings_min, 
                            Max_N_Ratings = n_ratings_max, 
                            Within_Group_AverageRating_Min = min_wn_rater_ratingavg, 
                            Within_Group_AverageRating_Max = max_wn_rater_ratingavg)

## distro of number of ratings / rater
hist(n_ratings$n, 
     breaks = 100, 
     main = "Number of Ratings per Rater", 
     xlab = "Number of Ratings")

# rating distro: average rating within raters #
hist(n_ratings$wnRater_rating_avg, 
     main = "Average Rating Within-Raters", 
     xlab = "Average Rating") 


## MOVIES ##
n_movies <- length(unique(train$movieId)) # ## number of unique movies
n_movies

ratings_movie <- train %>% 
  dplyr::group_by(movieId) %>% 
  summarize(n =n(), 
            wnMovie_rating_avg = mean(rating), 
            wnMovie_ratingsd = sd(rating))

ratings_movie

ratings_movie %>% filter(ratings_movie$n == min(ratings_movie$n))
ratings_movie %>% filter(ratings_movie$n == max(ratings_movie$n))

wn_movie_nratings_min <- min(ratings_movie$n)
wn_movie_nratings_max <- max(ratings_movie$n)
wn_movie_nratings_median <- median(ratings_movie$n)


min_wn_movie_ratingavg<- min(ratings_movie$wnMovie_rating_avg)
max_wn_movie_ratingavg<- max(ratings_movie$wnMovie_rating_avg)


raterdescript <- bind_rows(raterdescript,
                           data_frame(. = "Movies", 
                            Median_N_Ratings = wn_movie_nratings_median, 
                            Min_N_Ratings = wn_movie_nratings_min, 
                            Max_N_Ratings = wn_movie_nratings_max, 
                            Within_Group_AverageRating_Min = min_wn_movie_ratingavg, 
                            Within_Group_AverageRating_Max = max_wn_movie_ratingavg))

## histrograms of within movie rating
hist(ratings_movie$n, breaks = 100, main="Number of Ratings per Movie", xlab = "Number of Ratings")
hist(ratings_movie$wnMovie_rating_avg, main = "Average Rating Within-Movies", xlab = "Average Rating") 

## GENRES ##
genres <- unique(train$genres) ### number of unique genres
genres

ratings_genre <- train %>% dplyr::group_by(genres) %>% summarize(n =n(), wnGenre_rating_avg = as.numeric(mean(rating)), wnGenre_ratingsd = sd(rating))
ratings_genre

ratings_genre %>% filter(ratings_genre$n == min(ratings_genre$n))
ratings_genre %>% filter(ratings_genre$n == max(ratings_genre$n))


wn_genre_nratings_min <- min(ratings_genre$n)
wn_genre_nratings_max <- max(ratings_genre$n)
wn_genre_nratings_median <- median(ratings_genre$n)


min_wn_genre_ratingavg<- min(ratings_genre$wnGenre_rating_avg)
max_wn_genre_ratingavg<- max(ratings_genre$wnGenre_rating_avg)


raterdescript <- bind_rows(raterdescript,
                           data_frame(. = "Genres", 
                                      Median_N_Ratings = wn_genre_nratings_median, 
                                      Min_N_Ratings = wn_genre_nratings_min, 
                                      Max_N_Ratings = wn_genre_nratings_max, 
                                      Within_Group_AverageRating_Min = min_wn_genre_ratingavg, 
                                      Within_Group_AverageRating_Max = max_wn_genre_ratingavg))

hist(ratings_genre$n, breaks = 100, main = "Number of Ratings per Genre", xlab = "Number of Ratings")
hist(ratings_genre$wnGenre_rating_avg, main = "Average Rating Within-Genre", xlab = "Average Rating")


saveRDS(raterdescript, "Results/WNRater_descriptives.rds")


### looking at potential trend/impact of genre
train %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##### NAIVE MODEL: starting point ####
## then compute RMSE on test set data
naive_rmse <- RMSE(validation_train$rating, mu)
naive_rmse 

## RMSE results table
rmse_results <- data_frame(Model = "Just the average", RMSE = naive_rmse)
rmse_results

##### MOVIE EFFECT: accounting for 

movie_avgs <- train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# predicting ratings incorporting b for movie effect
predicted_ratings <- mu + validation_train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, validation_train$rating)
rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model = "Movie Effect Model",
                                     RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

#### MOVIE & RATER EFFECT ###
user_avgs <- train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu- b_i))

predicted_ratings <- validation_train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, validation_train$rating)
rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model = "Movie + User Effects Model",
                                     RMSE = model_2_rmse))

rmse_results%>% knitr::kable() ### see a further improvement



#### MOVIE, RATER, & GENRE EFFECT ###
genre_avgs <- train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu- b_i - b_u))

predicted_ratings <- validation_train %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation_train$rating)

rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model = "Movie + User + Genre Effects Model",
                                     RMSE = model_3_rmse))

rmse_results%>% knitr::kable() ### see a further improvement


### final rmse using original validation

movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
user_avgs <- edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu- b_i))
genre_avgs <- edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu- b_i - b_u))


predicted_ratings_final <- validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

final_model_rmse <- RMSE(predicted_ratings_final, validation$rating)

rmse_results <- bind_rows(rmse_results, 
                          data_frame(Model = "Movie + User + Genre Effects Model- Final RMSE",
                                     RMSE = final_model_rmse))

###saving rmseresult to call in markdown file ###
saveRDS(rmse_results, "Results/rmse_results.rds")

rmse_results%>% knitr::kable() ### see a further improvement
