#--------------------------------------------------------------------------------
## @Title: script_movie_project.R
## @Author: Julian Ibarguen
## @Date created: 06.09.2020
## -----------------------------------------------------------------------------

if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if (!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if (!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")

############################# CREATE DATA SET ##################################

# Credit: Data Science: Capstone - HarvardX: PH125.9x
# https://courses.edx.org/courses/course-v1:HarvardX+PH125.9x+2T2020/courseware/dd9a048b16ca477a8f0aaf1d888f0734/e8800e37aa444297a3a2f35bf84ce452/?child=last

# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- data.table::fread(
  text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
  col.names = c("userId", "movieId", "rating", "timestamp")
)

movies <- stringr::str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>%
  dplyr::mutate(
    movieId = as.numeric(movieId), title = as.character(title),
    genres = as.character(genres)
  )

movielens <- dplyr::left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- caret::createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index, ]
temp <- movielens[test_index, ]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
  dplyr::semi_join(edx, by = "movieId") %>%
  dplyr::semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- dplyr::anti_join(temp, validation)
edx <- rbind(edx, removed)

# Get basic describign figures on raw data
raw.nrow <- nrow(movielens)
raw.movies <- length(unique(movielens$movieId))
raw.users <- length(unique(movielens$userId))

#clean as you go
rm(dl, ratings, movies, test_index, temp, removed, movielens)


#################### PRE-PROCESING: DATA EXPLORATION ###########################

# basic summary of overall rating
summ.over.rating <- summary(edx$rating)

##### EXPLORE MOVIE EFFECT 

# Explore Rating VS Movie 

movie_rating <- edx[ , c("movieId", "rating")] %>%
  dplyr::group_by(movieId) %>%
  dplyr::summarise(avg.movie = mean(rating), .groups = "drop")

summary(movie_rating$avg.movie)

#Figure 1a: "Average Rating per Movie (density)
plot_movie_avg <- ggplot2::ggplot(movie_rating, ggplot2::aes(avg.movie)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 100, fill = "gray") +
  ggplot2::geom_density(alpha = 0.2, fill = "red3") +
  ggplot2::geom_vline(
    xintercept =  mean(movie_rating$avg.movie), linetype = "dashed", color = "red"
  ) +
  ggplot2::geom_vline(
    xintercept =  median(movie_rating$avg.movie), linetype = "dashed", color = "blue"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(0, max(movie_rating$avg.movie), 0.5)) +
  ggplot2::labs(x = "Avg. rating" ) +
  ggplot2::theme_bw()

## Clean as you go
rm(movie_rating)

##### EXPLORE USER EFFECT 

## Explore Rating VS User

user_rating <- edx[ , c("userId", "rating")] %>%
  dplyr::group_by(userId) %>%
  dplyr::summarise(avg.user = mean(rating), .groups = "drop")

summ_user_rating<- summary(user_rating$avg.user)

#Figure 1b: Average Rating per User (density)
plot_user_avg <- ggplot2::ggplot(user_rating, ggplot2::aes(avg.user)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 100, fill = "gray") +
  ggplot2::geom_density(alpha = 0.2, fill = "red3") +
  ggplot2::geom_vline(
    xintercept =  mean(user_rating$avg.user), linetype = "dashed", color = "red"
  ) +
  ggplot2::geom_vline(
    xintercept =  median(user_rating$avg.user), linetype = "dashed", color = "blue"
  ) +
  ggplot2::scale_x_continuous(breaks = seq(0, max(user_rating$avg.user), 0.5)) +
  ggplot2::labs(x = "Avg. rating",
    caption = "Red dashed line represent the average; blue dashed line shows the median"
  ) +
  ggplot2::theme_bw()  

rm(user_rating)

##### EXPLORE GENRE EFFECT 

## Explore Rating VS Genre

# Separate genres iunto binary 1/0 to explore independent effect 
genre_rating <- edx[, c("movieId", "genres", "rating")] %>%
  dplyr::mutate(
    Action = dplyr::if_else(grepl("Action", genres), "included", "excluded"),
    Adventure = dplyr::if_else(grepl("Adventure", genres), "included", "excluded"),
    Comedy = dplyr::if_else(grepl("Comedy", genres), "included", "excluded"),
    Drama = dplyr::if_else(grepl("Drama", genres), "included", "excluded"),
    Fantasy = dplyr::if_else(grepl("Fantasy", genres), "included", "excluded"),
    Horror = dplyr::if_else(grepl("Horror", genres), "included", "excluded"),
    Sci.Fi = dplyr::if_else(grepl("Sci-Fi", genres), "included", "excluded"),
    Thriller = dplyr::if_else(grepl("Thriller", genres), "included", "excluded")
  ) %>%
  dplyr::select(-genres) %>%
  tidyr::pivot_longer(-c(rating, movieId), names_to = "genres") %>%
  dplyr::group_by(movieId,genres) %>%
  dplyr::mutate(n.rates = dplyr::n())

# Plot average rating by genre
genre_avg <- genre_rating %>%
  dplyr::group_by(genres, value) %>%
  dplyr::summarise(avg.genre = mean(rating), sd.genre = sd(rating))

# Figure 2: Average rating by movie classified in a genre or not (included/excluded)
plot.genre_avg <- genre_avg %>%
  ggplot2::ggplot(ggplot2::aes(value, avg.genre, color = value)) +
  ggplot2::geom_point(size = 3, show.legend = FALSE) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = avg.genre - sd.genre, ymax = avg.genre + sd.genre),
    show.legend = FALSE
  ) +
  ggplot2::geom_hline(
    yintercept = mean(edx$rating), linetype = "dashed", color = "red"
  ) +
  ggplot2::labs(
    x = "Movie classfification in genre", y = "Avg. rating",
    caption = "Red dashed line represent the overall average") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5)) +
  ggplot2::facet_grid(. ~ genres) 


##### EXPLORE TIME (day) EFFECT 

# Convert timestamp to date - round by day (replicate with validation for final test)
edx <- dplyr::mutate(
  edx, timestamp = lubridate::round_date(lubridate::as_datetime(timestamp), "day")
)
validation <- dplyr::mutate(
  validation, timestamp = lubridate::round_date(lubridate::as_datetime(timestamp), "day")
)

## Explore Rating VS Time (day)

time_rating <- edx %>%
  dplyr::group_by(timestamp) %>%
  dplyr::summarise(avg.time = mean(rating), .groups = "drop")

# Figure 3: Average rating by number of rates given per day (1994 - 2009)
plot.time_avg <- time_rating %>%
  dplyr::filter(timestamp > "1995-01-10 00:00:00") %>% # 1995 only has 2 films
ggplot2::ggplot(ggplot2::aes(timestamp, avg.time)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "loess", span = 0.1) +
  ggplot2::geom_hline(
    yintercept = summ.over.rating[["Mean"]], linetype = "dashed", color = "red"
  ) +
  ggplot2::scale_x_datetime(date_breaks = "6 month") +
  ggplot2::labs(
    x = "Date", y = "Avg. rating",
    caption = "Red dashed line represent the average; blue line the smoothed trend"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))


avg_1994 <- time_rating %>% dplyr::filter(timestamp < "1996-04-01 00:00:00" ) %>%
  dplyr::summarise(mean = mean(avg.time)) %>% dplyr::pull()


## Clean as you go
rm(time_rating)

##### EXPLORE THE NUMBER OF RATES

## Explore Rating VS Number of Ratings per movie

movie_n.rate <- edx %>%
  dplyr::add_count(movieId, name = "n.rates") %>%
  dplyr::group_by(n.rates) %>%
  dplyr::summarise(avg.n.rates = mean(rating))

summary(movie_n.rate$n.rates)

fit_nrate_movie <- lm(avg.n.rates ~ n.rates, movie_n.rate )
summary(fit_nrate_movie)

# Figure 4a: Average rating by Number of rates given per movie
plot_nummov <- ggplot2::ggplot(movie_n.rate, ggplot2::aes(n.rates, avg.n.rates)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "loess", span = 0.5) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(movie_n.rate$n.rates), 2000), trans = "sqrt"
  ) +
  ggplot2::labs(
    x = "Number of rates given (sqrt)", y = "Average rating"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))

# Clean as you go
rm(movie_n.rate, fit_nrate_movie)

## Explore Rating VS Number of Ratings per user

user_n.rate <- edx %>%
  dplyr::add_count(userId, name = "n.rates") %>%
  dplyr::group_by(n.rates) %>%
  dplyr::summarise(avg.n.rates = mean(rating))

summary(user_n.rate$n.rates)

fit_nrate_user <- lm(avg.n.rates ~ n.rates, user_n.rate )
summary(fit_nrate_user)

# Figure 4b: Average rating by Number of rates given per user
plot_numuse <- ggplot2::ggplot(user_n.rate, ggplot2::aes(n.rates, avg.n.rates)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "loess", span = 0.5) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(user_n.rate$n.rates), 500), trans = "sqrt"
  ) +
  ggplot2::labs(
    x = "Number of rates given (sqrt)", y = "Average rating",
    caption = "Applied square root transformation of 'Number of rates given' to linearise the asociation"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))

# Clean as you go
rm(user_n.rate, fit_nrate_user)


## Explore Rating VS Number of Ratings per day

time_n.rate <- edx %>%
  dplyr::count(timestamp, name = "n.rates")

summary(time_n.rate$n.rates)

#Figure 4c: Average rating by Number of rates given per day
plot_numtime <- time_n.rate %>%
  dplyr::filter(timestamp > "1995-01-10 00:00:00") %>% # 1995 only has 2 films
  ggplot2::ggplot(ggplot2::aes(timestamp, n.rates)) + 
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "loess", span = 0.1) +
  ggplot2::scale_x_datetime(date_breaks = "6 months") +
  ggplot2::labs(x = "Date", y = "Number of rates") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Clean as you go
rm(time_n.rate)

## Explore Rating VS Number of Ratings per genre

genre_nrate <- genre_rating %>%
  dplyr::group_by(genres, value) %>%
  dplyr::summarise(n.rates = n.rates[1])

# Plot number of rates per gender
plot_genre_nrate <- ggplot2::ggplot(genre_nrate, ggplot2::aes(genres, n.rates, color = value)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_segment(
    data = tidyr::pivot_wider(genre_nrate, names_from = value, values_from = n.rates), 
    ggplot2::aes(x = genres, xend = genres, y = excluded, yend = included),
    color = "gray"
  ) +
  ggplot2::scale_y_continuous(breaks = seq(0, max(genre_nrate$n.rates), 5000)) +
  ggplot2::labs(x = "Genres", y = "Number of rates given") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.title = ggplot2::element_text("Inclusion/Exclusion of genre in a Movie"),
    legend.position = "bottom"
  )

# Clean as you go
rm(genre_nrate, genre_rating)


######################## PRE-PROCESING: DATA SPLIT #############################
# Crete resample x5 the training data (edx) and part it in training and probe sets  

#### BOOTSRAP: TRAINING & PROBE

## Bootstrap x5 for cross-validation
set.seed(28032020)
bootstrap <- caret::createResample(edx$rating, times = 5, list = TRUE)

## Partition 15% probe
set.seed(28032020)
probe_i <- lapply(
  1:length(bootstrap), function(x) 
  caret::createDataPartition(bootstrap[[x]], p = 0.15, list = TRUE)
)

## Subset partitions from edx and format "Timestamp"
probe_boots <- lapply(1:length(probe_i) , function(i) edx[probe_i[[i]][[1]], ])
train_boots <- lapply(1:length(probe_i) , function(i) edx[-probe_i[[i]][[1]], ])

## match probe to train to remove NA in summaries 
probe_boots <- purrr::map2(probe_boots, train_boots, ~ {
  .x %>%
    dplyr::semi_join(.y, by = "movieId") %>%
    dplyr::semi_join(.y, by = "userId") %>%
    dplyr::semi_join(.y, by = "timestamp")
})

# Overall average rating for each boot
mus <- lapply(
  1:length(train_boots), function(i) mean(train_boots[[i]][["rating"]])
)

# Compile all in list
data_set <- list(train_boots, mus, probe_boots)


#### DESCRIBE DATA  

## Create function for basic description of data
DescribeMe <- function (x, data.name = NULL) {
  # Args: x = a data frame to summarise. must include relevant var.names of MovieLens; 
  #       data.name = create data (name) column. Useful when binding multple tables
  # Return: a summarized data frame
  out <- x %>%
    dplyr::add_count(userId, rating, name = "rating_user") %>%
    dplyr::add_count(movieId, rating, name = "rating_movie") %>%
    dplyr::summarise(
      `n. ratings` = dplyr::n(),
      `avg. rating` = round(mean(rating), 2),
      `n. Users` = length(unique(userId)),
      `avg. ratings/user` = round(mean(rating_user), 2),
      `n. movies` = length(unique(movieId)),
      `avg. ratings/movie` = round(mean(rating_movie), 2),
      `avg. genres/movie` = round(mean(stringr::str_count(genres, "\\|") + 1), 2),
      `perc. commedy` = round(sum(grepl("Comedy", genres)) / dplyr::n(), 2),
      `perc. drama` = round(sum(grepl("Drama", genres)) / dplyr::n(), 2)
    ) %>%
    {
      if (!is.null(data.name)) {
        dplyr::mutate(., data = data.name) %>%
          dplyr::relocate(data)
      } else .
    }
  return(out)
}

# Table 1: Basic description of data sets
data_describe <- dplyr::bind_rows(
  DescribeMe(validation) %>% dplyr::mutate(data = "Raw-Hold-out"),
  DescribeMe(edx) %>% dplyr::mutate(data = "Raw-Training"),
  dplyr::bind_rows(lapply(train_boots, DescribeMe)) %>%
    dplyr::mutate(data = "Boots-Training"),
  dplyr::bind_rows(lapply(probe_boots, DescribeMe)) %>%
    dplyr::mutate(data = "Boots-Probe"),
) %>%
  dplyr::relocate(data)

## Clean as you go
rm(bootstrap, probe_i, probe_boots, train_boots)



############################# MODELLING: PREDICTORS ############################
# Create predictors (x5) and tune lambda for regularization


##### PREDICTOR 0: AVERAGE RATING (baseline)

rmses_baseline <- purrr::pmap(data_set, ~ {
  rmses_hat <- caret::RMSE(..3$rating, ..2)
  print(rmses_hat)
})
all_rmses["baseline"] <- mean(unlist(rmses_baseline)) 


##### PREDICTOR 1: MOVIE EFFECT

### Get RMSE with movie effect - no regularization

# helper function for Movie effect
MovieEffect <- function(data_set, lambdas) {
  # Args: data_set = a list of three items containing the training data_set, 
  #   the mus for each training data, and the probe data_set
  # lambdas = sequence of lambdas to tune for regularization
  # Return: a scales with he average mean of RMSEs
  rmses_movie <- purrr::pmap(data_set, ~ {
  
      print(Sys.time()) # print for time monitoring
      
      movie_effect <- ..1 %>% 
        dplyr::group_by(movieId) %>%
        dplyr::summarise(
          reg.avg.mov = sum(rating - ..2) / (dplyr::n() + lambdas), .groups = "drop"
        )
    
      prediction <- ..3 %>%
        dplyr::left_join(movie_effect, by = "movieId") %>%
        dplyr::mutate(prediction = ..2 + reg.avg.mov) %>%
        dplyr::pull(prediction)
      
      rmses_hat <- caret::RMSE(..3$rating, prediction)
      print(rmses_hat) # print for outcome monitoring

    })
  return(mean(unlist(rmses_movie)))
}

rmse_movie <- MovieEffect(data_set, 0)

### Movie Effect Regularization

lambdas <- seq(0, 5, 0.25) # Explored: lambdas <- seq(0, 10, 0.5); best was 2, Now tailor at 0.25
rmses_movie_reg <- lapply(lambdas, function(l) MovieEffect(data_set, l))

# Plot lambdas against rmses
ggplot2::qplot(lambdas, unlist(rmses_movie_reg)) +
  ggplot2::labs(x = "Lambdas", y = "RMSE") +
  ggplot2::scale_x_continuous(breaks = lambdas) +
  ggplot2::theme_bw() 

# Collect final regularized RMSE
all_rmses <- c()
lambda_movie <- lambdas[which.min(unlist(rmses_movie_reg))]
all_rmses["movie.effect"] <- MovieEffect(data_set, lambda_movie)

## Clean as you go
rm(lambdas, rmses_movie_reg, MovieEffect, movie_effect)


##### PREDICTOR 2: USER EFFECT

### Get RMSE with movie effect - no regularization

# helper function for User effect
UserEffect <- function (data_set, lambda.1 = 0, lambda.2 = 0) {

  rmses_hat <- purrr::pmap(data_set, ~ {
    
    print(Sys.time()) # print for time monitoring
    
    movie_effect <- ..1 %>% 
      dplyr::group_by(movieId) %>%
      dplyr::summarise(
        reg.avg.mov = sum(rating - ..2) / (dplyr::n() + lambda.1), .groups = "drop"
      )
    
    user_effect <- ..1 %>% 
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::group_by(userId) %>%
      dplyr::summarise(
        reg.avg.user = sum(rating - ..2 - reg.avg.mov) / (dplyr::n() + lambda.2),
        .groups = "drop"
      )
    
    prediction <- ..3 %>%
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::mutate(prediction = ..2 + reg.avg.mov + reg.avg.user) %>%
      dplyr::pull(prediction)
    
    rmses_hat <- caret::RMSE(..3$rating, prediction)
    print(rmses_hat) # print for outcome monitoring
  
  })
  return(mean(unlist(rmses_hat)))
}

rmse_user <- UserEffect(data_set, lambda_movie, 0)


### User Effect Regularization

lambdas <- seq(2, 8, 0.25) # Explored: lambdas <- seq(0, 10, 1); best was 5. Now tailor at 0.25
rmses_user_reg <- lapply(
  lambdas, function(l) UserEffect(data_set, lambda_movie, l)
)
# Plot lambdas against rmses
ggplot2::qplot(lambdas, unlist(rmses_user_reg)) +
  ggplot2::labs(x = "Lambdas", y = "RMSE") +
  ggplot2::scale_x_continuous(breaks = lambdas) +
  ggplot2::theme_bw() 

lambda_user <- lambdas[which.min(unlist(rmses_user_reg))]
all_rmses["user.effect"] <- UserEffect(data_set, lambda_movie, lambda_user)

## Clean as you go
rm(lambdas, rmses_user_reg, UserEffect, user_effect)


##### PREDICTOR 3: GENRE EFFECT

### Get RMSE with genre effect - no regularization

# helper function for Genre effect
GenreEffect <- function (data_set, lambda.1 = 0, lambda.2 = 0, lambda.3 = 0) {
  
  rmses_hat <- purrr::pmap(data_set, ~ {
    
    print(Sys.time()) # print for time monitoring
    
    movie_effect <- ..1 %>% 
      dplyr::group_by(movieId) %>%
      dplyr::summarise(
        reg.avg.mov = sum(rating - ..2) / (dplyr::n() + lambda.1), .groups = "drop"
      )
    
    user_effect <- ..1 %>% 
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::group_by(userId) %>%
      dplyr::summarise(
        reg.avg.user = sum(rating - ..2 - reg.avg.mov) / (dplyr::n() + lambda.2),
        .groups = "drop"
      )
    
    genre_effect <- ..1 %>% 
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::group_by(genres) %>%
      dplyr::summarise(
        reg.avg.genre = sum(rating - ..2 - reg.avg.mov - reg.avg.user) / 
          (dplyr::n() + lambda.3),
        .groups = "drop"
      )
    
    prediction <- ..3 %>%
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::left_join(genre_effect, by = "genres") %>%
      dplyr::mutate(
        prediction = ..2 + reg.avg.mov + reg.avg.user + reg.avg.genre
      ) %>%
      dplyr::pull(prediction)
    
    rmses_hat <- caret::RMSE(..3$rating, prediction)
    print(rmses_hat) # print for outcome monitoring
    
  })
  return(mean(unlist(rmses_hat)))
}

rmse_genre <- GenreEffect(data_set, lambda_movie, lambda_user, 0) 

### Genre Effect Regularization

lambdas <- seq(10, 15, 0.5) # Explored: lambdas <- seq(5, 20, 1); best was 13, Now tailor at 0.5
rmses_genre_reg <- lapply(
  lambdas, function(l) GenreEffect(data_set, lambda_movie, lambda_user, l)
)
# Plot lambdas against rmses
ggplot2::qplot(lambdas, unlist(rmses_genre_reg)) +
  ggplot2::labs(x = "Lambdas", y = "RMSE") +
  ggplot2::scale_x_continuous(breaks = lambdas) +
  ggplot2::theme_bw() 

lambda_genre <- lambdas[which.min(unlist(rmses_genre_reg))]
all_rmses["genre.effect"] <- GenreEffect(data_set, lambda_movie, lambda_user, lambda_genre) 

## Clean as you go
rm(lambdas, rmses_genre_reg, GenreEffect)


##### PREDICTOR 4: TIME (DAY)

### Get RMSE with Time (day) effect - no regularization

# helper function for Time effect
TimeEffect <- function (data_set, lambda.1 = 0, lambda.2 = 0, lambda.3 = 0,
                         lambda.4 = 0) {
  
  rmses_hat <- purrr::pmap(data_set, ~ {
    
    print(Sys.time()) # print for time monitoring
    
    movie_effect <- ..1 %>% 
      dplyr::group_by(movieId) %>%
      dplyr::summarise(
        reg.avg.mov = sum(rating - ..2) / (dplyr::n() + lambda.1), .groups = "drop"
      )
    
    user_effect <- ..1 %>% 
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::group_by(userId) %>%
      dplyr::summarise(
        reg.avg.user = sum(rating - ..2 - reg.avg.mov) / (dplyr::n() + lambda.2),
        .groups = "drop"
      )
    
    genre_effect <- ..1 %>% 
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::group_by(genres) %>%
      dplyr::summarise(
        reg.avg.genre = sum(rating - ..2 - reg.avg.mov - reg.avg.user) / 
          (dplyr::n() + lambda.3),
        .groups = "drop"
      )
    
    time_effect <- ..1 %>%  #edx
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::left_join(genre_effect, by = "genres") %>%
      dplyr::group_by(timestamp) %>%
      dplyr::summarise(
        reg.avg.time = sum(rating - ..2 - reg.avg.mov - reg.avg.user - reg.avg.genre) /
          (dplyr::n() + lambda.4),
        .groups = "drop"
      )
    
    prediction <- ..3 %>%
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::left_join(genre_effect, by = "genres") %>%
      dplyr::left_join(time_effect, by = "timestamp") %>%
      dplyr::mutate(
        prediction = ..2 + reg.avg.mov + reg.avg.user + reg.avg.genre + reg.avg.time
      ) %>%
      dplyr::pull(prediction)
    
    rmses_hat <- caret::RMSE(..3$rating, prediction)
    print(rmses_hat) # print for outcome monitoring
    
  })
  return(mean(unlist(rmses_hat)))
}

rmse_time <- TimeEffect(data_set, lambda_movie, lambda_user, lambda_genre, 0)

### Time (day) Effect Regularization

lambdas <- seq(80, 90, 0.5) # Explored: lambdas <- seq(0, 20, 2); best was 20. Re-explored seq(22, 32, 2); best was 32. Re-explored seq(40, 100, 10) best was 90. Now tailor at 0.5 
rmses_time_reg <- lapply(lambdas, function(l) 
  TimeEffect(data_set, lambda_movie, lambda_user, lambda_genre, l)
)
# Plot lambdas against rmses
ggplot2::qplot(lambdas, unlist(rmses_time_reg)) +
  ggplot2::labs(x = "Lambdas", y = "RMSE") +
  ggplot2::scale_x_continuous(breaks = lambdas) +
  ggplot2::theme_bw() 

lambda_time <- lambdas[which.min(unlist(rmses_time_reg))]
all_rmses["time.effect"] <- TimeEffect(
  data_set, lambda_movie, lambda_user, lambda_genre, lambda_time
)

## Clean as you go
rm(lambdas, rmses_time_reg, TimeEffect)


##### PREDICTOR 5: NUMBER OF RATES PER MOVIE

### Get RMSE with number of rates per movie - no regularization

# helper function for N rates per movie effect
NumMovEffect <- function (data_set, lambda.1 = 0, lambda.2 = 0, lambda.3 = 0,
                        lambda.4 = 0, lambda.5 = 0) {
  
  rmses_hat <- purrr::pmap(data_set, ~ {
    
    print(Sys.time()) # print for time monitoring
    
    movie_effect <- ..1 %>% 
      dplyr::group_by(movieId) %>%
      dplyr::summarise(
        reg.avg.mov = sum(rating - ..2) / (dplyr::n() + lambda.1), .groups = "drop"
      )
    
    user_effect <- ..1 %>% 
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::group_by(userId) %>%
      dplyr::summarise(
        reg.avg.user = sum(rating - ..2 - reg.avg.mov) / (dplyr::n() + lambda.2),
        .groups = "drop"
      )
    
    genre_effect <- ..1 %>% 
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::group_by(genres) %>%
      dplyr::summarise(
        reg.avg.genre = sum(rating - ..2 - reg.avg.mov - reg.avg.user) / 
          (dplyr::n() + lambda.3),
        .groups = "drop"
      )
    
    time_effect <- ..1 %>%  #edx
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::left_join(genre_effect, by = "genres") %>%
      dplyr::group_by(timestamp) %>%
      dplyr::summarise(
        reg.avg.time = sum(rating - ..2 - reg.avg.mov - reg.avg.user - reg.avg.genre) /
          (dplyr::n() + lambda.4),
        .groups = "drop"
      )
    
    nmovie_effect <- ..1 %>%
      dplyr::add_count(movieId, name = "n.rates") %>%
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::left_join(genre_effect, by = "genres") %>%
      dplyr::left_join(time_effect, by = "timestamp") %>%
      dplyr::group_by(movieId, n.rates) %>%
      dplyr::summarise(
        reg.avg.nmov = sum(
          rating - ..2 - reg.avg.mov - reg.avg.user - reg.avg.genre - reg.avg.time
        ) / (dplyr::n() + lambda.5),
        .groups = "drop"
      )
    
    prediction <- ..3 %>%
      dplyr::add_count(movieId, name = "n.rates") %>%
      dplyr::left_join(movie_effect, by = "movieId") %>%
      dplyr::left_join(user_effect, by = "userId") %>%
      dplyr::left_join(genre_effect, by = "genres") %>%
      dplyr::left_join(time_effect, by = "timestamp") %>%
      dplyr::left_join(nmovie_effect, by = "movieId") %>%
      dplyr::mutate(
        prediction = ..2 + reg.avg.mov + reg.avg.user + reg.avg.genre + reg.avg.time + reg.avg.nmov
      ) %>%
      dplyr::pull(prediction)
    
    rmses_hat <- caret::RMSE(..3$rating, prediction)
    print(rmses_hat) # print for outcome monitoring
    
  })
  return(mean(unlist(rmses_hat)))
}

rmse_nummov <- NumMovEffect(data_set, lambda_movie, lambda_user, lambda_genre, lambda_time, 0)

### Number of Rates per Movie Effect Regularization

lambdas <- seq(5, 10, 0.5) # Explored: lambdas <- seq(0, 100, 15); best was 15; re-explored seq(10, 20, 1), best was 10; Now tailor at 0.5
rmses_nummov_reg <- lapply(lambdas, function(l) 
  NumMovEffect(data_set, lambda_movie, lambda_user, lambda_genre, lambda_time, l)
)
# Plot lambdas against rmses
ggplot2::qplot(lambdas, unlist(rmses_nummov_reg )) +
  ggplot2::labs(x = "Lambdas", y = "RMSE") +
  ggplot2::scale_x_continuous(breaks = lambdas) +
  ggplot2::theme_bw() 

lambda_nummov <- lambdas[which.min(unlist(rmses_nummov_reg))]
all_rmses["nummov"] <- NumMovEffect(
  data_set, lambda_movie, lambda_user, lambda_genre, lambda_time, lambda_nummov
)

## Clean as you go
rm(lambdas, rmses_nummov_reg, NumMovEffect)


############################# MODELING: FINAL MODEL TEST #######################

####### CAPITALIZE TRAINING

# Summary table with achievements
model_summary <- data.frame(
  predictors = factor(
    names(all_rmses), 
    levels = c("baseline", "movie.effect", "user.effect", "genre.effect", "time.effect", "nummov")
  ), 
  rmse = all_rmses
) %>% 
  dplyr::arrange(predictors) %>%
  dplyr::mutate(
    relative.improve = tidyr::replace_na(rmse - lag(rmse), 0),
    cumm.improve = cumsum(relative.improve),
    predictors = case_when(
      predictors == "baseline" ~ "Baseline",
      predictors == "movie.effect" ~ "Movie effect",
      predictors == "user.effect" ~ "User effect",
      predictors == "genre.effect" ~ "Genre effect",
      predictors == "time.effect" ~ "Time effect",
      predictors == "nummov" ~ "Rates / Movie",
    )
  )

####### FINAL TEST MODEL WITH HOLD-OUT

# Predictor 1
movie_effect <- edx %>% 
  dplyr::group_by(movieId) %>%
  dplyr::summarise(
    reg.avg.mov = sum(rating - summ.over.rating[["Mean"]]) / (dplyr::n() + lambda_movie), .groups = "drop"
  )
# Predictor 2
user_effect <- edx %>% 
  dplyr::left_join(movie_effect, by = "movieId") %>%
  dplyr::group_by(userId) %>%
  dplyr::summarise(
    reg.avg.user = sum(rating - summ.over.rating[["Mean"]] - reg.avg.mov) / (dplyr::n() + lambda_user),
    .groups = "drop"
  )
# Predictor 3
genre_effect <- edx %>% 
  dplyr::left_join(movie_effect, by = "movieId") %>%
  dplyr::left_join(user_effect, by = "userId") %>%
  dplyr::group_by(genres) %>%
  dplyr::summarise(
    reg.avg.genre = sum(rating - summ.over.rating[["Mean"]] - reg.avg.mov - reg.avg.user) / 
      (dplyr::n() + lambda_genre),
    .groups = "drop"
  )
# Predictor 4
time_effect <- edx %>% 
  dplyr::left_join(movie_effect, by = "movieId") %>%
  dplyr::left_join(user_effect, by = "userId") %>%
  dplyr::left_join(genre_effect, by = "genres") %>%
  dplyr::group_by(timestamp) %>%
  dplyr::summarise(
    reg.avg.time = sum(rating - summ.over.rating[["Mean"]] - reg.avg.mov - reg.avg.user - reg.avg.genre) / 
      (dplyr::n() + lambda_time),
    .groups = "drop"
  )
# Predictor 5
nmovie_effect <- edx %>%
  dplyr::add_count(movieId, name = "n.rates") %>%
  dplyr::left_join(movie_effect, by = "movieId") %>%
  dplyr::left_join(user_effect, by = "userId") %>%
  dplyr::left_join(genre_effect, by = "genres") %>%
  dplyr::left_join(time_effect, by = "timestamp") %>%
  dplyr::group_by(movieId, n.rates) %>%
  dplyr::summarise(
    reg.avg.nmov = sum(
      rating - summ.over.rating[["Mean"]] - reg.avg.mov - reg.avg.user - reg.avg.genre - reg.avg.time
    ) / (dplyr::n() + lambda_nummov),
    .groups = "drop"
  )

# Get overall avg for validation data
mu_holdout <- mean(validation$rating)

# Test
prediction <- validation %>%
  dplyr::add_count(movieId, name = "n.rates") %>%
  dplyr::left_join(movie_effect, by = "movieId") %>%
  dplyr::left_join(user_effect, by = "userId") %>%
  dplyr::left_join(genre_effect, by = "genres") %>%
  dplyr::left_join(time_effect, by = "timestamp") %>%
  dplyr::left_join(nmovie_effect, by = "movieId") %>%
  dplyr::mutate(
    prediction = mu_holdout + reg.avg.mov + reg.avg.user + reg.avg.genre + reg.avg.time + reg.avg.nmov
  ) %>%
  dplyr::pull(prediction)

rmse_final <- caret::RMSE(validation$rating, prediction)

################################## REPORT RMD ##################################

rmarkdown::render(
  "C:/Users/jibarguen/Desktop/Personal/EdX Data Science/9.Capstone - thesis/MovieLens Report.Rmd",
)

