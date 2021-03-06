
library(data.table)
library(magrittr)
library(ggplot2)

library(textstem)
library(tidytext)
data(stop_words)

library(ruimtehol)

## Functions ---------------------------

get_classes <- Vectorize(function(string){
  
  if(string == "{}"){
    return("")
  }
  
  temp <- strsplit(string[1], split = '\\"')[[1]]
  temp <- temp[seq(4, length(temp), 4)]
  temp <- tolower(temp)
  
  # Remove everything that is not a number or letter
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", "")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  temp <- gsub(pattern = ' ', replacement = '_', x = temp)
  temp <- stringr::str_trim(temp)
  
  return(paste(temp, collapse = ","))
})


top_n_genres <- Vectorize(function(mov_gen){
  temp <- strsplit(mov_gen, ',')[[1]]
  temp <- temp[temp %in% popular_genres$word[1:50]]
  temp <- paste(temp, collapse = ',')
  return(temp)
})


clean_string <- Vectorize(function(string){
  # Lowercase
  temp <- tolower(string)
  # Remove puncuation
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Remove stop words
  temp <- unlist(strsplit(temp, " "))
  temp <- temp[!temp %in% stop_words$word]
  temp <- paste(temp, collapse = " ")
  # Stem words
  temp <- textstem::stem_strings(temp)
  ## Remove any whitespace at beginning or end of line
  temp <- stringr::str_trim(temp)
  
  return(temp)
})


top_n_predictions <- function(text, n){
  temp <- predict(model, newdata = text, type = 'labels')
  temp <- temp[1, 3:length(temp)]
  temp <- sort(temp, decreasing = TRUE)[1:n]
  temp <- colnames(temp)
  temp <- paste(temp, collapse = ',')
  return(temp)
}


our_metric <- function(model, text, genres){
  temp <- predict(model, newdata = text, type = 'labels')
  temp <- temp[,strsplit(genres, ',')[[1]]] %>% as.numeric(.)
  temp <- mean(temp)
  return(temp)
}


## DownloadData ------------------------

# On this website;
# http://www.cs.cmu.edu/~ark/personas/
# download the Dataset;
# http://www.cs.cmu.edu/~ark/personas/data/MovieSummaries.tar.gz

# OR

if (FALSE) {
  fn <- "http://www.cs.cmu.edu/~ark/personas/data/MovieSummaries.tar.gz"
  download.file(fn,destfile="MovieSummaries.tar.gz")
  untar("MovieSummaries.tar.gz",list=TRUE)  ## check contents
  untar("MovieSummaries.tar.gz")
}



## Prepare Data ----------

# we want plot_summaries.txt and movie.metadata.tsv
# these two connect on wikipedia movie id

movie_metadata <- data.table::fread('MovieSummaries/movie.metadata.tsv')
movie_metadata %>% View()

movie_plots <- data.table::fread("MovieSummaries/plot_summaries.txt", sep = "\t")
movie_plots %>% View()

dim(movie_metadata)
# [1] 81741     9
dim(movie_plots)
# [1] 42306     2


# a1 <- head(movie_metadata$V9, n = 19)
# a2 <- eval(a1[1])
# strsplit(a1[1], split = '\\"')[[1]] %>% View

# get_classes(a1[18])
# get_classes(a1[8:18])
# test No Genre
# get_classes(a1[19])

# get popular genres
popular_genres <- movie_metadata %>% 
  copy(.) %>% 
  .[, .(V9)] %>% 
  data.table::setnames(., c("messy_genres")) %>% 
  .[, clean_genres := get_classes(messy_genres)] %>% 
  .[, messy_genres := NULL] %>% 
  tidytext::unnest_tokens(., "word", clean_genres) %>% 
  .[, (word_freq = .N), keyby = word] %>% 
  .[order(-V1)]
  
popular_genres %>% View()
#

set.seed(21122019)

movies <- movie_metadata %>% 
  copy(.) %>% 
  .[, .(V1, V3, V9)] %>% 
  data.table::setnames(., c("ID", "Title", "messy_genres")) %>% 
  .[, clean_genres := get_classes(messy_genres)] %>% 
  .[, clean_genres := top_n_genres(mov_gen = clean_genres)] %>%
  # remove rows that now have no genres
  .[clean_genres != ""] %>% 
  .[, messy_genres := NULL] %>%
  data.table::merge.data.table(x = .,
                               y = movie_plots,
                               by.x = "ID", 
                               by.y = "V1") %>% 
  data.table::setnames(., "V2", "Plot") %>% 
  .[, clean_text := clean_string(Plot)] %>%
  .[, dataset := sample(x = c("TRAIN", "DEV", "TEST"), size = .N, replace = TRUE, prob = c(.8, .1, .1))]


movies %>% dim()
# [1] 41415     6
View(movies)

movies$dataset %>% table(.)

# save data
saveRDS(object = movies, file = "movie_plot_genre.rds")

movies <- readRDS(file = "movie_plot_genre.rds")


## EDA --------------------------

dim(movies)
# [1] 41415     6

# so might have some repeat movies titles...
length(unique(movies$Title))
# [1] 39182

# check count of unique genres
movies %>% 
  copy() %>% 
  .[, clean_genres_vectors := sapply(clean_genres, function(x){strsplit(x, ',')[[1]]})] %>% 
  .[, clean_genres_vectors] %>% 
  unlist() %>% unique()

length(unique(movies$ID))
# [1] 41409
# ID's are all unique, so don't need to worry about duplicate rows

# what are the repeated movies?
movies[duplicated(movies$Title) | duplicated(movies$Title, fromLast=TRUE)] %>% 
  View()

movies[duplicated(movies$Title) | duplicated(movies$Title, fromLast=TRUE), Title] %>% 
  unique(.) %>% 
  length()
# [1] 1790

# what is the distribution of number of genres per movie?
num_genre_df <- movies %>%
  .[, .(clean_genres, Title)] %>% 
  .[, num_genres := stringr::str_count(clean_genres, "\\,") + 1] %>% 
  .[order(-num_genres)]

num_genre_df %>% View()

g1 <- ggplot(data = num_genre_df, 
             mapping = aes(x = num_genres)) +
  geom_histogram(binwidth = 1)
g1

g2 <- ggplot(data = num_genre_df, 
             mapping = aes(y = num_genres)) +
  geom_boxplot()
g2


# let's look at the genres
popular_genres %>% 
  .[, cumsum := cumsum(V1)/sum(V1)] %>% 
  View()
# The top 12 genres account for over 50% of the genres
# (all genres that is, not just the top 50)

popular_genres[1:50, ] %>% 
  .[, cumsum := cumsum(V1)/sum(V1)] %>% 
  View()


# Let's look at word counts
word_frequencies <- movies %>%
  .[, "clean_text"] %>% 
  tidytext::unnest_tokens(., "word", clean_text) %>% 
  .[, (word_freq = .N), keyby = word] %>% 
  .[order(-V1)]

word_frequencies %>% View(.)

saveRDS(object = word_frequencies, file = 'word_frequencies.rds')

## Modellings ----------------

# prepare y
labels <- movies %>% 
  .[movies$dataset == "TRAIN"] %>% 
  .[, clean_genres_vectors := sapply(clean_genres, function(x){strsplit(x, ',')[[1]]})] %>% 
  .[, clean_genres_vectors]

length(labels)
labels %>% head

# prepare y
text <- movies[dataset == 'TRAIN', clean_text]

length(text)
class(text)

model <- ruimtehol::embed_tagspace(x = text
                                   ,y = labels
                                   ,dim = 20
                                   ,epoch = 100
                                   ,lr = 0.1
                                   ,loss = 'softmax'
                                   ,negSearchLimit = 10
                                   ,ws = 5
                                   ,minCount = 1000)

# save model
model_name <- "model_2020-01-07"
starspace_save_model(model, paste0('models/', model_name))

# load model
model <- starspace_load_model(paste0('models/', model_name))



# get metrics and genre predictions
nrow_movies <- nrow(movies)
metrics <- rep(0, nrow_movies)
predictions <- rep("", nrow_movies)

for (i in 1:nrow_movies) {
  metrics[i] <- our_metric(model = model, text = movies$clean_text[i], genres = movies$clean_genres[i])
  predictions[i] <- top_n_predictions(text = movies$clean_text[i], n = 5)
}

movie_predictions <- movies %>%
  copy() %>% 
  .[, top_5_genres := predictions] %>% 
  .[, metrics := metrics] %>% 
  .[, c("ID", "metrics", "Title", "clean_genres", "top_5_genres", "Plot", "clean_text", "dataset"), with = FALSE]

movie_predictions %>% View

saveRDS(object = movie_predictions, file = 'movie_predictions.rds')

movie_predictions <- readRDS('movie_predictions.rds')

# average of our metric
results <- movie_predictions %>% 
  .[, .(avg_metric = mean(metrics, na.rm = TRUE)), by = dataset]

results
# dataset avg_metric
# 1:   TRAIN  0.1372981
# 2:     DEV  0.1330690
# 3:    TEST  0.1324563


