
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
    return('no genre')
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
  # Remove everything that is not a number or letter (may want to keep more 
  # stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # remove stop words
  temp <- unlist(strsplit(temp, " "))
  temp <- temp[!temp %in% stop_words$word]
  temp <- paste(temp, collapse = " ")
  # stem words
  temp <- textstem::stem_strings(temp)
  ## remove any whitespace at beginning or end of line
  temp <- stringr::str_trim(temp)
  
  return(temp)
})

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
  ## or, if you just want to extract the target file:
  # untar("tmp.tar.gz",files="wp2011-survey/anon-data.csv")
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
# [1] 41409     6
View(movies)

movies$dataset %>% table(.)

# save data
saveRDS(object = movies, file = "movie_plot_genre.rds")

movies <- readRDS(file = "movie_plot_genre.rds")


## EDA --------------------------

# look at the distribution of genres

genres_tmp1 <- mapply(function(Genres, Title, ID){
  FUN = strsplit(Genres, split = ',')[[1]] %>% 
    paste(., Title, ID, sep = ',,,')}, 
  movies$Genres, movies$Title, movies$ID) %>% 
  unlist(x = ., use.names = FALSE) %>% 
  strsplit(., split = ',,,') 

genres_tmp_genre <- sapply(genres_tmp1, FUN = function(x){x[1]})
genres_tmp_title <- sapply(genres_tmp1, FUN = function(x){x[2]})
genres_tmp_id <- sapply(genres_tmp1, FUN = function(x){x[3]})

genres_df <- data.table(ID = genres_tmp_id, Title = genres_tmp_title, Genre = genres_tmp_genre)

genres_df %>% View()                      

# how many movies?
dim(movies)
# [1] 42207     5
length(unique(genres_df$Title))
# [1] 39917
length(unique(genres_df$ID))
# [1] 42207

# so might have some repeat movies titles...
length(unique(movies$Title))
# [1] 39917

# what are the repeated movies?
movies[duplicated(movies$Title) | duplicated(movies$Title, fromLast=TRUE)] %>% 
  View()

movies[duplicated(movies$Title) | duplicated(movies$Title, fromLast=TRUE), Title] %>% 
  unique(.) %>% 
  length()
# [1] 1830

# 1830 repeated movie titles. some are different movies with the same title, most are
# the same movie. Due to the general inconsistancy that the movies are labelled,
# I am happy to leave those repeated movies in, as the have different plot descriptions,
# even for the same movie, and different genres.

# what is the distribution of number of genres per movie?

num_genre_df <- genres_df %>%
  .[, .(num_genres = .N), by = .(ID, Title)] %>% 
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

popular_genres <- genres_df %>% 
  .[, .(count = .N), by = Genre] %>% 
  .[order(-count)] %>% 
  .[, cumsum := cumsum(count)/sum(count)]

popular_genres %>% View
# The top 10 genres account for 46% of the genres
# The top 74 genres account for 90% of the genres


# Let's look at word counts for the plots
word_frequencies <- movies %>%
  .[, "clean_text"] %>% 
  tidytext::unnest_tokens(., "word", clean_text) %>% 
  .[, (word_freq = .N), keyby = word] %>% 
  .[order(-V1)]

word_frequencies %>% View(.)

saveRDS(object = word_frequencies, file = 'word_frequencies.rds')

## Modellings ----------------

# prepare x
x_values <- movies$clean_text[movies$dataset == "TRAIN"]

# prepare y
# we will only use the top 50 most popular genres
prep_genres <- Vectorize(function(arg){
  temp <- strsplit(arg, ",")
  temp <- unlist(temp)
  temp <- temp[temp %in% popular_genres$Genre[1:50]]
  if (length(temp) == 0) {
    temp <- "no_genre"
    return(temp)
  }
  temp <- gsub(pattern = ' ', replacement = '_', x = temp)
  temp <- gsub(pattern = '-', replacement = '_', x = temp)
  temp <- gsub(pattern = '/', replacement = '_', x = temp)
  return(temp)
}, USE.NAMES = FALSE)
labels <- prep_genres(movies$Genres[movies$dataset == "TRAIN"])

length(labels)
View(labels)
length(x_values)
length(labels)
x_values %>% View
labels %>% head

model <- ruimtehol::embed_tagspace(x = movies$clean_text
                                   ,y = labels
                                   ,dim = 20
                                   ,epoch = 100
                                   ,lr = 0.1
                                   ,loss = 'softmax'
                                   ,negSearchLimit = 10
                                   ,ws = 5
                                   ,minCount = 1000)

# save model
starspace_save_model(model, "model_2020-01-04")

# load model
model <- starspace_load_model("model_2020-01-04")


predict(model, newdata = movies$clean_text[1], type = 'labels')
