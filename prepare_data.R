
library(data.table)
library(magrittr)
library(ggplot2)
library(ruimtehol)

## Functions ---------------------------

get_classes <- Vectorize(function(string){
    
    if(string == "{}"){
        return('no genre')
    }
    
    tmp1 <- strsplit(string[1], split = '\\"')[[1]]
    tmp2 <- tmp1[seq(4, length(tmp1), 4)]
    tmp3 <- tolower(tmp2)
    
    return(paste(tmp3, collapse = ","))
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

set.seed(21122019)

movies <- movie_metadata %>%
    copy(.) %>% 
    .[, .(V1, V3, V9)] %>% 
    data.table::setnames(., c("ID", "Title", "messy_genres")) %>% 
    .[, Genres := get_classes(messy_genres)] %>% 
    .[, messy_genres := NULL] %>% 
    data.table::merge.data.table(x = .,
                                 y = movie_plots,
                                 by.x = "ID", 
                                 by.y = "V1") %>% 
    data.table::setnames(., "V2", "Plot") %>% 
    .[, dataset := sample(x = c("TRAIN", "DEV", "TEST"), size = .N, replace = TRUE, prob = c(.8, .1, .1))]


movies %>% dim()
# [1] 42207     4
movies %>% View()

movies$dataset %>% table(.)

# save data
saveRDS(object = movies, file = "movie_plot_genre.rds")

movies <- readRDS(file = "movie_plot_genre.rds")


## EDA --------------------------

# look at the distribution of genres
sapply(X = movies[1:5], 
       FUN = function(x){
           strsplit(x$Genres, split = ',')[[1]] %>% 
               paste(., x$ID, sep = '-')
       }) %>%
    unlist(x = ., use.names = FALSE)


apply(X = movies[1:5], MARGIN = 1, 
      FUN = function(x){
          strsplit(x$Genres, split = ',')[[1]] #%>% 
              # paste(., x$ID, sep = '-')
      }) %>%
    unlist(x = ., use.names = FALSE)

genres_tmp1 <- mapply(function(Genres, Title){
    FUN = strsplit(Genres, split = ',')[[1]] %>% 
        paste(., Title, sep = ',,,')},
    movies$Genres, movies$Title) %>% 
    unlist(x = ., use.names = FALSE) %>% 
    strsplit(., split = ',,,') 

genres_tmp2 <- sapply(genres_tmp1, FUN = function(x){x[1]})
genres_tmp3 <- sapply(genres_tmp1, FUN = function(x){x[2]})

genres_df <- data.table(Title = genres_tmp3, Genre = genres_tmp2)

genres_df %>% View()                      

# how many movies?
dim(movies)
# [1] 42207     5
length(unique(genres_df$Title))
# [1] 39917

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
    .[, .(num_genres = .N), by = Title] %>% 
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

# let's look at the genres for the top 5 movies with the most genres
num_genre_df %>% 
    head(., 5) %>% 
    merge.data.table(x = .,
                     y = movies,
                     by.x = "Title", 
                     by.y = "Title") %>% 
    View(.)

# the outliers are due to repeated film titles.

# let's look at the genres

popular_genres <- genres_df %>% 
    .[, .(count = .N), by = Genre] %>% 
    .[order(-count)] %>% 
    .[, cumsum := cumsum(count)/sum(count)]

popular_genres %>% View
# The top 10 genres account for 46% of the genres
# The top 74 genres account for 90% of the genres

