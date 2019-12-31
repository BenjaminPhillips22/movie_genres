
library(data.table)
library(magrittr)

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



