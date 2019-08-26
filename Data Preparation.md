### Load Required Libraries 

```{r}
library(data.table)
library(caret)
library(tidyverse)
```

### Download Data from MovieLens Website 

```{r}
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip",dl)
movies <- data.frame(read.csv(unzip(dl,"ml-latest-small/movies.csv")))
ratings <- data.frame(read.csv(unzip(dl,"ml-latest-small/ratings.csv")))
tags <- data.frame(read.csv(unzip(dl,"ml-latest-small/tags.csv")))
links <- data.frame(read.csv(unzip(dl,"ml-latest-small/links.csv")))
```

### Clean Data 

#### Movies dataset 

```{r}
# Separate movie title and year of release of movie
movies$yearofrelease <- as.numeric(str_extract(movies$title,  "(?<=\\()\\d{4}"))
movies$title <- substr(movies$title,1,str_length(movies$title)-7)

# Output number of NAs for each variable
apply(movies,2,function(x){sum(is.na(x))})

# Replace NA values of yearofrelease
## Find year with max number of movies produced 
## Assume movies with NA vaues in yearofrelease are released in the year where most number of movies are produced 
names(which.max(table(movies$yearofrelease)))
movies$yearofrelease[is.na(movies$yearofrelease)] <- 2002

# Replace (no genres listed) values of genres
## Calculate no. of movies by genre
colSums(movies[genres])
## Assume movies with no genres belong to the most popular genres: drama and comedy
movies$drama[movies["(no genres listed)"]==1] <- 1
movies$comedy[movies["(no genres listed)"]==1] <- 1

# Drop genres and `(no genres listed)`
movies <- subset(movies,select=-c(genres,`(no genres listed)`))
```

#### Ratings dataset 

```{r}
# Onehot encode genre data 
movies.genres <- as.data.table(tstrsplit(tolower(as.character(movies$genres)), "[|]"))
genres <- sort(unique(unlist(movies.genres))) 

movies.genres_onehot <- data.frame() 
for (i in 1:nrow(movies.genres)) { 
  movies.genres_onehot <- rbind(movies.genres_onehot,as.numeric(genres %in% movies.genres[i,])) 
} 

names(movies.genres_onehot) <- c(genres)
movies <- cbind(movies, movies.genres_onehot) 

# Reformat timestamp 
ratings$timestamp <- as.POSIXct(ratings$timestamp, origin="1970-01-01")

# Output number of NAs for each variable
apply(ratings,2,function(x){sum(is.na(x))})
```

#### Tags dataset 

```{r}
# Reformat timestamp 
tags$timestamp <- as.POSIXct(tags$timestamp, origin = "1970-01-01")

# Consolidate tags submitted for each movie
compiled_tags <- data.frame(names(c("movieId", "tag")))

for (movieId in unique(tags$movieId)){
  t <- tags[tags$movieId==movieId,]$tag
  compiled_tags <- rbind(compiled_tags,data.frame(movieId, tag = paste(t, collapse = " ")))
}
tags <- compiled_tags

# Output number of NAs for each variable
apply(tags,2,function(x){sum(is.na(x))})
```

### Export data 

```{r}
write.csv(movies, file = "movies.csv", row.names = F)
write.csv(ratings, file = "ratings.csv", row.names = F)
write.csv(tags, file = "tags.csv", row.names = F)
```
