### Load Required Packages 

```{r}
require(quanteda)
library(data.table)
library(caret)
```

### Import Data 
```{r}
ratings <- read.csv("ratings.csv")
movies <- read.csv("movies.csv")
```

### Data Analysis

#### Create user-item matrix

```{r}
# The following function takes ratings data as input and outputs the user-item matrix
user_item_matrix <- function(d){
  # Create user-item matrix
  # nrow = no. of unique users; ncol = no. of unique movies 
  ui_mat <- matrix(nrow = length(unique(d$userId)), ncol = length(unique(d$movieId))) 
  rownames(ui_mat) <- sort(unique(d$userId))
  colnames(ui_mat) <- sort(unique(d$movieId))
  
  # Fill in ratings values 
  for (i in 1:nrow(d)) {
    x <- as.character(d$userId[i])
    y <- as.character(d$movieId[i])
    ui_mat[x, y] <- d$rating[i]
  }
  return(ui_mat)
}

```

#### Create mean-centred user-item matrix 

```{r}
# The following function takes ratings data as input and outputs the mean-centred user-item matrix
mc_user_item_matrix <- function(d){
  ui_mat <- user_item_matrix(d)
  # Calculate mean rating for each user
  mratings <- rowMeans(ui_mat, na.rm=T)
  # Calculate mean-centred ratings
  mc_ui_mat <- ui_mat - mratings
  rownames(mc_ui_mat) <- rownames(ui_mat)
  colnames(mc_ui_mat) <- colnames(ui_mat)
  return(mc_ui_mat)
}
```

#### Create cosine similarity matrix 

```{r}
# The following function takes the mean-centred user-item matrix as input and outputs the cosine similarity computed along rows of the matrix 
cossim <- function(mc_ui_mat){
  # Replace NA in user-item matrix with 0
  mc_ui_mat[is.na(mc_ui_mat)] <- 0
  
  # Create cosine similarity matrix
  cs_mat <- matrix(0,nrow = nrow(mc_ui_mat), ncol=nrow(mc_ui_mat))
  rownames(cs_mat) <- rownames(mc_ui_mat)
  colnames(cs_mat) <- rownames(mc_ui_mat)
  
  for (i in 1:nrow(mc_ui_mat)) {
    for (j in i:nrow(mc_ui_mat)) {
      if (i==j){
        cs_mat[i, j] <- 1
      } else { 
        num <- mc_ui_mat[i,] %*% mc_ui_mat[j,]
        denom <- sqrt(mc_ui_mat[i,] %*% mc_ui_mat[i,]) * sqrt(mc_ui_mat[j,] %*% mc_ui_mat[j,])
        if(denom != 0){
          cs_mat[i, j] <- num / denom
          cs_mat[j, i] <- num / denom
        } else {
          cs_mat[i, j] <- 0
          cs_mat[j, i] <- 0
        }
      }
    }
  }
  return(cs_mat)
}
```

#### Predict ratings using user-based filtering

```{r}
# The following function takes ratings data and target user as input and outputs predicted ratings for all unrated movies of target user using user-based filtering
predicted_ratings_ubf <- function(d, userId) {
  # Generate user-item matrix, mean-centred user-item matrix, and cosine similarity matrix
  ui_mat <- user_item_matrix(d)
  mc_ui_mat <- mc_user_item_matrix(d)
  cs_mat <- cossim(mc_ui_mat)
  userId <- as.character(userId)
  
  # Identify unrated movies of target user
  unrated_movies <- names(ui_mat[userId,][is.na(ui_mat[userId,])])
  # Predict ratings for unrated movies of target user
  pratings <- rep(0, length(unrated_movies))
  names(pratings) <- unrated_movies
  
  for (m in c(unrated_movies)) {
    # Identify all other users who have rated movie m 
    susers <- names(ui_mat[, m][!is.na(ui_mat[, m])])
    
    if (length(susers > 0)) {
      # Find cosine similarity of identified users to target user
      cossim_susers <- cs_mat[userId, c(susers)]
      # Consider only the 10 nearest neighbours with positive cosine similarity values (If less than 10, include all)
      nearest_neighbours <- names(sort(cossim_susers[cossim_susers > 0], decreasing=T)[1:min(length(cossim_susers[cossim_susers > 0]), 10)])
     
      if (length(nearest_neighbours) >= 2) {
        pratings[m] <- mean(ui_mat[userId,], na.rm=T) +
          (cossim_susers[nearest_neighbours] %*% mc_ui_mat[nearest_neighbours, m])/sum(cossim_susers[nearest_neighbours])
      }
    }
  }
  return(pratings)
}
```

#### Make recommendations using user-based filtering

```{r}
# The following function takes ratings data and target user as input and outputs top 5 recommended movies for target user using user based filtering 
recommendations_ubf <- function(d, userId){
  # Generate predicted ratings 
  pratings <- predicted_ratings_ubf(d, userId)
  # Identify top 5 movies with positive predicted ratings (If less than 5, include all)
  top5_pratings <- sort(pratings[pratings>0], decreasing=T)[1:min(5,length(pratings[pratings>0]))]
  recommendations <- names(top5_pratings)
  recommendations_movietitle <- as.character(movies$title[movies$movieId %in% recommendations])
  return(data.frame(movie_title = recommendations_movietitle, predicted_rating = round(top5_pratings,2)))
}
```

```{r}
recommendations_ubf(ratings, 200)
recommendations_ubf(ratings, 400)
recommendations_ubf(ratings, 600)
```
