### Load Required Packages 

```{r}
require(quanteda)
library(data.table)
library(caret)
```

### Import data 
```{r}
ratings <- read.csv("ratings.csv")
movies <- read.csv("movies.csv")
```

### Data analysis
  
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
  
  # Create mean-centred user-item matrix
  mc_ui_mat <- ui_mat - mratings
  rownames(mc_ui_mat) <- rownames(ui_mat)
  colnames(mc_ui_mat) <- colnames(ui_mat)
  return(mc_ui_mat)
}
```

#### Create cosine similarity matrix 

```{r}
# The following function takes the mean-centred user-item matrix as input and outputs the cosine similarity matrix 
cossim <- function(mc_ui_mat){
  # Replace NA in user-item matrix with 0
  mc_ui_mat[is.na(mc_ui_mat)] <- 0
  
  # Create cosine similarity matrix
  cs_mat <- matrix(0,nrow = nrow(mc_ui_mat), ncol=nrow(mc_ui_mat))
  rownames(cs_mat) <- colnames(mc_ui_mat)
  colnames(cs_mat) <- colnames(mc_ui_mat)
  
  # Compute cosine similarity along columns of the matrix 
  for (i in 1:ncol(mc_ui_mat)) {
    for (j in i:ncol(mc_ui_mat)) {
      if (i==j){
        cs_mat[i, j] <- 1
      } else { 
        num <- mc_ui_mat[,i] %*% mc_ui_mat[,j]
        denom <- sqrt(mc_ui_mat[,i] %*% mc_ui_mat[,i]) * sqrt(mc_ui_mat[,j] %*% mc_ui_mat[,j])
        
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

#### Predict ratings using item-based filtering

```{r}
# The function predicted_ratings_ibf takes data and user id as input and outputs predicted ratings for all unrated movies of target user using item-based filtering
predicted_ratings_ibf <- function(d, userId) {
  # Generate user-item matrix, mean-centred user-item matrix, and cosine similarity matrix
  ui_mat <- user_item_matrix(d)
  mc_ui_mat <- mc_user_item_matrix(d)
  cs_mat <- cossim(t(mc_ui_mat))
  userId <- as.character(userId)
  
  # Identify unrated movies of target user
  unrated_movies <- names(ui_mat[userId,][is.na(ui_mat[userId,])])[1:10]
  
  # Predict ratings for unrated movies of target user
  pratings <- rep(0, length(unrated_movies))
  names(pratings) <- unrated_movies
  
  for (m in c(unrated_movies)) {
    # Identify all other movies that user has rated
    smovies <- names(ui_mat[userId,][!is.na(ui_mat[userId,])])
    
    if (length(smovies > 0)) {
      # Find cosine similarity of identified movies to target movie
      cossim_smovies <- cs_mat[m, c(smovies)]
      
      # Consider only the 10 nearest neighbours with positive cosine similarity values (If less than 10, include all)
      nearest_neighbours <- names(sort(cossim_smovies[cossim_smovies > 0], decreasing=T)[1:min(length(cossim_smovies[cossim_smovies > 0]), 10)])
      
      if (length(nearest_neighbours) >= 2) {
        pratings[m] <- mean(ui_mat[,m], na.rm=T) +
          (cossim_smovies[nearest_neighbours] %*% mc_ui_mat[userId, nearest_neighbours])/sum(cossim_smovies[nearest_neighbours])
      }
    }
  }
  return(pratings)
}
```

#### Make recommendations using item-based filtering

```{r}
# The following function takes ratings data and target user as input and outputs top 5 recommended movies for target user using item based filtering 
recommendations_ibf <- function(d, userId){
  # Generate predicted ratings 
  pratings <- predicted_ratings_ibf(d, userId)
  
  # Identify top 5 movies with positive predicted ratings (If less than 5, include all)
  top5_pratings <- sort(pratings[pratings>0], decreasing=T)[1:min(5,length(pratings[pratings>0]))]
  recommendations <- names(top5_pratings)
  recommendations_movietitle <- as.character(movies$title[movies$movieId %in% recommendations])
  return(data.frame(movie_title = recommendations_movietitle, predicted_rating = round(top5_pratings,2)))
}
```

```{r}
recommendations_ibf(ratings, 200)
recommendations_ibf(ratings, 400)
recommendations_ibf(ratings, 600)
```
