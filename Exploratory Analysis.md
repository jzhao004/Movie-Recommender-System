#### Load Required Libraries 

```{r}
library(data.table)
library(caret)
library(tidyverse)
library(ggplot2)
library(hexbin)
require(quanteda)
```

#### Import Data 

```{r}
movies <- read.csv("movies.csv")
ratings <- read.csv("ratings.csv")
tags <- read.csv("tags.csv")

# Merge datasets 
rdata <- merge(ratings, movies, by="movieId")
tdata <- merge(tags, movies, by="movieId")

genres <- c("action","adventure","animation","children","comedy","crime","documentary","drama","fantasy","film.noir","horror","imax","musical","mystery","romance","sci.fi","thriller","war","western")
```

#### Data Analysis 
##### What are the most common words in movie titles? 

```{r}
# Create corpus for movie titles
title_corpus <- corpus(as.character(movies[,"title"]))
# Create document-term frequency matrix (dfm)
title_dfm <- dfm(title_corpus,remove=stopwords("english"), remove_numbers=TRUE, remove_punct=TRUE)
docvars(title_dfm, field = "MovieId") <- movies$movieId
#Draw wordcloud
textplot_wordcloud(title_dfm, min_size = 0.5, max_size = 5, max_words = 150)
```

##### What are the percentages of movies by genre? 

```{r}
# Calculate no. of unique genres
length(genres)

# Calculate no. and percentage of movies by genre
nmovies_genre <- colSums(movies[genres]); nmovies_genre
pmovies_genre <- data.frame(genre=genres, percentage=colSums(movies[genres])/nrow(movies))
data.frame(genre=pmovies_genre$genre, no_of_movies = nmovies_genre, percentage=paste(round(pmovies_genre$percentage,3)*100,"%"))
      
# Plot graph of percentage of movies by genre
ggplot(pmovies_genre, aes(x=reorder(genre,percentage), y=percentage)) + geom_bar(stat="identity", width=1, color="black", alpha=0.5) + coord_flip() + labs(title="Percentage of Movies by Genre", x="Genre", y="Percentage")
```

###### There are 19 genres altogether with Drama, Comedy, and Thriller being the top 3 genres by total no. of movies produced. 

##### How many movies did users rate on average? 

```{r}
# Calculate summarys statistics of no. of movies rated per user
summary(c(table(ratings$userId)))
paste("Standard Deviation", round(sd(c(table(ratings$userId))),1))
```

###### Users on average rated 165.3 movies, with a standard deviation of 269.5. The median number of movies rated is 70.5. 

##### How are ratings distributed? 

```{r}
# Calculate summary statistics of ratings
summary(ratings$rating)
paste("Standard Deviation", round(sd(ratings$rating),3))

# Plot graph of distribution of ratings
ggplot(ratings, aes(x=rating)) + geom_histogram(binwidth=0.5, colour="black", alpha=0.5) + scale_x_continuous(breaks=sort(unique(ratings$rating))) + labs(title="Distribution of ratings", x="Rating", y="Count") +  theme(legend.position = "none") 
```

###### The average rating for each movie is 3.5 with a standard deviation of 1.043. The distribution of ratings is skewed to the right, suggesting that users, on average, tend to rate on the higher end of the five-star scale.

##### Do ratings vary by year of release? 

```{r}
# Calculate summary statistics for ratings by year
srating_year <- aggregate(ratings$rating, list(year(ratings$timestamp)), summary)

# Plot graph of average ratings by year
ggplot(srating_year,aes(x=Group.1,y=x[,'Mean'])) + geom_errorbar(aes(ymin=x[,'1st Qu.'], ymax=x[,'3rd Qu.']), width=.3, color="grey") + geom_point() + geom_line() + ylim(0,5.0) + scale_x_continuous(breaks=sort(unique(year(ratings$timestamp)))) + theme(axis.text.x = element_text(angle=90, hjust=1)) + labs(title="Mean ratings by year", x="Year", y="Mean rating")
```

###### Average ratings are fairly consistent between 3 and 4 across the years

##### Do ratings vary by age of movies?

```{r}
# Calculate average ratings by year of release 
mrating_yearofrelease <- aggregate(rdata$rating, list(rdata$yearofrelease), mean)
names(mrating_yearofrelease) <- c("yearofrelease","mrating")

# Calculate ages of movies
rdata$ageofmovie <-  year(Sys.time()) - rdata$yearofrelease

# Plot average ratings by age of movie
ggplot(rdata, aes(x=ageofmovie, y=rating)) + stat_summary(fun.y="mean",geom="point",alpha=0.7)  + stat_smooth(method="lm", color="grey",  se=F) + ylim(0,5) + labs(title="Mean Ratings by Age of Movie", x="Age of Movie", y="Mean Rating")  

round(cor(rdata$ageofmovie,rdata$rating),4)
```

###### There is a very weak positive correlation between average ratings and age of movies (R=0.084). As movies age, there are greater variabilities in average ratings.

##### Do ratings vary across users?

```{r}
# Calculate average ratings by user
mrating_user <- aggregate(rdata$rating, list(rdata$userId), mean)

# Plot graph of average ratings by user
ggplot(rdata, aes(x=userId, y=rating)) + stat_summary(fun.y="mean",geom="point",alpha=0.7) + ylim(0,5) + labs(title="Average Ratings by User", x="User Id", y="Mean Rating")  
```

###### Average ratings are fairly consistent between 2.5 and 4.5 across users

##### Do ratings vary by genre? 

```{r}
# Convert all 0 values in one hot encoded genre data to NA 
r <- rdata[,genres]
r[r==0] <- NA 

# Calculate average ratings by genre 
mrating_genre <- data.frame(genre=names(r), mrating=colMeans(rdata$rating * r, na.rm=T))

# Plot graph of average ratings by genre
ggplot(mrating_genre, aes(x=genre, y=mrating)) + geom_bar(stat="identity",alpha=0.7) + ylim(0,5) + theme(axis.text.x=element_text(angle=90, hjust=1)) + labs(title="Average Ratings by Genre", x="Genre", y="Mean Rating")
```

###### Average ratings are fairly consistent between 3 and 4 across genres

##### Do ratings vary by no. of ratings received per movie? 

```{r}
# Calculate no. of ratings for each movie
nrating <- data.frame(table(rdata$movieId))
names(nrating) <- c("movieId", "nrating")

# Calculate average rating for each movie
mrating <- aggregate(rdata$rating, list(rdata$movieId), mean)
names(mrating) <- c("movieId", "mrating")

# Merge data frames 
nmrating <- merge(nrating, mrating, by="movieId")

#Plot graph of average ratings against no. of ratings
ggplot(nmrating, aes(nrating,mrating)) + stat_binhex(bins=35, alpha=0.7) + stat_smooth(method="lm", color="black", size=1,  se=F) + scale_fill_distiller(palette = "Spectral") + labs(title="Average Ratings against No. of Ratings", x="No. of Ratings", y="Mean Rating") + scale_fill_gradient(name="Count",high="darkturquoise",low="darkgrey")
```
  
###### There is a positive relationship between the total number of ratings a movie received and its average rating.
