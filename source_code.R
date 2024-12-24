#----install and load libraries----

install.packages("recommenderlab")
install.packages("ggplot")
install.packages(c("ggplot2", "data.table", "reshape2"))
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

#----Load Data----

setwd("E:/Projects/Movie Recomendation system - R/IMDB-Dataset") # changing the directory struct R works on                     #Author DataFlair
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data)

#----Summary and Head----
summary(movie_data)  
summary(rating_data)   

head(movie_data)
head(rating_data) 

#----Data Pre-processing----
#1.one hot encode genres

movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
library(data.table)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE) #DataFlair
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
}
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)

#2.Search matrix
SearchMatrix <- cbind(movie_data[,1:2], genre_mat2[])
head(SearchMatrix)   


#3.convert matrix into a sparse matrix one
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix


#----Parameters----

recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)#displays list of rec. algo available for the realRatingMatrix
lapply(recommendation_model, "[[", "description")


#----Exploring similar data----
#using operators like cosine, pearson as well as jaccard
#1.similarity between 2 users
similarity_mat <- similarity(ratingMatrix[1:4, ],
                               method = "cosine",
                               which = "users")
as.matrix(similarity_mat)
#vizual representation
image(as.matrix(similarity_mat), main = "User's Similarities")

#2.similarity btwn movies
movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(movie_similarity)

image(as.matrix(movie_similarity), main = "Movies similarity")

#----Extract unique ratings----
rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) 

Table_of_Ratings <- table(rating_values) # creating a count of movie ratings
Table_of_Ratings

#----Most viewed Movies----
library(ggplot2)
movie_views <- colCounts(ratingMatrix) # count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views
table_views$title <- NA
for (index in 1:10325){
  table_views[index,3] <- as.character(subset(movie_data,
                                         movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]

#Bar plot
ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  ggtitle("Total Views of the Top Films")

#Heatmap for ratings
image(ratingMatrix[1:20, 1:25], axes = FALSE, 
		main = "Heatmap of the first 25 rows and 25 columns")


#----Performing Data Preparation-----
    #Selecting useful data.
    #Normalizing data.
    #Binarizing the data.

#set threshold for minimum no. users who have rated a film as 50.
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,colCounts(ratingMatrix) > 50]
movie_ratings

#delineate matrix 
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                     colCounts(movie_ratings) > minimum_users],
main = "Heatmap of the top users and movies")


# Calculate average ratings per user
average_ratings <- rowMeans(movie_ratings)
# Create a histogram using ggplot()
ggplot(data = data.frame(average_ratings), aes(x = average_ratings, fill = I("steelblue"), col = I("red"))) +
  geom_histogram(binwidth = 0.1, color = "black", aes(y = ..count..), fill = "steelblue") +
  labs(title = "Distribution of the Average Rating per User")

#Normalize ratings
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)# returns a logical vector where each element is TRUE if the corresponding row mean is greater than 0.00001, and FALSE otherwise

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                          colCounts(normalized_ratings) > minimum_users],
main = "Normalized Ratings of the Top Users")

#----Performing dAata binarization----
#1 if the rating is>3
binary_minimum_movies<- quantile(rowCounts(movie_ratings),0.95)
binary_minimum_users<- quantile(colCounts(movie_ratings),0.95)
#movies_watched<- binarize(movie_ratings,minRating=1)


good_rated_films<- binarize(movie_ratings,minRating=3)
image(good_rated_films[rowCounts(movie_ratings)>
binary_minimum_movies,
colCounts(movie_ratings)>binary_minimum_users],
main="Heatmap of top users and movies")


#----Collaborative Filtering system----
    #For each Item i1 present in the product catalog, purchased by customer C.
    #And, for each item i2 also purchased by the customer C.
    #Create record that the customer purchased items i1 and i2.
    #Calculate the similarity between i1 and i2.

sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

#----Building Recoomendation system----

recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommen_model <- Recommender(data = training_data,
                          method = "IBCF",
                          parameter = list(k = 30))
recommen_model
class(recommen_model)

#vizualize model
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
   main = "Heatmap of the first rows and columns")

# sum of rows and columns with the similarity of the objects above 0
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")

#Build recommendation system
top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                          newdata = testing_data,
                          n = top_recommendations)
predicted_recommendations

user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                         movie_data$movieId == movies_user1[index])$title)
}
movies_user2



recommendation_matrix <- sapply(predicted_recommendations@items,
                      function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:4]





#-----api------
install.packages("plumber")
install.packages("recommenderlab") # if not already installed


# Load necessary libraries
library(plumber)
library(recommenderlab)

# Load your trained recommendation model
load("recommen_model.RData") # Make sure you have your model saved as an RData file

# Load movie data
movie_data <- read.csv("movie_data.csv") # Replace with your movie data file

#* @apiTitle Recommendation API

#* Get top N recommendations for a user
#* @param user_id The ID of the user for whom to get recommendations
#* @param top_n The number of top recommendations to return (default is 10)
#* @get /recommend
function(user_id, top_n = 10) {
  user_id <- as.integer(user_id)
  top_n <- as.integer(top_n)
  
  # Get the user index in the testing data
  user_index <- which(rownames(testing_data) == user_id)
  
  if(length(user_index) == 0) {
    return(list(error = "User not found"))
  }
  
  # Get recommendations
  predicted_recommendations <- predict(object = recommen_model, newdata = testing_data[user_index, , drop = FALSE], n = top_n)
  
  user_recommendations <- predicted_recommendations@items[[1]]
  movie_ids <- predicted_recommendations@itemLabels[user_recommendations]
  
  # Convert movie IDs to titles
  movie_titles <- sapply(movie_ids, function(id) {
    as.character(subset(movie_data, movie_data$movieId == id)$title)
  })
  
  return(list(recommendations = movie_titles))
}

# Run the API
# plumb("api.R")$run(port = 8000) # Uncomment to run the API


# Save the recommendation model
save(recommen_model, file = "recommen_model.RData")

# Save the movie data
write.csv(movie_data, file = "movie_data.csv", row.names = FALSE)


library(plumber)
# Point to your API script
r <- plumb("api.R")
r$run(port = 8000)

