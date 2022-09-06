### Movielnse Project, HarvardX PH124.9x, Baqir Fateh
# Date: January, 2021

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(tidyr)
library(stringi)
library(stringr)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

### Data Wrangling and Exploration 
head(edx)
dim(edx)
glimpse(edx)
# Extracting the year that movie was rated
edx<-edx%>%
  mutate(year_rated=year(as_datetime(timestamp)))
#Extracting the year that movie was released
edx<-edx%>%
  mutate(year_released=as.numeric(str_sub(title,-5,-2)))

#Extracting Genres (This line of code took several hours)
edx_gen<-edx%>%
  separate_rows(genres,sep="\\|")
##Genre Disbribution 
genre_matrix<-edx_gen%>%group_by(genres)%>%
  summarise(rating_freq_pergen=n(),mean_rating_pergen=mean(rating),
            movie_pergen_sum=n_distinct(movieId),user_pergen_sum=n_distinct(userId))%>%
  arrange(desc(rating_freq_pergen))
genre_matrix
## Visualize Genre Disbribution 
edx_gen%>%group_by(genres)%>%
  summarise(n=n())%>%
  mutate(total_rating=sum(n),percentage=n/total_rating)%>%
  ggplot(aes(reorder(genres,percentage),percentage, fill=percentage))+
  geom_bar(stat="identity")+
  coord_flip()+
  ggtitle("Genres Disbribution")+
  labs(y="Percentage",x="Genres")

# Calculating Movie Age and Rating Age
edx<-edx%>%
  mutate(movie_age=2021-year_released, rating_age=year_rated-year_released)
head(edx)
dim(edx)
## Rating Distribution 
mean_rating<-mean(edx$rating)
edx%>%ggplot(aes(rating))+
  geom_histogram(color=I("black"),binwidth = 0.2)+
  scale_x_continuous(breaks=seq(0.5,5,0.5))+
  xlab("Movie Rating")+
  ylab("Number of Ratings")+
  ggtitle("Rating Distribution")+
  geom_vline(xintercept = mean_rating,col="blue",linetype="dashed")
## Distribution of Users' Rating 
edx%>%
  group_by(userId)%>%
  summarise(n=n())%>%
  ggplot(aes(n))+
  geom_histogram(bins = 20, color=I("black"))+
  scale_x_log10()+
  ggtitle("Users' Rating Distributin")
## Distribution of Movie's Rating
edx%>%
  group_by(movieId)%>%
  summarise(n=n())%>%
  ggplot(aes(n))+
  geom_histogram(bins = 20, color=I("black"))+
  scale_x_log10()+
  ggtitle("Movie's Rating Disribution")

## Visualize Age and Average Movie Rate by Age
edx%>%group_by(movie_age)%>%
  summarise(rating_avg_by_age=mean(rating))%>%
  ggplot(aes(movie_age,rating_avg_by_age))+
  geom_line()+
  geom_smooth()+
  geom_hline(yintercept = mean_rating)

### Modelling 
##RMSE Function 
RMSE<-function(true_ratings, pred_ratings){
  sqrt(mean((true_ratings-pred_ratings)^2))
}
##prediction just based on the average 
mu<-mean(edx$rating)
mu
naive_rmse<-RMSE(validation$rating,mu)
##Creating a table to store the model and RMSE
rmse_results<-data_frame(Model="Just the Average",RMSE=naive_rmse)
rmse_results%>%knitr::kable() 
## Estimating Age Effect 

validation_age<-validation%>%
  mutate(year_released=as.numeric(str_sub(title,-5,-2)))
validation_age<-validation_age%>%
  mutate(year_rated=year(as_datetime(timestamp)))
validation_age<-validation_age%>%
  mutate(rating_age=year_rated-year_released)
age_effect<-edx%>%
  group_by(rating_age)%>%
  summarise(b_a=mean(rating-mu))
pred_rating_age<-mu+validation_age%>%
  left_join(age_effect,by="rating_age")%>%
  pull(b_a)
model_age_rmse<-RMSE(validation$rating,pred_rating_age)
rmse_results<-bind_rows(rmse_results,data_frame(Model="Age Effect",
                                               RMSE=model_age_rmse))
rmse_results%>%knitr::kable()
## Estimating Movie Effect 
movie_avg<-edx%>%
  group_by(movieId)%>%
  summarise(b_i=mean(rating-mu))
#movie_avg%>%qplot(b_i,geom = "histogram",bins=30,data=.,color=I("black"))
pred_rating_2<-validation%>%
  left_join(movie_avg,by="movieId")%>%
  mutate(prediction=b_i+mu)
model_2_rmse<-RMSE(validation$rating,pred_rating_2$prediction)  
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie Effect",
                                     RMSE = model_2_rmse ))
rmse_results%>%knitr::kable()

## Estimating User Effect, b_u in the Model 
user_avg<- edx %>% 
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

pred_rating_3<-validation%>%
  left_join(movie_avg,by="movieId")%>%
  left_join(user_avg,by="userId")%>%
  mutate(prediction_u=mu+b_i+b_u)%>%
  pull(prediction_u)
model_3_rmse<-RMSE(validation$rating,pred_rating_3)

rmse_results<-bind_rows(rmse_results,
                        data_frame(Model="Movie Effect+User Effect",
                                   RMSE=model_3_rmse))
rmse_results%>%knitr::kable()

##Regularization: Estimating Movie Effect 

lambdas <- seq(0, 10, 0.25)
mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

rmse_results<-bind_rows(rmse_results,
                        data_frame(Model="Regularized Movie Effect",
                                   RMSE=min(rmses)))
rmse_results%>%knitr::kable()

## Regularization: Estimating User Effect in the Model 
lambdas <- seq(0, 10, 0.25)
rmses_u <- sapply(lambdas, function(l){
  mu<-mean(edx$rating)
  b_i<-edx%>%
    group_by(movieId)%>%
    summarise(b_i=sum(rating-mu)/(n()+l))
  b_u<-edx%>%
    left_join(b_i,by="movieId")%>%
    group_by(userId)%>%
    summarise(b_u=sum(rating-b_i-mu)/(n()+l))
  predicted_ratings <- validation %>% 
    left_join(b_i,by="movieId")%>%
    left_join(b_u,by="userId")%>%
    mutate(pred = mu + b_i+b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambdas, rmses_u)  
lambdas[which.min(rmses_u)]

rmse_results<-bind_rows(rmse_results,
                        data_frame(Model="Regularized Movie Effect + Regularized User Effect",
                                   RMSE=min(rmses_u)))
rmse_results%>%knitr::kable()

### Matrix Factorization 
library(recosystem)
train_mf <- edx %>% select(movieId, userId, rating)
test_mf <- validation %>% select(movieId, userId, rating)

train_mf <- as.matrix(train_mf)
test_mf <- as.matrix(test_mf)

write.table(train_mf, file = "trainingset.txt", sep = " ", row.names = FALSE, 
            col.names = FALSE)

write.table(test_mf, file = "testset.txt", sep = " ", 
            row.names = FALSE, col.names =  FALSE)
set.seed(1989, sample.kind = "Rounding")
train_set <- data_file("trainingset.txt")

test_set <- data_file("testset.txt")
r=Reco()
opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 
                                                                         0.2), costp_l1 = 0, costq_l1 = 0, nthread = 1, niter = 10))
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

stored_prediction = tempfile()

r$predict(test_set, out_file(stored_prediction))

real_ratings <- read.table("testset.txt", header = FALSE, sep = " ")$V3


pred_ratings <- scan(stored_prediction)

rmse_model_mf <- RMSE(real_ratings, pred_ratings)
rmse_model_mf
rmse_results<-bind_rows(rmse_results,
                        data_frame(Model="Matrix Factorization",
                                   RMSE=rmse_model_mf))
rmse_results%>%knitr::kable()
