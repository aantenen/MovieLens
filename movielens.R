#Additional datasetup
#add year and age values, change genres to id value to reduce size of objects
genreList<-data.frame(genres=unique(edx$genres))
genreIDdf<-genreList%>%mutate(genreID=row_number())

addNewColumns<-function(x){
  x<-x%>%left_join(genreIDdf,by='genres')%>%
    mutate(year=as.numeric(str_sub(title,-5,-2)),age=year(as_datetime(timestamp))-as.numeric(str_sub(title,-5,-2)))%>%select(-genres)
  return(x)
  }

edx<-addNewColumns(edx)
validation<-addNewColumns(validation)


#create test set 
test_index<-createDataPartition(edx$rating,times = 1,p=.2,list=FALSE)
test_setEdx<-edx[test_index]
train_setEdx<-edx[-test_index]

#remove movies and users unique to test set
test_setEdx<-test_setEdx%>%
  semi_join(train_setEdx,by='movieId')%>%
  semi_join(train_setEdx,by='userId')

# #setup small test dataset, comment out after completed
# minEdx<-slice_sample(edx,n=1000)
# minTest_index<-createDataPartition(minEdx$rating,times = 1,p=.2,list=FALSE)
# minTest_setEdx<-minEdx[minTest_index]
# minTrain_setEdx<-minEdx[-minTest_index]
# 
# 
# minTest_setEdx<-minTest_setEdx%>%
#   semi_join(minTrain_setEdx,by='movieId')%>%
#   semi_join(minTrain_setEdx,by='userId')
# test_setEdx<-minTest_setEdx
# train_setEdx<-minTrain_setEdx


#setup full data set summaries
edxMovie<-edx%>%mutate(group=movieId)%>%group_by(group)%>%summarise(cnt=n(),avg=mean(rating),min=min(rating),max=max(rating))
edxUser<-edx%>%mutate(group=userId)%>%group_by(group)%>%summarize(cnt=n(),avg=mean(rating),min=min(rating),max=max(rating))

edxYear<-edx%>%mutate(group=year)%>%group_by(group)%>%summarize(cnt=n(),avg=mean(rating),min=min(rating),max=max(rating))
edxAge<-edx%>%mutate(group=age)%>%group_by(group)%>%summarize(cnt=n(),avg=mean(rating),min=min(rating),max=max(rating))

#Clean and Analyze Data
#check if any NA
edxNA<-na.omit(edx)
identical(edxNA,edx)
#check score distribution
hist(edx$year)

#overall average
overallAvgRating<-mean(test_setEdx$rating)
naiveModelRMSE<-RMSE(test_setEdx$rating,overallAvgRating)

#build dataframes for testing groupings against averages
movieReviewCnt <-train_setEdx%>%group_by(movieId,title)%>%
  summarise(avg=mean(rating),cnt=n(),sd=sd(rating),effect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)
  movieReviewCnt <-movieReviewCnt[,c(1,3:8,2)]
  
userReviewCnt <-train_setEdx%>%group_by(userId)%>%
  summarize(avg=mean(rating),cnt=n(),sd=sd(rating),effect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)

genreReviewCnt<-train_setEdx%>%group_by(genreID)%>%
  summarise(avg=mean(rating),cnt=n(),sd=sd(rating),effect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)

yearReviewCnt<-train_setEdx%>%group_by(year)%>%
  summarize(avg=mean(rating),cnt=n(),sd=sd(rating),effect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)

movieAgeReviewCnt<-train_setEdx%>%group_by(age)%>%
  summarize(avg=mean(rating),cnt=n(),sd=sd(rating),effect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)
  

#test individual effects for each category

# guessAvg<- function(x,y){
#   #x - pass list of group from test set
#   #y - pass training data frame grouped by tested attribute
#   #y<-as.matrix(y)
#   guess<-y[which(y[,1]==x),2]
#   as.numeric(guess)
#   
#   
# }

dimensionEffect<-function(x,y){
  guess<-overallAvgRating+test_setEdx%>%left_join(x,by=y)%>%pull(effect)
  as.numeric(guess)
  }

movieEffectModel<-data.frame(guess=dimensionEffect(x=movieReviewCnt,y='movieId'))
movieEffectRMSE<-RMSE(movieEffectModel$guess,test_setEdx$rating)

userEffectModel<-data.frame(guess=dimensionEffect(x=userReviewCnt,y='userId'))
userEffectRMSE<-RMSE(userEffectModel$guess,test_setEdx$rating)

genreEffectModel<-data.frame(guess=dimensionEffect(x=genreReviewCnt,y='genreID'))
genreEffectRMSE<-RMSE(genreEffectModel$guess,test_setEdx$rating)

yearEffectModel<-data.frame(guess=dimensionEffect(x=yearReviewCnt,y='year'))
yearEffectRMSE<-RMSE(yearEffectModel$guess,test_setEdx$rating)

ageEffectModel<-data.frame(guess=dimensionEffect(x=movieAgeReviewCnt,y='age'))
ageEffectRMSE<-RMSE(ageEffectModel$guess,test_setEdx$rating)

#plot model RMSE
#To Do: improve plot, create plot function
effectRmseList<-data.frame(model=c("Overall Avg","Movie Avg","User Avg","Genre Avg","Year Avg","Age Avg"),RMSE=c(naiveModelRMSE,movieEffectRMSE,userEffectRMSE,genreEffectRMSE,yearEffectRMSE,ageEffectRMSE))

#create ridgeline visual with errors from each model

#combine dimension effects
#can this be written as a function? Pass list of effects and loop throungh?
#movie effect was greatest by itself so all effects are added in addition to it
movieEffect<-data.frame(movieId=movieReviewCnt$movieId,effect=movieReviewCnt$effect)

userAdjEffect<-train_setEdx%>%left_join(movieReviewCnt,by='movieId')%>%group_by(userId)%>%
  summarize(userAdjEffect = mean(rating - overallAvgRating-effect))
userAdjModel<-test_setEdx%>%left_join(movieEffect,by='movieId')%>%left_join(userAdjEffect,by='userId')%>%
  mutate(guess=overallAvgRating+effect+userAdjEffect)%>%pull(guess)
userAdjEffectRMSE<-RMSE(userAdjModel,test_setEdx$rating)

genreAdjEffect<-train_setEdx%>%left_join(movieEffect,by='movieId')%>%left_join(userAdjEffect,by='userId')%>%
  group_by(genreID)%>%summarize(genreAdjEffect=mean(rating-overallAvgRating-effect-userAdjEffect))
genreAdjModel<-test_setEdx%>%left_join(movieEffect,by='movieId')%>%left_join(userAdjEffect,by='userId')%>%
 left_join(genreAdjEffect,by='genreID')%>%mutate(guess=overallAvgRating+effect+userAdjEffect+genreAdjEffect)%>%pull(guess)
genreAdjEffectRMSE<-RMSE(genreAdjModel,test_setEdx$rating)

yearAdjEffect<-train_setEdx%>%left_join(movieReviewCnt,by='movieId')%>%left_join(userAdjEffect,by='userId')%>%left_join(genreAdjEffect,by='genreID')%>%
  group_by(year)%>%summarise(yearAdjEffect=mean(rating-overallAvgRating-effect-userAdjEffect-genreAdjEffect))
#yearAdjEffectOnly<-train_setEdx%>%group_by(year)%>%summarize(yearAdjEffect = mean(rating-overallAvgRating))
yearAdjModel<-test_setEdx%>%left_join(movieReviewCnt,by='movieId')%>%
  left_join(userAdjEffect,by='userId')%>%left_join(genreAdjEffect,by='genreID')%>%left_join(yearAdjEffect,by='year')%>%
  mutate(guess=overallAvgRating+effect+userAdjEffect+genreAdjEffect+yearAdjEffect+genreAdjEffect)%>%pull(guess)
yearAdjRMSE<-RMSE(yearAdjModel,test_setEdx$rating)

ageAdjEffect<-train_setEdx%>%left_join(movieReviewCnt,by='movieId')%>%left_join(userAdjEffect,by='userId')%>%
  left_join(genreAdjEffect,by='genreID')%>%group_by(age)%>%
  summarize(ageAdjEffect=mean(rating-overallAvgRating-effect-userAdjEffect-genreAdjEffect))
ageAdjModel<-test_setEdx%>%left_join(movieReviewCnt,by='movieId')%>%left_join(userAdjEffect,by='userId')%>%
  left_join(genreAdjEffect,by='genreID')%>%left_join(ageAdjEffect,by='age')%>%mutate(guess=overallAvgRating+effect+userAdjEffect+genreAdjEffect)%>%pull(guess)
ageAdjRMSE<-RMSE(ageAdjModel,test_setEdx$rating)

adjEffectRMSEList<-data.frame(model=c("Overall Avg","Movie Avg","User Adj","Genre Adj","Year Adj","Age Adj"),RMSE=c(naiveModelRMSE,movieEffect,userAdjEffectRMSE,genreAdjEffectRMSE,yearEffectRMSE,ageEffectRMSE))
#Regularization
lambda<-seq(1,10,.5)


allEffectRegularRMSES<-sapply(lambda,function(L){
  movieEffectRegular<-train_setEdx%>%group_by(movieId)%>%summarize(movieRegularized=sum(rating-overallAvgRating)/(n()+L))
  movieEffectRegularModel<-test_setEdx%>%left_join(movieEffectRegular, by='movieId')%>%group_by(movieId)%>%summarize(guess=overallAvgRating+movieRegularized)%>%pull(guess)
  movieEffectRegularRMSE <-RMSE(movieEffectRegularModel,test_setEdx$rating)
  
  userEffectRegular<-train_setEdx%>%left_join(movieEffectRegular,by='movieId')%>%group_by(userId)%>%
    summarize(userRegularized=sum(rating-overallAvgRating-movieRegularized)/(n()+L))
  userEffectREgularModel<-test_setEdx%>%left_join(movieEffectRegular, by='movieId')%>%left_join(userEffectRegular,by='userId')%>%group_by(userId)%>%
    summarise(guess=overallAvgRating+movieRegularized+userRegularized)
  userEffectRegularRMSE<-RMSE(userEffectRegularModel,test_setEdx$rating)  
  
  genreEffectRegular<-train_setEdx%>%left_join(movieEffectRegular, by='movieId')%>%left_join(userEffectRegular,by='userId')%>%group_by(genreID)%>%
    summarize(genreRegularized=sum(rating-overallAvgRating-movieRegularized-userRegularized)/(n()+L))
  genreEffectRegularModel<-test_setEdx%>%left_join(movieEffectRegular, by='movieId')%>%left_join(userEffectRegular,by='userId')%>%left_join(genreEffectRegular,by='genreID')%>%
    group_by(genreID)%>%mutate(guess=overallAvgRating+movieRegularized+userRegularized+genreRegularized)%>%pull(guess)
  genreEffectRegularRMSE<-RMSE(genreEffectRegularModel,test_setEdx$rating)
#ear and age do not affect RMSE
  yearEffectRegular<-train_setEdx%>%left_join(movieEffectRegular,by='movieId')%>%left_join(userEffectRegular, by='userId')%>%left_join(genreEffectRegular,by='genreID')%>%group_by(year)%>%
    summarize(yearRegularized=sum(rating-overallAvgRating-movieRegularized-userRegularized-genreRegularized)/(n()+5))
  yearEffectRegularModel <- test_setEdx%>%left_join(movieEffectRegular)%>%left_join(userEffectRegular)%>%left_join(genreEffectRegular,by='genreID')%>%left_join(yearEffectRegular)%>%
    mutate(guess=overallAvgRating+movieRegularized+userRegularized+genreRegularized+yearRegularized)%>%pull(guess)
  yearEffectRegularRMSE<- RMSE(yearEffectRegularModel,test_setEdx$rating)
  
  ageEffectRegular<-train_setEdx%>%left_join(movieEffectRegular,by='movieId')%>%left_join(userEffectRegular,by='userId')%>%group_by(age)%>%
    summarize(ageRegularized=sum(rating-overallAvgRating-movieRegularized-userRegularized)/(n()+5))
  ageEffectRegularModel <- test_setEdx%>%left_join(movieEffectRegular)%>%left_join(userEffectRegular)%>%left_join(ageEffectRegular)%>%
    mutate(guess=overallAvgRating+movieRegularized+userRegularized+ageRegularized)%>%pull(guess)
  ageEffectRegularRMSE<- RMSE(ageEffectRegularModel,test_setEdx$rating)
  regularRMSEList<-data.frame(c(L,movieEffectRegularRMSE,userEffectRegularRMSE,genreEffectRegularRMSE,yearEffectRegularRMSE,ageEffectRegularRMSE),)
return(regularRMSEList)                                                                
})

movieEffectRegular<-train_setEdx%>%group_by(movieId)%>%summarize(movieRegularized=sum(rating-overallAvgRating)/(n()+5))
userEffectRegular<-train_setEdx%>%left_join(movieEffectRegular,by='movieId')%>%group_by(userId)%>%
  summarize(userRegularized=sum(rating-overallAvgRating-movieRegularized)/(n()+5))
userEffectRegularModel<-test_setEdx%>%left_join(movieEffectRegular)%>%left_join(userEffectRegular)%>%
  mutate(guess=overallAvgRating+movieRegularized+userRegularized)%>%pull(guess)
testRegularizationRMSE<-RMSE(userEffectRegularModel,test_setEdx$rating)

#final model full edx vs validation data set using regularized effects
finalMovieEffectRegular<-edx%>%group_by(movieId)%>%summarize(finalMovieEffectRegularized=sum(rating-overallAvgRating)/(n()+5))
finalUserEffectRegular<-edx%>%left_join(finalMovieEffectRegular)%>%group_by(userId)%>%
  summarize(finalUserRegularized=mean(rating-overallAvgRating-finalMovieRegularized)/(n()+5))
finalGenreEffectRegular<-edx%>%left_join(finalMovieEffectRegular)%>%left_join(finalUserEffectRegular)%>%group_by(genreID)%>%
  summarize(finalGenreEffectRegular=sum(rating-overallAvgRating-finalMovieEffectRegularized-finalUserEffectRegularized)/(n()+5))

finalEffectRegularModel<-validation%>%left_join(finalMovieEffectRegular)%>%left_join(finalUserEffectRegular)%>%
  left_join(finalYearEffectRegular)%>%mutate(guess=overallAvgRating+finalMovieRegularized+finalUserRegularized+finalYearRegularized)%>%pull(guess)
finalRMSE<-RMSE(finalEffectRegularModel,validation$rating)
