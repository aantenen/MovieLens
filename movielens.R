#Clean and Analyze Data

#check if any NA
edxNA<-na.omit(edx)
identical(edxNA,edx)
#create test set 
test_index<-createDataPartition(edx$rating,times = 1,p=.2,list=FALSE)
test_setEdx<-edx[test_index]
train_setEdx<-edx[-test_index]

#remove movies and users unique to test set
test_setEdx<-test_setEdx%>%
  semi_join(train_setEdx,by='movieId')%>%
  semi_join(train_setEdx,by='userId')

#setup small test dataset, comment out after completed
minEdx<-slice_sample(edx,n=10000)
minTest_index<-createDataPartition(minEdx$rating,times = 1,p=.2,list=FALSE)
minTest_setEdx<-minEdx[minTest_index]
minTrain_setEdx<-minEdx[-minTest_index]


minTest_setEdx<-minTest_setEdx%>%
  semi_join(minTrain_setEdx,by='movieId')%>%
  semi_join(minTrain_setEdx,by='userId')
test_setEdx<-minTest_setEdx
train_setEdx<-minTrain_setEdx

#add year and age values to test set
test_setEdx<-test_setEdx%>%mutate(year=str_sub(title,-5,-2),age=year(as_datetime(timestamp))-as.numeric(str_sub(title,-5,-2)))

###TO DO check ranges of values: User, Movie, Score
### What to do with review with negative Age
###Change min max to min max score selection not SD

#check score distribution
hist(edx$rating)

#overall average
overallAvgRating<-mean(test_setEdx$rating)
naiveModelRMSE<-RMSE(test_setEdx$rating,overallAvgRating)

#build dataframes for testing groupings against averages
movieReviewCnt <-train_setEdx%>%mutate(group=movieId)%>%group_by(group,title)%>%
  summarise(avg=mean(rating),cnt=n(),sd=sd(rating),movieEffect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)
  movieReviewCnt <-movieReviewCnt[,c(1,3:8,2)]
  
userReviewCnt <-train_setEdx%>%mutate(group=userId)%>%group_by(group)%>%
  summarize(avg=mean(rating),cnt=n(),sd=sd(rating),userEffect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)

genreReviewCnt<-train_setEdx%>%mutate(group=genres)%>%group_by(group)%>%
  summarise(avg=mean(rating),cnt=n(),sd=sd(rating),genreEffect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)

yearReviewCnt<-train_setEdx%>%mutate(group=str_sub(title,-5,-2))%>%group_by(group)%>%
  summarize(avg=mean(rating),cnt=n(),sd=sd(rating),yearEffect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)

movieAgeReviewCnt<-train_setEdx%>%mutate(group=year(as_datetime(timestamp))-as.numeric(str_sub(title,-5,-2)))%>%group_by(group)%>%
  summarize(avg=mean(rating),cnt=n(),sd=sd(rating),yearEffect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)

avgRatingChart<-function(x){
  x%>%ggplot(aes(group,avg))+
    geom_boxplot(aes(group=cut_width(group,3)))+
    scale_x_discrete(breaks=seq(min(group),max(group),5))
}

cntChart<-function(x){x%>%ggplot(aes(group,cnt,fill=avg))+
    geom_crossbar()+
    scale_fill_gradientn(colors=heat.colors(5))}

dfList=list(movieReviewCnt,userReviewCnt,genreReviewCnt,yearReviewCnt,movieAgeReviewCnt)
ratingCharts<-lapply(dfList, avgRatingChart)
#cntCharts<-lapply(dfList,cntChart)

#test avg score for each category

guessAvg<- function(x,y){
  #x - pass list of group from test set
  #y - pass training data frame grouped by tested attribute
  #y<-as.matrix(y)
  guess<-y[which(y[,1]==x),2]
  as.numeric(guess)
}

print(Sys.time())
movieModel<-data.frame(id=test_setEdx$movieId,guess=sapply(X=test_setEdx$movieId,FUN=guessAvg,y=movieReviewCnt))
movieModelRMSE<-RMSE(movieModel$guess,test_setEdx$rating)
print(Sys.time())
userModel<-data.frame(id=test_setEdx$userId,guess=sapply(test_setEdx$userId,guessAvg,y=userReviewCnt))
userModelRMSE<-RMSE(userModel$guess,test_setEdx$rating)
  
genreModel<-data.frame(id=test_setEdx$genres,guess=sapply(test_setEdx$genres,guessAvg,y=genreReviewCnt))
genreModelRMSE<-RMSE(genreModel$guess,test_setEdx$rating)

yearModel<-data.frame(id=test_setEdx$year,guess=sapply(test_setEdx$year,guessAvg,y=yearReviewCnt))
yearModelRMSE<-RMSE(yearModel$guess,test_setEdx$rating)

ageModel <-data.frame(id=test_setEdx$age,guess=sapply(test_setEdx$age,guessAvg,y=movieAgeReviewCnt))
ageModelRMSE<-RMSE(ageModel$guess,test_setEdx$rating)

#plot model RMSE
#To Do: improve plot, create plot function
rmseList<-c(naiveModelRMSE,movieModelRMSE,userModelRMSE,genreModelRMSE,yearModelRMSE,ageModelRMSE)
plot(rmseList)

#movie effect, user effect etc.
movieEffectModel <- overallAvgRating + test_setEdx%>%
  left_join(movieReviewCnt,by='group')%>%pull(movieEffect)
movieEffectModelRMSE<-RMSE(movieEffectModel,test_setEdx$rating)

