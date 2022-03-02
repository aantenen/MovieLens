#Clean and Analyze Data

#check if any NA
edxNA<-na.omit(edx)
identical(edxNA,edx)
#create test set 
test_index<-createDataPartition(edx$rating,times = 1,p=.2,list=FALSE)
test_setEdx<-edx[test_index]
train_setEdx<-edx[-test_index]

###TO DO check ranges of values: User, Movie, Score
###Change min max to min max score selection not SD

#check score distribution
hist(edx$rating)

#overall average
overallAvgRating<-mean(edx$rating)
naiveModelAvg<-RMSE(test_setEdx$rating,overallAvgRating)

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
yearReviewCnt<-train_setEdx%>%mutate(group=str_suba(title,-5,-2))%>%group_by(group)%>%
  summarize(avg=mean(rating),cnt=n(),sd=sd(rating),yearEffect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)
movieAgeReviewCnt<-train_setEdx%>%mutate(year=year(as_datetime(timestamp))-str_sub(title,-5,-2))%>%group_by(group)%>%
  summarize(avg=mean(rating),cnt=n(),sd=sd(rating),yearEffect=mean(rating-overallAvgRating))%>%
  mutate(min=avg-sd,max=avg+sd)

avgRatingChart<-function(x){
  x%>%ggplot(aes(group,avg,fill=cnt,ymax=max,ymin=min))+
    geom_crossbar()+
    scale_fill_gradientn(colors=heat.colors(5))
}

cntChart<-function(x){x%>%ggplot(aes(group,cnt,fill=avg))+
    geom_point()+
    scale_fill_gradientn(colors=heat.colors(5))}

dfList=list(movieReviewCnt,userReviewCnt,genreReviewCnt)
ratingCharts<-lapply(dfList, avgRatingChart)
cntCharts<-lapply(dfList,cntChart)

year<-year(as_date(train_setEdx$timestamp))
#test random guess of score based on user


userIdGuess <-function(x){
  guess<-userReviewCnt[which(userReviewCnt$group==x),2]
  as.numeric(guess)
}

guess<-data.frame(guess=sapply(test_setEdx$userId,userIdGuess))
mean(guess==test_setEdx$rating)
