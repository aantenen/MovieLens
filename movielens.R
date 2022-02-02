#Clean and Analyze Data

#check if any NA
edxNA<-na.omit(edx)
identical(edxNA,edx)

###TO DO check ranges of values: User, Movie, Score
###Change min max to min max score selection not SD

#check score distribution
hist(edx$rating)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

movieReviewCnt <-edx%>%mutate(group=movieId)%>%group_by(group,title)%>%summarise(avg=getmode(rating),cnt=n(),sd=sd(rating))%>%mutate(min=avg-sd,max=avg+sd)
movieReviewCnt%>%ggplot(aes(avg,cnt))+
  geom_point()

userReviewCnt <-edx%>%mutate(group=userId)%>%group_by(group)%>%summarize(avg=getmode(rating),cnt=n(),sd=sd(rating))%>%mutate(min=avg-sd,max=avg+sd)

userReviewCnt%>%ggplot(aes(group,cnt,color=avg))+
  geom_col()+
  scale_color_gradient(low = 'red',high = 'blue')

individualGenreReviewCnt<-edx%>%mutate(group=str_split(genres,"\\|"))%>%
  unnest(group)%>%group_by(group)%>%summarize(avg=getmode(rating),cnt=n(),sd=sd(rating))%>%mutate(min=avg-sd,max=avg+sd)

movieReviewCnt%>%ggplot(aes(group,avg,color=cnt))+
  geom_boxplot(y_min=avg-sd,y_max=avg+sd)
scale_fill_gradientn(colors=heat.colors(5))

avgRatingChart<-function(x){
  x%>%ggplot(aes(group,avg,fill=cnt,ymax=max,ymin=min))+
    geom_crossbar()+
    scale_fill_gradientn(colors=heat.colors(5))
}

cntChart<-function(x){x%>%ggplot(aes(group,cnt,fill=avg))+
    geom_point()+
    scale_fill_gradientn(colors=heat.colors(5))}

#overall average
overallAvgRating<-mean(edx$rating)



#test random guess of score based on user
test_index<-createDataPartition(edx$rating,times = 1,p=.2,list=FALSE)
test_setEdx<-edx[test_index]
train_setEdx<-edx[-test_index]

userIdGuess <-function(x){
  guess<-userReviewCnt[which(userReviewCnt$group==x),2]
  as.numeric(guess)
}

guess<-data.frame(guess=sapply(test_setEdx$userId,userIdGuess))
mean(guess==test_setEdx$rating)
