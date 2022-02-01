#Check for missing data
edxNA<-edx%>%na.omit()
identical(edxNA,edx) ##No Missing Data, returns true


#investigate general review data
hist(edx$rating)

#investigate how number of reviews for movie affects score
moviereviewcnt <- edx%>%group_by(movieId,title)%>%summarize(cnt=n(),avg=mean(rating))
plot(moviereviewcnt$avg,moviereviewcnt$cnt)


moviereviewcnt%>%arrange(desc(avg))
