################################################################################
# This is part of Kaggle's Data Science Bowl 2019.  The goal was to "Uncover   #
# new insights in early childhood education and how media can support learning #
# outcomes."  This script includes data wrangling, fitting using Random Forest,#
# and validation.  Due to time constraints, this code was never finished nor   #
# were the results ever submitted to the competition.                          #
################################################################################


library(tidyverse)
library(lubridate)
library(ggExtra)

# begin <- Sys.time()
train <- read.csv("train.csv")
train$set <- "train"

test <- read.csv("test.csv")
test$set <- "test"

# Combine train and test data, then remove initial files for space
train_test <- rbind(train,test)
rm(train)
rm(test)



# Data Preprocessing ------------------------------------------------------

# Data wrangling.  Data is quite messy, so we need to make substantial changes

# First let's get rid of users who never took an assessment
df <- train_test %>%
  mutate(assessment=ifelse((title=="Bird Measurer (Assessment)" & event_code==4110) |
                             (title!="Bird Measurer (Assessment)" & event_code==4100),1,0)) %>%
  group_by(installation_id) %>%
  mutate(assessment_count=sum(assessment)) %>%
  ungroup() %>%
  filter(assessment_count>0)

df$event_data <- as.character(df$event_data)

# Now we want to convert correct assessment scores into the Kaggle assessment
# score, which is a 3 for correct answer on first attempt, 2 for correct answer
# on second attempt, 1 for correct answer on 3rd or more attempt, and 0 for 
# never got correct answer
get_score <- function(x){
  ind <- gregexpr('correct',x)[[1]][1]
  if(str_sub(x,ind+9,ind+9)=='f'){
    return(0)
  } else{
    return(1)
  }
}

score_vector <- apply(df,1,function(x){
  if(grepl('correct',x[4]) & (x[7]==4100 & x[9]!="Bird Measurer (Assessment)") | (x[7]==4110 & x[9]=="Bird Measurer (Assessment)")){
    return(get_score(x[4]))
  } else{
    return(NA)
  }
})

df$correct <- score_vector
df$timestamp <- as.character(df$timestamp)

df$timestamp <- paste0(str_sub(df$timestamp,1,10),
                       ' ',
                       str_sub(df$timestamp,12,19))

df <- df %>%
  group_by(game_session) %>%
  mutate(num_correct = sum(correct,na.rm=T),
         num_incorrect = sum(correct==0,na.rm=T)) %>%
  mutate(accuracy_group = ifelse(num_correct==0 & num_incorrect==0,NA,ifelse(num_correct==0,0,ifelse(num_incorrect==0,3,ifelse(num_incorrect==1,2,1))))) %>%
  ungroup()
  

# Now we want to summarize some characteristics of each individual user.  We'll
# do this by calculating the mode for factor or charachter variables.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv <- uniqv[!is.na(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Convert "timestamp" column into more useful format, what day of week users
# most ofetn play on and what time of the day
df$day_of_week <- as.Date(str_sub(df$timestamp,1,10))
df$day_of_week <- format(df$day_of_week,"%a")
df$time_of_day <- as.numeric(str_sub(df$timestamp,12,13))
df$time_of_day <- cut(df$time_of_day,breaks=c(0,5,11,16,20,24),
                      labels=c('Night','Morning','Afternoon',
                               'Evening','Night'))

# Now we'll start sumamrizing variables
# First lets see how often each event code comes up per player.  This is a bit
# messy...
df1 <- df %>%
  select(game_session,installation_id,event_code,accuracy_group,
         timestamp,game_time,title,type,world,set,day_of_week,
         time_of_day) %>%
  group_by(installation_id) %>%
  mutate(accuracy_mode=getmode(accuracy_group),
         ec2000=length(which(event_code==2000)),
         ec3010=length(which(event_code==3010)),
         ec4070=length(which(event_code==4070)),
         ec3020=length(which(event_code==3020)),
         ec3120=length(which(event_code==3120)),
         ec2025=length(which(event_code==2025)),
         ec4025=length(which(event_code==4025)),
         ec4040=length(which(event_code==4040)),
         ec4100=length(which(event_code==4100)),
         ec2010=length(which(event_code==2010)),
         ec4110=length(which(event_code==4110)),
         day_of_week_mode=getmode(day_of_week),
         time_of_day_mode=getmode(time_of_day)) %>%
  summarise_all(getmode)
df1 <- df1[,-c(2,3,4,5,6,7,8,9,10,11,12)]

# Next we'll see how long on average each user plays
df2 <- df %>%
  select(game_session,installation_id,game_time) %>%
  group_by(game_session) %>%
  mutate(session_length=max(game_time)) %>%
  ungroup() %>%
  group_by(installation_id) %>%
  summarise_all(mean)
df2 <- df2[,-c(2,3)]

# Then how much time passes in between playing the game
df3 <- df %>% select(installation_id, timestamp, set) %>%
  arrange(installation_id, timestamp) %>% 
  mutate(timestamp = as_date(timestamp)) %>% 
  group_by(installation_id) %>% 
  mutate(initial_date = first(timestamp), final_date = last(timestamp)) %>% 
  ungroup() %>%
  select(installation_id, initial_date, final_date, set) %>% 
  distinct() 

df3 <- df3 %>%
  mutate(days=final_date - initial_date) %>%
  mutate(days = ifelse(days==0,0,ifelse(days>0&days<7,1,ifelse(days>6&days<31,2,3)))) %>%
  mutate(days = factor(days,labels = c("one-day","less-than-a-week","less-than-a-month","more-than-a-month")))
df3 <- df3[,-c(2,3)]

# Then just clean up by installation ID
df4 <- df %>%
  select(installation_id,title,type,world) %>%
  group_by(installation_id) %>%
  summarise_all(getmode)

# And finally join them all together
df_all <- df1 %>%
  left_join(df2,by='installation_id') %>%
  left_join(df3,by='installation_id') %>%
  left_join(df4,by='installation_id')

# Now, to use Random Forest we need to convert all numeric values to categories.
# For simplicity, we'll simply put them into 1/3 quantiles of low, normal,
# and high
ind=list()
k=1

for(i in 3:13){
  test <- as.matrix(df_all[,i])
  myquant <- quantile(as.numeric(test),
                      probs=c(0,0.33,0.66,1))
  if(length(unique(myquant))!=4){
    ind[[k]] <- i
    k=k+1
    next
  }
  test2 <- cut(test,myquant,
               labels=c('Low','Normal',
                        'High'))
  test2[which(is.na(test2))] <- 'Low'
  df_all[,i] <- test2
}

df_all <- df_all[,-c(unlist(ind))]


df_ids <- df_all$installation_id
df_all <- df_all[,-1]
df_all$accuracy_mode <- as.factor(df_all$accuracy_mode)
myquant <- quantile(as.numeric(df_all$session_length),
                    probs=c(0,0.33,0.66,1))
df_all$session_length <- cut(df_all$session_length,
                             myquant,
                             labels=c('Low','Normal',
                                      'High'))
df_all$session_length[which(is.na(df_all$session_length))] <- 'Low'
# print(Sys.time()-begin)


# Model Fitting -----------------------------------------------------------


# Now fit a model with Random Forest.  
# Fit with train data
library(randomForest)
df_all_train <- df_all %>%
  filter(set=='train',
         !is.na(time_of_day_mode)) %>%
  select(-set) %>% 
  mutate_if(is.character, as.factor)

rf <- randomForest(accuracy_mode ~ .,data=df_all_train,
                   importance=TRUE)

# And predict with test data
df_all_test <- df_all %>%
  filter(set=='test') %>%
  select(-set) %>% 
  mutate_if(is.character, as.factor)
pred <- predict(rf,df_all_test)


