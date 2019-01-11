setwd("C:\\Users\\Mike\\Desktop\\School\\tdcs-wm\\nback_rawdata")

session1files = list.files(pattern = "*_001*")
#combine all of the files into one big dataframe called df1
for (file in 1:length(session1files)){ #for each file in the list of files called session1files
  if (file == 1){ #if it's the first one
    df1<- read.table(session1files[file], header = TRUE) #assign it to a dataframe called df1
  }
  else{ #if it's not the first one
    tempdf1<- read.table(session1files[file], header = TRUE) #assign it to a temporary dataframe called tempdf1
    df1<- rbind(df1,tempdf1) #combine tempdf1 and df1, make that the new df1
  }
}

session2files = list.files(pattern = "*_002*")
#combine all of the files into one big dataframe called df2
for (file in 1:length(session2files)){ #for each file in the list of files called session2files
  if (file == 1){ #if it's the first one
    df2<- read.table(session2files[file], header = TRUE) #assign it to a dataframe called df2
  }
  else{ #if it's not the first one
    tempdf2<- read.table(session2files[file], header = TRUE) #assign it to a temporary dataframe called tempdf2
    df2<- rbind(df2,tempdf2) #combine tempdf2 and df2, make that the new df2
  }
}

df1$session = 1
df2$session = 2
df = rbind(df1, df2)

test<-aggregate(difficulty ~ subj + session + task, data = subset(df, task != "P"), FUN = max)
test2<-aggregate(difficulty ~ subj + session + task, data = subset(df, task != "P"), FUN = sd)
View(test2[test2$difficulty == 0,])

baddata = test$subj[test$difficulty > 2 & test$task == "two"]

newdf = subset(df, !subj %in% baddata)
length(levels(as.factor(newdf$subj)))

accagg<- aggregate(acc ~ subj + session + task, data = newdf[newdf$task!="P",], FUN = mean)
baddata2 = accagg$subj[accagg$acc < .7]
newdf2 = subset(newdf, !subj %in% baddata2)
length(levels(as.factor(newdf2$subj)))

for (subnum in levels(as.factor(newdf2$subj))){
  if (length(newdf2$block[newdf2$subj == subnum & newdf2$session == 1]) == 0){
    newdf2 = subset(newdf2, subj != subnum)
  }
}

length(levels(as.factor(newdf2$subj)))
