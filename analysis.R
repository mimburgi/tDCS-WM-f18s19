nback<-read.table("nback_summary.txt", header = TRUE)
ospan<-read.table("wm_summary.txt", header = TRUE)
survey<-read.table("surveytotals.txt", header = T)

nback<-subset(nback, task !="P" & subject != "45")
nback$subj<-as.factor(nback$subj)
nback$session<-as.factor(nback$session)
nb_totals<-aggregate(difficulty ~ session + subj + task,
                     data = nback,
                     FUN = mean)
nb_totals$stim<-NA
firstsessionactivesubs<-c(
  "46", "48", "50", "52", "54", "56", "58", "60", "62", "66", "68", 
  "70", "72", "74", "76", "78", "79", "82", "84", "86", "88"
)
for (rown in 1:length(nb_totals$session)){
  subject<-nb_totals$subj[rown]
  session<-nb_totals$session[rown]
  if (subject %in% firstsessionactivesubs){
    if (session == "1"){
      nb_totals$stim[rown]<-"A"
    }
    else{
      nb_totals$stim[rown]<-"S"
    }
  }
  else{
    if (session == "1"){
      nb_totals$stim[rown]<-"S"
    }
    else{
      nb_totals$stim[rown]<-"A"
    }
  }
}
for (subject in levels(nb_totals$subj)){
  nb_totals$ospan[nb_totals$subj == subject]<-ospan$wmc[ospan$subj == subject]
  nb_totals$bdi[nb_totals$subj == subject]<-survey$bdi[survey$subj == subject]
  nb_totals$rrs[nb_totals$subj == subject]<-survey$rrs[survey$subj == subject]
  nb_totals$age[nb_totals$subj == subject]<-survey$age[survey$subj == subject]
  nb_totals$gender[nb_totals$subj == subject]<-survey$gender[survey$subj == subject]
}

ospans<-aggregate(ospan ~ subj, data = nb_totals, FUN = mean)
bdis<-aggregate(bdi ~ subj, data = nb_totals, FUN = mean)
rrss<-aggregate(rrs ~ subj, data = nb_totals, FUN = mean)

nb_totals$ospangroup[nb_totals$ospan < 3]<-"L"
nb_totals$ospangroup[nb_totals$ospan > 2]<-"H"
nb_totals$ospangroup<-as.factor(nb_totals$ospangroup)

nb_totals$bdigroup[nb_totals$bdi < median(bdis$bdi)]<-"L"
nb_totals$bdigroup[nb_totals$bdi > median(bdis$bdi)]<-"H"
nb_totals$bdigroup<-as.factor(nb_totals$bdigroup)

nb_totals$rrsgroup[nb_totals$rrs < median(rrss$rrs)]<-"L"
nb_totals$rrsgroup[nb_totals$rrs > median(rrss$rrs)]<-"H"

#remove no show session 2 subs
badsubs<-c()
for (subject in levels(nb_totals$subj)){
 if (mean(as.numeric(nb_totals$session[nb_totals$subj == subject])) != 1.5){
   badsubs<-c(badsubs, subject)
 }
}

#nb_totals<-subset(nb_totals, !(subj %in% badsubs))
nb_totals$stim<-as.factor(nb_totals$stim)

s1<-subset(nb_totals, session == "1")
summary(aov(difficulty ~ stim*ospan*task*bdi*rrs, data = nb_totals))
summary(aov(difficulty ~ stim*ospan*task*bdi*rrs, data = subset(nb_totals, task == "adaptive")))
summary(aov(difficulty ~ stim*ospan*task*bdi*rrs, data = subset(nb_totals, task == "two")))
library(lme4)
library(lmerTest)
s1$task<-as.factor(s1$task)
s1$stim<-as.factor(s1$stim)
s1$bdi<-scale(s1$bdi)
s1$rrs<-scale(s1$rrs)
s1$ospan<-scale(s1$ospan)
s1$ospangroup<-as.factor(s1$ospangroup)

fullmod<-lmer(difficulty ~ stim*task*log(bdi)*log(rrs)*ospangroup + (1|subj), data=s1)
anova(fullmod)

summary(aov(difficulty ~ stim*task, data = subset(s1, ospangroup == "H")))
aggregate(difficulty ~ stim + task + ospangroup, data = s1, FUN = mean)

kruskal.test(difficulty ~ stim + ospangroup + task, data = s1)

#surveys

