nback<-read.table("nback_summary.txt", header = TRUE)
nback<-subset(nback, block != "P" & subj != "45")
b_totals$condition<-NA
firstsessionactivesubs<-c(
  "46", "48", "50", "52", "54", "56", "58", "60", "62", "66", "68", 
  "70", "72", "74", "76", "78", "79", "82", "84", "86", "88"
)
for (rown in 1:length(nback$session)){
  subject<-nback$subj[rown]
  session<-nback$session[rown]
  if (subject %in% firstsessionactivesubs){
    if (session == "1"){
      nback$condition[rown]<-"A"
    }
    else{
      nback$condition[rown]<-"S"
    }
  }
  else{
    if (session == "1"){
      nback$condition[rown]<-"S"
    }
    else{
      nback$condition[rown]<-"A"
    }
  }
}

#remove no show session 2 subs
badsubs<-c("45", "56", "61", "67", "72", "73", "77", "80", "82", "83", "84", "85", "88", "89")
nback<-subset(nback, !(subj %in% badsubs))

nback<-subset(nback, rt > 200) 
nback$fullinter<-interaction(nback$session, nback$condition, nback$task)
nback$rt<-nback$rt/1000
colnames(nback)[7]<-"response"
write.csv(nback, "HDDM/input.csv", row.names = F)
