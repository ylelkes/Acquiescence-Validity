## Load Data
anes2012 <- foreign::read.spss("Data/anes_timeseries_2012.sav",to.data.frame=T)
table(anes2012$effic_complicrev)
anes2012$oldnew <- NA
anes2012$oldnew[as.numeric(anes2012$effic_complicstd)!=6]='Old'
anes2012$oldnew[as.numeric(anes2012$effic_complicstd)==6]='New'
anes2012$oldnew <- as.factor(anes2012$oldnew)

##########
anes2012$efficacy1stdpre <- as.numeric(anes2012$effic_complicstd)
anes2012$efficacy1stdpre <- zero1(car::recode(anes2012$efficacy1stdpre,"1:6=NA"))
anes2012$efficacy1revpre <- as.numeric(anes2012$effic_complicrev)
anes2012$efficacy1revpre <- zero1(car::recode(anes2012$efficacy1revpre,"1:6=NA"))

anes2012$efficacy1pre <- NA 
anes2012$efficacy1pre[which(anes2012$oldnew=="Old")]=anes2012$efficacy1stdpre[which(anes2012$oldnew=="Old")]
anes2012$efficacy1pre[which(anes2012$oldnew=="New")]=anes2012$efficacy1revpre[which(anes2012$oldnew=="New")]



anes2012$efficacy2stdpre <- as.numeric(anes2012$effic_undstd)
anes2012$efficacy2stdpre <- 1-zero1(car::recode(anes2012$efficacy2stdpre,"1:6=NA"))
anes2012$efficacy2revpre <- as.numeric(anes2012$effic_undrev)
anes2012$efficacy2revpre <- 1-zero1(car::recode(anes2012$efficacy2revpre,"1:6=NA"))


anes2012$efficacy2pre <- NA 
anes2012$efficacy2pre[which(anes2012$oldnew=="Old")]=anes2012$efficacy2stdpre[which(anes2012$oldnew=="Old")]
anes2012$efficacy2pre[which(anes2012$oldnew=="New")]=anes2012$efficacy2revpre[which(anes2012$oldnew=="New")]

anes2012$efficacy3stdpre <- as.numeric(anes2012$effic_carestd)
anes2012$efficacy3stdpre <- zero1(car::recode(anes2012$efficacy3stdpre,"1:6=NA"))

anes2012$efficacy3revpre <- as.numeric(anes2012$effic_carerev)
anes2012$efficacy3revpre <- 1-zero1(car::recode(anes2012$efficacy3revpre,"1:6=NA"))

anes2012$efficacy3pre <- NA 
anes2012$efficacy3pre[which(anes2012$oldnew=="Old")]=anes2012$efficacy3stdpre[which(anes2012$oldnew=="Old")]
anes2012$efficacy3pre[which(anes2012$oldnew=="New")]=anes2012$efficacy3revpre[which(anes2012$oldnew=="New")]


anes2012$efficacy4stdpre <- as.numeric(anes2012$effic_saystd)
anes2012$efficacy4stdpre <- zero1(car::recode(anes2012$efficacy4stdpre,"1:6=NA"))
anes2012$efficacy4revpre <- as.numeric(anes2012$effic_sayrev)
anes2012$efficacy4revpre <- 1-zero1(car::recode(anes2012$efficacy4revpre,"1:6=NA"))

anes2012$efficacy4pre <- NA 
anes2012$efficacy4pre[which(anes2012$oldnew=="Old")]=anes2012$efficacy4stdpre[which(anes2012$oldnew=="Old")]
anes2012$efficacy4pre[which(anes2012$oldnew=="New")]=anes2012$efficacy4revpre[which(anes2012$oldnew=="New")]

with(anes2012,cor(data.frame(efficacy4revpre,efficacy3revpre,efficacy2revpre,efficacy1revpre),use="pairwise.complete.obs"))
with(anes2012,cor(data.frame(efficacy4stdpre,efficacy3stdpre,efficacy2stdpre,efficacy1stdpre),use="pairwise.complete.obs"))



library(stringr)
wordsum <- as.data.frame(mapply(as.character,anes2012[811:820]))
trim.leading <- function (x)  sub("^\\s+", "", x)

wordsum1 <- as.data.frame(mapply(function(x)as.factor(str_replace_all(x, "[[:digit:]].", "")),wordsum))
levels(wordsum1[,1])[1]
wordsum01 <- as.data.frame(mapply(function(x)car::recode(x,"' Correct'=1;else=0"),wordsum1))

library(ltm)
b <- ltm(wordsum01~z1)
fscores <- factor.scores(b)
newdf <- fscores$score.dat
tobind <- as.data.frame(data.matrix(wordsum01)-1)
tobind$join <- do.call(paste, c(tobind, sep = ""))
newdf$join <- do.call(paste, c(newdf[,1:10], sep = ""))
anes2012$wordsumz1 <-(newdf$z1[match(tobind$join,newdf$join)])
cor(anes2012$wordsumz1,rowMeans(tobind[,1:10]),use="pairwise.complete.obs")
range(anes2012$wordsumz1)
quantile(anes2012$wordsumz1,c(.33,.67))
anes2012$wordsumtri <- car::recode(anes2012$wordsumz1,"-5:-.43='Lower';.36:5='Upper';else=NA",as.factor=T)
table(anes2012$wordsumtri)




anes2012$oldnewpre <- NA
anes2012$oldnewpre[is.na(anes2012$efficacy1stdpre)==F]='Old'
anes2012$oldnewpre[is.na(anes2012$efficacy1revpre)==F]='New'
anes2012$oldnewpre <- as.factor(anes2012$oldnewpre)

anes2012$oldnewpost <- NA
anes2012$oldnewpost[is.na(anes2012$efficacy1stdpo)==F]='Old'
anes2012$oldnewpost[is.na(anes2012$efficpo_complicrev)==F]='New'
anes2012$oldnewpost <- as.factor(anes2012$oldnewpost)

table(anes2012$oldnewpre,anes2012$oldnewpost)
source("../func.R")
levels(anes2012$trustgov_trustgrev)
anes2012$trust1rev <- as.numeric(anes2012$trustgov_trustgrev)
anes2012$trust1rev <- zero1(car::recode(anes2012$trust1rev,"1:6=NA"))

anes2012$trust1std <- as.numeric(anes2012$trustgov_trustgstd)
anes2012$trust1std <- zero1(car::recode(anes2012$trust1std,"1:6=NA"))

anes2012$trust1 <- NA
anes2012$trust1[which(anes2012$oldnew=='Old')]=anes2012$trust1std[which(anes2012$oldnew=='Old')]
anes2012$trust1[which(anes2012$oldnew=='New')]=anes2012$trust1rev[which(anes2012$oldnew=='New')]
#########Efficacy Pre


#########



#########



## 
#########Efficacy Post
anes2012$efficacy1stdpo <- as.numeric(anes2012$efficpo_complicstd)
anes2012$efficacy1stdpo <- zero1(car::recode(anes2012$efficacy1stdpo,"1:7=NA"))
anes2012$efficacy1revpo <- as.numeric(anes2012$efficpo_complicrev)
anes2012$efficacy1revpo <- zero1(car::recode(anes2012$efficacy1revpo,"1:7=NA"))


anes2012$efficacy1po <- NA 
anes2012$efficacy1po[which(anes2012$oldnew=="Old")]=anes2012$efficacy1stdpo[which(anes2012$oldnew=="Old")]
anes2012$efficacy1po[which(anes2012$oldnew=="New")]=anes2012$efficacy1revpo[which(anes2012$oldnew=="New")]

#########

anes2012$efficacy2stdpo <- as.numeric(anes2012$efficpo_undstd)
anes2012$efficacy2stdpo <- 1-zero1(car::recode(anes2012$efficacy2stdpo,"1:7=NA"))
anes2012$efficacy2revpo <- as.numeric(anes2012$efficpo_undrev)
anes2012$efficacy2revpo <- 1-zero1(car::recode(anes2012$efficacy2revpo,"1:7=NA"))

anes2012$efficacy2po <- NA 
anes2012$efficacy2po[which(anes2012$oldnew=="Old")]=anes2012$efficacy2stdpo[which(anes2012$oldnew=="Old")]
anes2012$efficacy2po[which(anes2012$oldnew=="New")]=anes2012$efficacy2revpo[which(anes2012$oldnew=="New")]

#########

anes2012$efficacy3stdpo <- as.numeric(anes2012$efficpo_carestd)
anes2012$efficacy3stdpo <- zero1(car::recode(anes2012$efficacy3stdpo,"1:7=NA"))
anes2012$efficacy3revpo <- as.numeric(anes2012$efficpo_carerev)
anes2012$efficacy3revpo <- 1-zero1(car::recode(anes2012$efficacy3revpo,"1:7=NA"))

anes2012$efficacy3po <- NA 
anes2012$efficacy3po[which(anes2012$oldnew=="Old")]=anes2012$efficacy3stdpo[which(anes2012$oldnew=="Old")]
anes2012$efficacy3po[which(anes2012$oldnew=="New")]=anes2012$efficacy3revpo[which(anes2012$oldnew=="New")]
levels(anes2012$efficpo_sayrev)

anes2012$efficacy4stdpo <- as.numeric(anes2012$efficpo_saystd)
anes2012$efficacy4stdpo <- zero1(car::recode(anes2012$efficacy4stdpo,"1:7=NA"))
anes2012$efficacy4revpo <- as.numeric(anes2012$efficpo_sayrev)
anes2012$efficacy4revpo <- 1-zero1(car::recode(anes2012$efficacy4revpo,"1:7=NA"))

anes2012$efficacy4po <- NA 
anes2012$efficacy4po[which(anes2012$oldnew=="Old")]=anes2012$efficacy4stdpo[which(anes2012$oldnew=="Old")]
anes2012$efficacy4po[which(anes2012$oldnew=="New")]=anes2012$efficacy4revpo[which(anes2012$oldnew=="New")]

with(anes2012,cor(data.frame(efficacy4revpo,efficacy3revpo,efficacy2revpo,efficacy1revpo),use="pairwise.complete.obs"))


with(anes2012,cor(data.frame(pk,efficacy4stdpo,efficacy3stdpo,efficacy2stdpo,efficacy1stdpo),use="pairwise.complete.obs"))


with(anes2012,cor(data.frame(pk,efficacy4stdpre,efficacy3stdpre,efficacy2stdpre,efficacy1stdpre),use="pairwise.complete.obs"))

anes2012lowiq <- subset(anes2012,wordsumtri=="Lower")
anes2012highiq <- subset(anes2012,wordsumtri=="Upper")

############# Discuss Politics
anes2012$discusspolitics <- as.numeric(anes2012$discuss_discpstwk)
anes2012$discusspolitics  <- zero1(car::recode(anes2012$discusspolitics,"1:7=NA"))
anes2012$discusspolitics[as.numeric(anes2012$discuss_disc)==9]=0

############# Political Activities
a <- car::recode(as.numeric(anes2012$dhsinvolv_march),"8=1;9=0;else=NA")
b <- car::recode(as.numeric(anes2012$dhsinvolv_board),"8=1;9=0;else=NA")
c <- car::recode(as.numeric(anes2012$dhsinvolv_netpetition),"8=1;9=0;else=NA")
d <- car::recode(as.numeric(anes2012$dhsinvolv_petition),"8=1;9=0;else=NA")
e <- car::recode(as.numeric(anes2012$dhsinvolv_relig),"8=1;9=0;else=NA")
f <-  car::recode(as.numeric(anes2012$dhsinvolv_org),"8=1;9=0;else=NA")
g <-  car::recode(as.numeric(anes2012$dhsinvolv_call),"8=1;9=0;else=NA")
e <-  car::recode(as.numeric(anes2012$dhsinvolv_message),"8=1;9=0;else=NA")
f <-  car::recode(as.numeric(anes2012$dhsinvolv_letter),"8=1;9=0;else=NA")
g <-  car::recode(as.numeric(anes2012$dhsinvolv_contact1),"8=1;9=0;else=NA")
h <-  car::recode(as.numeric(anes2012$dhsinvolv_contact1),"8=1;9=0;else=NA")
anes2012$polactivity <- rowSums(data.frame(a,b,c,d,e,f,g,h))

############# Political Activities
a <- car::recode(as.numeric(anes2012$dhsinvolv_march),"8=1;9=0;else=NA")
b <- car::recode(as.numeric(anes2012$dhsinvolv_board),"8=1;9=0;else=NA")
c <- car::recode(as.numeric(anes2012$dhsinvolv_netpetition),"8=1;9=0;else=NA")
d <- car::recode(as.numeric(anes2012$dhsinvolv_petition),"8=1;9=0;else=NA")
e <- car::recode(as.numeric(anes2012$dhsinvolv_relig),"8=1;9=0;else=NA")
f <-  car::recode(as.numeric(anes2012$dhsinvolv_org),"8=1;9=0;else=NA")
g <-  car::recode(as.numeric(anes2012$dhsinvolv_call),"8=1;9=0;else=NA")
e <-  car::recode(as.numeric(anes2012$dhsinvolv_message),"8=1;9=0;else=NA")
f <-  car::recode(as.numeric(anes2012$dhsinvolv_letter),"8=1;9=0;else=NA")
g <-  car::recode(as.numeric(anes2012$dhsinvolv_contact1),"8=1;9=0;else=NA")
h <-  car::recode(as.numeric(anes2012$dhsinvolv_contact1),"8=1;9=0;else=NA")
anes2012$polactivity <- rowSums(data.frame(a,b,c,d,e,f,g,h))




anes2012$pk1 <- car::recode(as.numeric(anes2012$candrel_dpc),"7=1;else=0")
anes2012$pk2 <- car::recode(as.numeric(anes2012$candrel_rpc),"11=1;else=0")
anes2012$pk3 <- car::recode(as.numeric(anes2012$preknow_prestimes),"2=1;else=0")
anes2012$pk4 <- car::recode(as.numeric(anes2012$preknow_sizedef),"8=1;else=0")
anes2012$pk5 <- car::recode(as.numeric(anes2012$preknow_senterm),"6=1;else=0")
anes2012$pk6 <- car::recode(as.numeric(anes2012$preknow_medicare),"8=1;else=0")
anes2012$pk7 <- car::recode(as.numeric(anes2012$preknow_leastsp),"8=1;else=0")

cor(with(anes2012,data.frame(pk1,pk2,pk3,pk4,pk5,pk6,pk7)),use="pairwise.complete.obs")
###################
wordsum01 <- as.data.frame(mapply(function(x)car::recode(x,"' Correct'=1;else=0"),wordsum1))
pk01 <- with(anes2012,data.frame(pk1,pk2,pk3,pk4,pk5,pk6,pk7))
library(ltm)
b <- ltm(pk01~z1)
fscores <- factor.scores(b)
newdf <- fscores$score.dat
tobind <- pk01
tobind$join <- do.call(paste, c(tobind, sep = ""))
newdf$join <- do.call(paste, c(newdf[,1:7], sep = ""))
anes2012$pk <-zero1((newdf$z1[match(tobind$join,newdf$join)]))

pk <- rowMeans(with(anes2012,data.frame(pk1,pk2,pk3,pk4,pk5,pk6,pk7)))
cor(pk,anes2012$pk,use="pairwise.complete.obs")

anes2012$howlikelyvote <- 1-zero1(car::recode(as.numeric(anes2012$likelypct_howlikvt1),"1:6=NA"))
anes2012$howlikelypercent <- 1-zero1(car::recode(as.numeric(anes2012$likelypct_whatpct1),"-6:-1=NA"))


anes2012$agreeableness <- rowMeans(data.frame(zero1(car::recode(as.numeric(anes2012$tipi_crit),"1:7=NA")),zero1(car::recode(as.numeric(anes2012$tipi_warm),"1:7=NA"))),na.rm=T)

quantile(anes2012$agreeableness,.67,na.rm=T)
anes2012$ad <- car::recode(anes2012$agreeableness,".61:1='A';else=NA")
anes2012agree <- subset(anes2012,ad=='A')

levels(anes2012$prevote_regist_addr)
anes2012$registered <- car::recode(as.numeric(anes2012$prevote_regist_addr),"7=1;else=0",as.factor=T)


<<echo=FALSE,results='asis',warning=F,message=F>>=
  load("reliabilityanalysis.RData")
colnames(reliabilityanalysis) <- c("Agree/Disagree","Construct-Specific","Difference")
library(xtable)   
print(xtable(reliabilityanalysis,caption="Test-Retest Reliability Estimates for Standard Efficacy Item and Revised Items",align="lccc",label="tab:reliability"),caption.placement="top")
@