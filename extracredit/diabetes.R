diabetes <- read.delim("~/Downloads/diabetes.txt")
summary(diabetes)
library(sampling)
set.seed(1)
#question a
#Simple random sampling without replacement
s=srswor(300,length(diabetes$id))
ss<-data.frame(diabetes,s)
ss$location
write.csv(ss, "~/Desktop/ss.csv")

#ss1 is training set
ss1<-ss[s==1,]
#ss2 is testing set
ss0<-ss[s==0,]

#question b
summary(ss1)
summary(ss0)
summary(ss)
write.csv(summary(ss1), "~/Desktop/summaryss1.csv")
write.csv(summary(ss0), "~/Desktop/summaryss0.csv")
write.csv(summary(ss), "~/Desktop/summaryss.csv")

#questionc c
#center chol as cchol
mchol<-rep(mean(ss1$chol,na.rm=T),length(ss1$id))
cchol<-ss1$chol-mchol
cchol

#center hdl as chdl
mhdl<-rep(mean(ss1$hdl,na.rm=T),length(ss1$id))
chdl<-ss1$hdl-mhdl


#center ratio as cratio
mratio<-rep(mean(ss1$ratio,na.rm=T),length(ss1$id))
cratio<-ss1$ratio-mratio


#center age as cage
mage<-rep(mean(ss1$age,na.rm=T),length(ss1$id))
cage<-ss1$age-mage


#center height
mheight<-rep(mean(ss1$height,na.rm=T),length(ss1$id))
cheight<-ss1$height-mheight


#center weight
mweight<-rep(mean(ss1$weight,na.rm=T),length(ss1$id))
cweight<-ss1$weight-mweight
cweight<-ss1$weight-mweight
  

#center bp.1s
mbp.1s<-rep(mean(ss1$bp.1s,na.rm=T),length(ss1$id))
cbp.1s<-ss1$bp.1s-mbp.1s
cbp.1s

#center bp.1d
mbp.1d<-rep(mean(ss1$bp.1d,na.rm=T),length(ss1$id))
cbp.1d<-ss1$bp.1d-mbp.1d
cbp.1d

#recode gender 0=female, 1=male
ngender<-rep(NA,300)
ngender<-ifelse(ss1$gender=="female",ngender<-0,ngender<-1)
ngender
#recode location 0=Buckingham, 1=Louisa

nlocation<-rep(NA,300) 
nlocation<-ifelse(ss1$location=="Buckingham",nlocation<-0,nlocation<-1)
nlocation
#recode frame
# Frame  frame1  frame2
# large   1       0 
# small   0       1
# medium  0       0
frame1<-rep(NA,300)
frame2<-rep(NA,300)
for (i in 1:300) {
  if (ss1$frame[i]=="large") {
    frame1[i]<-1 
    frame2[i]<-0
  } 
  else if (ss1$frame[i]=="medium") {
    frame1[i]<-0
    frame2[i]<-1
  }
  else if (ss1$frame[i]=="small") {
    frame1[i]<-0
    frame2[i]<-0
  }
}

#quadratic 
attach(ss1)
age2<-age^2;bp.1s2<-bp.1s^2;bp.1d2<-bp.1d^2;chol2<-chol^2;hdl2<-hdl^2;height2<-height^2;weight2<-weight^2;ratio2=ratio^2
  
  
  
  #new data set with only interesed variables
glu<-ss1$stab.glu
ss1new<-data.frame(glu,cage,cbp.1d,cbp.1s,cchol,chdl,cheight,cratio,cweight,ngender,frame1,frame2)
  
  
  #model
Lfull <- lm(stab.glu ~ 
#continuous
cage + cbp.1d +cbp.1s + cchol + chdl + cheight + cratio + cweight + 
#quadratic terms
age2 + bp.1d2 +bp.1s2 + chol2 + hdl2 + height2 + ratio2 + weight2 + 
#categorical
frame1 + frame2 + ngender +
#interactions
(cage + cbp.1d +cbp.1s + cchol + chdl + cheight + ngender + cweight + cratio) * (frame1 + frame2) +
(cage + cbp.1d +cbp.1s + cchol + chdl + cheight + cratio + cweight) * ngender +
(cage + cbp.1d +cbp.1s + cchol + chdl + cheight + cratio ) * cweight +
(cage + cbp.1d +cbp.1s + cchol + chdl + cheight  ) * cratio + 
(cage + cbp.1d +cbp.1s + cchol + chdl  ) * cheight + 
(cage + cbp.1d +cbp.1s + cchol) * chdl + 
(cage + cbp.1d +cbp.1s) * cchol + 
(cage + cbp.1d ) * cbp.1s + 
(cage) * (cbp.1d)
#interaction quadratic with frame
,data=ss1)
Lfull
summary(Lfull)
anova(Lfull)
forward <- stepAIC(Lfull, direction="forward")
backward<- stepAIC(Lfull, direction="backward")
forward
summary(backward)
plot(forward)
backward$anova
library(leaps)
regsubsets(stab.glu ~ 
             #continuous
             cage + cbp.1d +cbp.1s + cchol + chdl + cheight + cratio + cweight + 
             #quadratic terms
             age2 + bp.1d2 +bp.1s2 + chol2 + hdl2 + height2 + ratio2 + weight2 + 
             #categorical
             frame1 + frame2 + ngender +
             #interactions
             (cage + cbp.1d +cbp.1s + cchol + chdl + cheight + ngender + cweight + cratio) * (frame1 + frame2) +
             (cage + cbp.1d +cbp.1s + cchol + chdl + cheight + cratio + cweight) * ngender +
             (cage + cbp.1d +cbp.1s + cchol + chdl + cheight + cratio ) * cweight +
             (cage + cbp.1d +cbp.1s + cchol + chdl + cheight  ) * cratio + 
             (cage + cbp.1d +cbp.1s + cchol + chdl  ) * cheight + 
             (cage + cbp.1d +cbp.1s + cchol) * chdl + 
             (cage + cbp.1d +cbp.1s) * cchol + 
             (cage + cbp.1d ) * cbp.1s + 
             (cage) * (cbp.1d)
           #interaction quadratic with frame
           ,data=ss1,really.big=T)

?regsubsets
hist(diabetes$stab.glu)
lnglu<-log(diabetes$stab.glu)
hist(lnglu)
sqglu<-sqrt(diabetes$stab.glu)
cbglu<-diabetes$stab.glu^(1/3)
shapiro.test(cbglu)
hist(cbglu)
