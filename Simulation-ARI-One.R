set.seed(123)
library(mvtnorm)
library(optimx)
all_diff_f=list()
all_diff_r=list()
reps=100

for (kk in 1:reps){
  print(kk)
  
  now=aco=c(1,0)
  acofreq=c(25,75)
  
  total_sample=10000
  
  
  
  n=ceiling(total_sample*acofreq[1]*0.01)
  data=matrix(nrow=n,ncol=17)
  colnames(data)=c("acoqtr","year","under65","origdisabled","female","racegp","sumrisks_curyr","medicaid","nhres","esrd2","highpov","diabetes_curyr","hrr","XCHaRLSON","pracnum","povlt150","acopost")
  data=as.data.frame(data)
  
  #################################################################################################################
  now=acoqtr=c(17,18,19,21,25)
  freq=ceiling(c(23.19,5.54,26.23,24.92,20.12))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,1]=sim
  
  now=year=c(2009,2010,2011,2012,2013,2014)
  freq=ceiling(c(16.86,16.57,16.61,16.93,16.98,16.05))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,2]=sim
  
  now=under65=c(0,1)
  freq=ceiling(c(86.1,13.9))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,3]=sim
  
  now=origdisabled=c(0,1)
  freq=ceiling(c(79.44,20.56))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,4]=sim
  
  now=female=c(0,1)
  freq=ceiling(c(41.86,58.14))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,5]=sim
  
  now=racegp=c("bl","hi","ot","wt")
  freq=ceiling(c(8.27,5.49,3.99,82.25))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,6]=sim
  
  now=sumrisks_curyr=c(0,1,2,3,4,5,6,7,8,9)
  freq=ceiling(c(49.32,28.74,12.65,5.54,2.47,0.97,0.27,0.04,0,0))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,7]=sim
  
  now=medicaid=c(0,1)
  freq=ceiling(c(81.99,18.01))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,8]=sim
  
  now=nhres=c(0,1)
  freq=ceiling(c(96.45,3.55))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,9]=sim
  
  now=esrd2=c(0,1)
  freq=ceiling(c(99.47,0.53))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,10]=sim
  
  now=highpov=c("no","yes")
  freq=ceiling(c(85.43,14.57))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,11]=sim
  
  now=diabetes_curyr=c(0,1)
  freq=ceiling(c(75.64,24.36))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,12]=sim
  
  now=hrr=rep(1:5)
  freq=ceiling(rep(100/length(now),length(now)))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,13]=sim
  
  now=XCHaRLSON=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
  freq=ceiling(c(56.5,7.43,17.72,5.51,4.95,2.59,2.38,1.16,0.89,0.4,0.24,0.11,0.06,0.03,0.01,0,0,0,0,0,0,0))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,14]=sim
  
  now=pracnum=rep(1:10)
  freq=ceiling(rep(100/length(now),length(now)))
  
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,15]=sim
  
  data[,16]=sample(rep(1:100),n,replace=TRUE)
  
  now=acopost=c(0,1)
  freq=ceiling(c(61.50,38.50))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,17]=sim
  
  acodata=data
  #################################################################################################################
  now=aco=c(1,0)
  acofreq=c(25,75)
  
  
  n=total_sample-ceiling(total_sample*acofreq[1]*0.01)
  data=matrix(nrow=n,ncol=17)
  colnames(data)=c("acoqtr","year","under65","origdisabled","female","racegp","sumrisks_curyr","medicaid","nhres","esrd2","highpov","diabetes_curyr","hrr","XCHaRLSON","pracnum","povlt150","acopost")
  data=as.data.frame(data)
  
  now=acoqtr=c(29)
  freq=ceiling(c(100))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,1]=sim
  
  now=year=c(2009,2010,2011,2012,2013,2014)
  freq=ceiling(c(16.04,16.47,16.7,16.76,16.94,17.09))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,2]=sim
  
  now=under65=c(0,1)
  freq=ceiling(c(84.86,15.14))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,3]=sim
  
  now=origdisabled=c(0,1)
  freq=ceiling(c(77.44,22.56))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,4]=sim
  
  now=female=c(0,1)
  freq=ceiling(c(42.61,57.39))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,5]=sim
  
  now=racegp=c("bl","hi","ot","wt")
  freq=ceiling(c(8.94,4.8,3.51,82.75))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,6]=sim
  
  now=sumrisks_curyr=c(0,1,2,3,4,5,6,7,8,9)
  freq=ceiling(c(49.77,28.23,12.58,5.6,2.52,0.98,0.27,0.03,0,0))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,7]=sim
  
  now=medicaid=c(0,1)
  freq=ceiling(c(80.93,19.07))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,8]=sim
  
  now=nhres=c(0,1)
  freq=ceiling(c(95.75,4.25))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,9]=sim
  
  now=esrd2=c(0,1)
  freq=ceiling(c(99.45,0.55))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,10]=sim
  
  now=highpov=c("no","yes")
  freq=ceiling(c(80.59,19.41))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,11]=sim
  
  now=diabetes_curyr=c(0,1)
  
  freq=ceiling(c(76,24))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,12]=sim
  
  now=hrr=rep(1:5)
  freq=ceiling(rep(100/length(now),length(now)))
  
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,13]=sim
  
  for (t in 1:5){
    tmp=c()
    tmpid=c()
    sigma=diag(10,2)
    sigma[2,2]=2
    for (j in 1:5){
      tmp=rbind(tmp,abs(rmvnorm(300,mean = c(10+20*(j-1),2+4*(j-1)),sigma=sigma)))
    }
    tmp=cbind(tmp,as.vector(as.numeric(paste(t,0,rep(1:5,each=300),sep=""))))
    data[which(data[,"hrr"]==t),14]=tmp[,2]
    data[which(data[,"hrr"]==t),15]=tmp[,3]
    data[which(data[,"hrr"]==t),16]=tmp[,1]
  }
  
  
  now=acopost=c(0)
  freq=ceiling(c(100))
  sim=c()
  for (i in 1:length(now)){
    sim=c(sim,rep(now[i],ceiling(freq[i]*0.01*n)))
  }
  sim=sample(sim,n)
  data[,17]=sim
  
  
  nonacodata=data

  #################################################################################################################
  
  data=rbind(acodata,nonacodata)

  a1=data[,"year"]-mean(data[,"year"])
  names=c(colnames(data),"yearcnt")
  a2=c(rep(1,dim(acodata)[1]),rep(0,dim(nonacodata)[1]))
  names=c(names,"aco")
  a3=as.numeric(data[,"year"]==2010)
  names=c(names,"year2010")
  a4=as.numeric(data[,"year"]==2011)
  names=c(names,"year2011")
  a5=as.numeric(data[,"year"]==2012)
  names=c(names,"year2012")
  a6=as.numeric(data[,"year"]==2013)
  names=c(names,"year2013")
  a7=as.numeric(data[,"year"]==2014)
  names=c(names,"year2014")
  a8=as.numeric(data[,"racegp"]=="bl")
  names=c(names,"racegpbl")
  a9=as.numeric(data[,"racegp"]=="hi")
  names=c(names,"racegphi")
  a10=as.numeric(data[,"racegp"]=="ot")
  names=c(names,"racegpot")
  a11=as.numeric(data[,"highpov"]=="yes")
  names=c(names,"highpovyes")
  a12=data[,"acopost"]*a2
  names=c(names,"acomacopost")
  a13=1-a2
  names=c(names,"nonaco")
  
  data=cbind(data,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)
  colnames(data)=names
  

  Intercept=-2.5
  acoe=0.2
  acomacoposte=-0.3
  year2010e=-0.15
  year2011e=-0.25
  year2012e=-0.35
  year2013e=-0.55
  year2014e=-0.7
  under65e=0.06541
  origdisablede=0.1312
  femalee=0.2463
  racegpble=-0.03416
  racegphie=-0.03575
  racegpote=-0.1648
  sumrisks_curyre=1.2292
  medicaide=0.04264
  nhrese=-0.1635
  esrd2e=0.3673
  highpove=0.01171
  diabetes_curyre=-0.8623
  
  
  randomint_hrr=1.3
  randomint_pracnum=1.2
  random_yearcnt_hrr=0.2
  random_yearcnt_pracnum=0.5
  random_acoacopost_pracnum=0.3
  
  
  library("dplyr")
  data <- data %>%
    group_by(hrr) %>%
    mutate(RI1=rnorm(1,mean=0,sd=sqrt(randomint_hrr))) %>%
    mutate(RS1=rnorm(1,mean=0,sd=sqrt(random_yearcnt_hrr)))
  
  data <- data %>%
    group_by(pracnum) %>%
    mutate(RI2=rnorm(1,mean=0,sd=sqrt(randomint_pracnum))) %>%
    mutate(RS2=rnorm(1,mean=0,sd=sqrt(random_yearcnt_pracnum))) %>%
    mutate(RS3=rnorm(1,mean=0,sd=sqrt(random_acoacopost_pracnum)))
  
  data_r<-as.matrix(data[,c("RI1","RI2","RS1","RS2","RS3","yearcnt","acomacopost")])
  data_r[,"RI1"]=as.numeric(data_r[,"RI1"])
  data_r[,"RI2"]=as.numeric(data_r[,"RI2"])
  data_r[,"RS1"]=as.numeric(data_r[,"RS1"])
  data_r[,"RS2"]=as.numeric(data_r[,"RS2"])
  data_r[,"RS3"]=as.numeric(data_r[,"RS3"])
  data_r[,"yearcnt"]=as.numeric(data_r[,"yearcnt"])
  data_r[,"acomacopost"]=as.numeric(data_r[,"acomacopost"])

  random=data_r[,"RI1"]+data_r[,"RI2"]+data_r[,"RS1"]*data_r[,"yearcnt"]+data_r[,"RS2"]*data_r[,"yearcnt"]+data_r[,"RS3"]*data_r[,"acomacopost"]
  
  xe=as.matrix(as.numeric(c(acoe,acomacoposte,year2010e,year2011e,year2012e,year2013e,year2014e,under65e,origdisablede,femalee,racegpble,racegphie,racegpote,sumrisks_curyre,medicaide,nhrese,esrd2e,highpove,diabetes_curyre)))
  xdata=as.matrix(data[,c("aco","acomacopost","year2010","year2011","year2012","year2013","year2014","under65","origdisabled","female","racegpbl","racegphi","racegpot","sumrisks_curyr","medicaid","nhres","esrd2","highpovyes","diabetes_curyr")])

  fixed=xdata%*%xe+Intercept

  prob=1/(1+exp(-(fixed+random)))

  y=c()
  for (i in 1:length(prob)){
    y=c(y,rbinom(1,1,prob[i]))
  }

  class(data)
  data=as.data.frame(data)
  data=cbind(data,y)
  data[,"sumrisks_curyr"]=scale(data[,"sumrisks_curyr"],center=TRUE,scale=TRUE)
  #################################################################################################################
  data[which(data[,"aco"]==0),"pracnum"]=0
  #################################################################################################################
  library("lme4")
  
  fit <- glmer(y ~ aco+acomacopost+year2010+year2011+year2012+year2013+year2014+under65+origdisabled+female+racegpbl+racegphi+racegpot+sumrisks_curyr+medicaid+nhres+esrd2+highpovyes+diabetes_curyr+
                 (1|hrr)+(0+yearcnt|hrr) +(1|pracnum)+(0+yearcnt|pracnum)+(0+acomacopost|pracnum),
               family = binomial, data = data, control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
  xe=c(Intercept,xe)
  diff_f=xe-coef(summary(fit))[ , "Estimate"]

  re=c(randomint_hrr,randomint_pracnum,random_yearcnt_hrr,random_yearcnt_pracnum,random_acoacopost_pracnum)
  t=as.data.frame(VarCorr(fit))
  res=c(t[which(t[,2]=="(Intercept)" & t[,1]=="hrr.1"),"vcov"],t[which(t[,2]=="(Intercept)" & t[,1]=="pracnum.2"),"vcov"],t[which(t[,2]=="yearcnt" & t[,1]=="hrr"),"vcov"],t[which(t[,2]=="yearcnt" & t[,1]=="pracnum.1"),"vcov"],t[which(t[,2]=="acomacopost" & t[,1]=="pracnum"),"vcov"])
  diff_r=re-res

  all_diff_f[[kk]]=diff_f
  all_diff_r[[kk]]=diff_r
  
}
print(all_diff_f)
print(all_diff_r)
#################################################################################################################
m_f<-c()
for (j in 1:length(all_diff_f[[1]])){
  tmp=0
  for (i in 1:length(all_diff_f)){
    tmp=tmp+all_diff_f[[i]][j]
  }
  m_f<-c(m_f,tmp/length(all_diff_f))
}
print(m_f)

m_r<-c()
for (j in 1:length(all_diff_r[[1]])){
  tmp=0
  for (i in 1:length(all_diff_r)){
    tmp=tmp+all_diff_r[[i]][j]
  }
  m_r<-c(m_r,tmp/length(all_diff_r))
}
print(m_r)
#################################################################################################################