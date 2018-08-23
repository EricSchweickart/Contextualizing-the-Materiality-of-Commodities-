#This is the code describing the methodology used to analyze data collected from
#flat-stamped copper-alloy buttons by Eric Schweickart, it was written for R version 3.1.1 
#To run this code:
  #Download the "bdat.csv" file and load it into R
  #Install/Load the packages: "car", "fBasics", "rstan", "shinystan",and "QuantPsyc"
  #Download the files: "dirichlet.stan","MeanPartialPool.stan","completePooling.stan", and "PartialPooling.stan", place them into a single directory and set it as the working directory


#This section contains the code for the "Market-Level Analysis" section of the paper

#The histigram showing the three modes of the button diameter data
hist(bdat$`Average Diameter`, breaks = 30) 

#Chi-Squared tests 

#This test compares the counts of small unflawed, minorly flawed, moderately flawed, and majorly flawed buttons to the counts of large buttons of each flaw significance 
Chi_T1<-c(sum(subset(bdat, Size == 1, select = 6)==0),sum(subset(bdat, Size == 1, select = 6)==1),sum(subset(bdat, Size == 1, select = 6)==2),sum(subset(bdat, Size == 1, select = 6)==3))
Chi_T2<-c(sum(subset(bdat, Size == 2, select = 6)==0),sum(subset(bdat, Size == 2, select = 6)==1),sum(subset(bdat, Size == 2, select = 6)==2),sum(subset(bdat, Size == 2, select = 6)==3))
Chi_T<-rbind(Chi_T1,Chi_T2) #contingency table (Table 1a)
ResChiT<-chisq.test(Chi_T) #p value

#This test compares the counts of unbackstamped unflawed, minorly flawed, moderately flawed, and majorly flawed buttons to the counts of backstamped buttons of each flaw significance 
Chi_S0<-c(sum(subset(bdat, Backstamped == 0, select = 6)==0),sum(subset(bdat, Backstamped == 0, select = 6)==1),sum(subset(bdat, Backstamped == 0, select = 6)==2),sum(subset(bdat, Backstamped == 0, select = 6)==3))
Chi_S1<-c(sum(subset(bdat, Backstamped == 1, select = 6)==0),sum(subset(bdat, Backstamped == 1, select = 6)==1),sum(subset(bdat, Backstamped == 1, select = 6)==2),sum(subset(bdat, Backstamped == 1, select = 6)==3))
Chi_S<-rbind(Chi_S0,Chi_S1) #contingency table (Table 1b)
ResChiS<-chisq.test(Chi_S) #p value


#This test compares the counts of small unbackstamped and backstamped buttons to the counts of large unbackstamped and backstamped buttons
Chi_TS1<-c(sum(subset(bdat, Size == 1, select = 7)==0),sum(subset(bdat, Size == 1, select = 7)==1))
Chi_TS2<-c(sum(subset(bdat, Size == 2, select = 7)==0),sum(subset(bdat, Size == 2, select = 7)==1))
Chi_TS<-rbind(Chi_TS1,Chi_TS2) #contingency table
ResChiTS<-chisq.test(Chi_TS) #p value


#Generalized Linear Models
#First I divide the dataset into small buttons and large buttons 
sdiam<-subset(bdat, Size == 1, select = c(4:8))
ldiam<-subset(bdat, Size == 2, select = c(4:8))

#Then, to examine the relationship between button diameter and flaw significance, I redefined flaw significance as a binomial factor (either unflawed or flawed)
newFlaw<-sdiam$`Flaw Significance`
newF<-replace(newFlaw, newFlaw < 1 , 0)
newFs<-replace(newF, newF > 0 , 1)

lnewFlaw<-ldiam$`Flaw Significance`
lnewF<-replace(lnewFlaw, lnewFlaw < 1 , 0)
newFl<-replace(lnewF, lnewF > 0 , 1)

#Then I regress small button diameter against flaw presence using a generlized linear model and treating flaws as a binomial factor
DsFmod<-glm(newFs ~ sdiam$`Average Diameter`, family= "binomial")
summary(DsFmod) #the non-significant slope beta estimate suggests that the diameter of small buttons does not significantly effect the likelyhood that they were flawed

#Finally I regress large button diameter against flaw presence using a generlized linear model and treating flaws as a binomial factor
DlFmod<-glm(newFl ~ ldiam$`Average Diameter`, family= "binomial")
summary(DlFmod) #the non-significant slope beta estimate suggests that the diameter of large buttons does not significantly effect the likelyhood that they were flawed

#Backstamp presence/absence
#Then I regress small button diameter against backstamping using a generlized linear model and treating backstamping as a binomial factor
DsSmod<-glm(sdiam$Backstamped ~ sdiam$`Average Diameter`, family= "binomial")
summary(DsSmod) #the significant positive slope beta estimate suggests that small buttons with larger diameters are significantly more likely to be backstamped 

#Finally, I regress large button diameter against backstamping using a generlized linear model and treating backstamping as a binomial factor
DlSmod<-glm(ldiam$Backstamped ~ ldiam$`Average Diameter`, family= "binomial")
summary(DlSmod) #the non-significant slope beta estimate suggests that the diameter of large buttons does not significantly effect the likelyhood that they were backstamped


#This section contains the code for the "Site-Level Analysis" section of the paper


#First I created vectors containing the site #, mean ceramic date, freedom level
#(1 = enslaved, 2 = free) and property affiliation (1 = Monticello, 2 =
#Montpelier, 3 = Poplar Forest, 4 = Liberty Hall) of each site
site<-c(1,2,3,4,5,6,7,8,9,10)
MCD<-c(1803,1811.4,1806.3, 1810.1, 1820.5,1793, 1820.4, 1801.8, 1798,1796.2)
free<-c(1,1,2,1,1,1,2,2,1,1)
prop<-c(2,2,2,2,3,3,4,4,1,2)

#Then I extracted data from each variable by site
#I first extracted manufacturing flaw significance data by site
#I created 4 blank vectors for small buttons with:
Fs_N<-c(rep(0,10)) #the total number of small buttons at each site
Fs_0<-c(rep(0,10)) #the number of small unflawed buttons at each site
Fs_1<-c(rep(0,10)) #the number of samll minorly flawed buttons at each site
Fs_2<-c(rep(0,10)) #the number of samll moderately flawed buttons at each site
Fs_3<-c(rep(0,10)) #the number of samll majorly flawed buttons at each site
#I created 4 blank vectors for large buttons with:
Fl_N<-c(rep(0,10)) #the total number of large buttons at each site
Fl_0<-c(rep(0,10)) #the number of large unflawed buttons at each site
Fl_1<-c(rep(0,10)) #the number of large minorly flawed buttons at each site
Fl_2<-c(rep(0,10)) #the number of large moderately flawed buttons at each site
Fl_3<-c(rep(0,10)) #the number of large majorly flawed buttons at each site

#then I made a for loop to fill in each of the blank vectors 
for (i in 1:10){
  col<-6
  site_dat<-subset(bdat, Site == i, select = 4:7)
  Fs_N[i]<-sum(subset(site_dat, Size == 1, select = (col-3)) >= 0)
  Fs_0[i]<-sum(subset(site_dat, Size == 1, select = (col-3)) == 0)
  Fs_1[i]<-sum(subset(site_dat, Size == 1, select = (col-3)) == 1)
  Fs_2[i]<-sum(subset(site_dat, Size == 1, select = (col-3)) == 2)
  Fs_3[i]<-sum(subset(site_dat, Size == 1, select = (col-3)) == 3)
  Fl_N[i]<-sum(subset(site_dat, Size == 2, select = (col-3)) >= 0)
  Fl_0[i]<-sum(subset(site_dat, Size == 2, select = (col-3)) == 0)
  Fl_1[i]<-sum(subset(site_dat, Size == 2, select = (col-3)) == 1)
  Fl_2[i]<-sum(subset(site_dat, Size == 2, select = (col-3)) == 2)
  Fl_3[i]<-sum(subset(site_dat, Size == 2, select = (col-3)) == 3)
}
#Finally, I created two matricies (one for small buttons, one for large buttons) with each site's buttons divided by flaw significance
Fs_all<-cbind(Fs_0,Fs_1,Fs_2,Fs_3)
Fl_all<-cbind(Fl_0,Fl_1,Fl_2,Fl_3)

#I then extracted face diameter by site
#I created a two matricies (one for small buttons one for large buttons) containing diameter of each button and the site number it was found at
Ds_all<-subset(bdat, Size == 1, select = 3:4)
Dl_all<-subset(bdat, Size == 2, select = 3:4)

#Finally, I extracted backstamp and small/large button ratio data by site
#I created 1 blank vector for small buttons with:
Ss_B<-c(rep(0,10)) #The number of small buttons with backstamps at each site
#I created 1 blank vector for large buttons with:
Sl_B<-c(rep(0,10)) #The number of large buttons with backstamps at each site
#I created 1 blank vector with:
T_N<-c(rep(0,10)) #The total number of buttons at each site

#then I made a for loop to fill in each of the blank vectors 
for (i in 1:10){
  site_dat<-subset(bdat, Site == i, select = 4:7)
  Ss_B[i]<-sum(subset(site_dat, Size == 1, select = 4) == 1)
  Sl_B[i]<-sum(subset(site_dat, Size == 2, select = 4) == 1)
  T_N[i]<-sum(site_dat$Size >= 0)
}


#In order to reduce the effect of sites with small sample sizes (since button
#assemblages had to be divided into small and large buttons) I ran all of the
#site-level data through a beysian model with the market-level variation as a
#prior and the buttons from each site as data. The effect of this calculation is
#to shift the postierior distribution of sites with small datasets closer to the
#market-level distribtuion, without strongly effecting the distributions of
#datasets with more information.

#First I performed a beysian analysis of manufacturing flaw significance as a multinomial distribution
#I pulled the data from the matrix of small buttons divided by site and manufacturing flaw signficance
K<-length(Fs_all[1,]) #Number of manufacturing flaw catagories
N<-length(Fs_all[,1]) #Number of sites
y<-Fs_all #data matrix
alpha<-apply(Fs_all, 2, sum)/sum(Fs_all) #Normalized prior (percent of each manufacturing flaw catagory in total data set)
logsigma<-0.5 #variance parameter for prior
Fsdat<-list(K=K, N=N,y=y, alpha=alpha,logsigma=logsigma) 

Fsmod<-stan(file="dirichlet.stan", data=Fsdat, chains=4, iter=2000) #run model using "dirichlet.stan" code, 
launch_shinystan(Fsmod) #to check the results of the analysis. this code as written usually throws an error message saying that there were ~5 divergent transitions, this is due to the fat tail of the log normal distribution used as a prior for the variance of the dirichlet distribution due to the step size algorithum stan uses. The larger the shape parameter of the log normal distrituion is, the fewer divergent transions which occur, but it tends to mess up the rest of the model since it provides such a large area of probability space to explore. Therefore, I have tried to find a parameter that provides a happy medium where there are <10 divergent transitions.
summary(Fsmod)
#Next I extracted the means of the posterior data distributions
S1_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[1,1]")),mean(as.matrix(Fsmod, "theta[1,2]")),mean(as.matrix(Fsmod, "theta[1,3]")),mean(as.matrix(Fsmod, "theta[1,4]")))
S2_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[2,1]")),mean(as.matrix(Fsmod, "theta[2,2]")),mean(as.matrix(Fsmod, "theta[2,3]")),mean(as.matrix(Fsmod, "theta[2,4]")))
S3_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[3,1]")),mean(as.matrix(Fsmod, "theta[3,2]")),mean(as.matrix(Fsmod, "theta[3,3]")),mean(as.matrix(Fsmod, "theta[3,4]")))
S4_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[4,1]")),mean(as.matrix(Fsmod, "theta[4,2]")),mean(as.matrix(Fsmod, "theta[4,3]")),mean(as.matrix(Fsmod, "theta[4,4]")))
S5_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[5,1]")),mean(as.matrix(Fsmod, "theta[5,2]")),mean(as.matrix(Fsmod, "theta[5,3]")),mean(as.matrix(Fsmod, "theta[5,4]")))
S6_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[6,1]")),mean(as.matrix(Fsmod, "theta[6,2]")),mean(as.matrix(Fsmod, "theta[6,3]")),mean(as.matrix(Fsmod, "theta[6,4]")))
S7_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[7,1]")),mean(as.matrix(Fsmod, "theta[7,2]")),mean(as.matrix(Fsmod, "theta[7,3]")),mean(as.matrix(Fsmod, "theta[7,4]")))
S8_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[8,1]")),mean(as.matrix(Fsmod, "theta[8,2]")),mean(as.matrix(Fsmod, "theta[8,3]")),mean(as.matrix(Fsmod, "theta[8,4]")))
S9_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[9,1]")),mean(as.matrix(Fsmod, "theta[9,2]")),mean(as.matrix(Fsmod, "theta[9,3]")),mean(as.matrix(Fsmod, "theta[9,4]")))
S10_Fs_thetas<-c(mean(as.matrix(Fsmod, "theta[10,1]")),mean(as.matrix(Fsmod, "theta[10,2]")),mean(as.matrix(Fsmod, "theta[10,3]")),mean(as.matrix(Fsmod, "theta[10,4]")))

Fs_alpha<-apply(Fs_all, 2, sum)/sum(Fs_all) #here I create a vector with the alpha values which represent the market-level variant ratios
flip<-c(1,-1,-1,-1) #here I create a vector which flips the signs of the deviation of the minor flaws, moderate flaws, and major flaws
#In order to calculate the quality deviation from the Market-Level distribution each site's posterior data mean is subtracted from the alphas then divided by the alphas* 1-alphas to account for the effect of the binomial distributions and then flipped so that higher quality than the market is positive and lower quality than the market is negative
Sall_Fs_alldevs<-rbind((S1_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S2_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S3_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S4_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S5_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S6_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S7_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S8_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S9_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip,(S10_Fs_thetas-Fs_alpha)/(Fs_alpha*(1-Fs_alpha))*flip)
Sall_Fs_devs<-apply(Sall_Fs_alldevs,1,mean) #Finally, the mean of manufacturing flaw catagories deviation is the total site deviation

#I pulled the data from the matrix of small buttons divided by site and manufacturing flaw signficance
K<-length(Fl_all[1,]) #Number of manufacturing flaw catagories
N<-length(Fl_all[,1]) #Number of sites
y<-Fl_all #data matrix
alpha<-apply(Fl_all, 2, sum)/sum(Fl_all) #Normalized prior (percent of each manufacturing flaw catagory in total data set)
logsigma<-1 #variance parameter for prior
Fldat<-list(K=K, N=N,y=y, alpha=alpha, logsigma=logsigma)

Flmod<-stan(file="dirichlet.stan", data=Fldat, chains=4, iter=2000) #run model using "dirichlet.stan" code
launch_shinystan(Flmod) #to check the results of the analysis. this code as written usually throws an error message saying that there were ~5 divergent transitions, this is due to the fat tail of the log normal distribution used as a prior for the variance of the dirichlet distribution due to the step size algorithum stan uses. The larger the shape parameter of the log normal distrituion is, the fewer divergent transions which occur, but it tends to mess up the rest of the model since it provides such a large area of probability space to explore. Therefore, I have tried to find a parameter that provides a happy medium where there are <10 divergent transitions.
summary(Flmod)
#Next I extracted the means of the posterior data distributions
S1_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[1,1]")),mean(as.matrix(Flmod, "theta[1,2]")),mean(as.matrix(Flmod, "theta[1,3]")),mean(as.matrix(Flmod, "theta[1,4]")))
S2_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[2,1]")),mean(as.matrix(Flmod, "theta[2,2]")),mean(as.matrix(Flmod, "theta[2,3]")),mean(as.matrix(Flmod, "theta[2,4]")))
S3_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[3,1]")),mean(as.matrix(Flmod, "theta[3,2]")),mean(as.matrix(Flmod, "theta[3,3]")),mean(as.matrix(Flmod, "theta[3,4]")))
S4_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[4,1]")),mean(as.matrix(Flmod, "theta[4,2]")),mean(as.matrix(Flmod, "theta[4,3]")),mean(as.matrix(Flmod, "theta[4,4]")))
S5_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[5,1]")),mean(as.matrix(Flmod, "theta[5,2]")),mean(as.matrix(Flmod, "theta[5,3]")),mean(as.matrix(Flmod, "theta[5,4]")))
S6_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[6,1]")),mean(as.matrix(Flmod, "theta[6,2]")),mean(as.matrix(Flmod, "theta[6,3]")),mean(as.matrix(Flmod, "theta[6,4]")))
S7_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[7,1]")),mean(as.matrix(Flmod, "theta[7,2]")),mean(as.matrix(Flmod, "theta[7,3]")),mean(as.matrix(Flmod, "theta[7,4]")))
S8_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[8,1]")),mean(as.matrix(Flmod, "theta[8,2]")),mean(as.matrix(Flmod, "theta[8,3]")),mean(as.matrix(Flmod, "theta[8,4]")))
S9_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[9,1]")),mean(as.matrix(Flmod, "theta[9,2]")),mean(as.matrix(Flmod, "theta[9,3]")),mean(as.matrix(Flmod, "theta[9,4]")))
S10_Fl_thetas<-c(mean(as.matrix(Flmod, "theta[10,1]")),mean(as.matrix(Flmod, "theta[10,2]")),mean(as.matrix(Flmod, "theta[10,3]")),mean(as.matrix(Flmod, "theta[10,4]")))

Fl_alpha<-apply(Fl_all, 2, sum)/sum(Fl_all)#here I create a vector with the alpha values which represent the market-level variant ratios
#In order to calculate the quality deviation from the Market-Level distribution each site's posterior data mean is subtracted from the alphas then divided by the alphas* 1-alphas to account for the effect of the binomial distributions and then flipped so that higher quality than the market is positive and lower quality than the market is negative
Sall_Fl_alldevs<-rbind((S1_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S2_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S3_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S4_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S5_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S6_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S7_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S8_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S9_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip,(S10_Fl_thetas-Fl_alpha)/(Fl_alpha*(1-Fl_alpha))*flip)
Sall_Fl_devs<-apply(Sall_Fl_alldevs,1,mean) #Finally, the mean of manufacturing flaw catagories deviation is the total site deviation

#First I tested the MCD variable (the only predictor variable that is a continous variable) for deviations from normality
shapiroTest(MCD) #not significantly different from normal

#This code is for the multiple regression of small button average quality 
shapiroTest(Sall_Fs_devs) #average quality not significantly different from normal distribution

fullmod_Fs_test<-lm(Sall_Fs_devs~MCD*free*prop)
Anova(fullmod_Fs_test)
#no significant interaction between predictor variables

fullmod_Fs<-lm(Sall_Fs_devs~prop+free+MCD) #model with all variables
vif(fatmod_Fs) #no co-linearity between predictor varaiables
step(fatmod_Fs, direction = "backward") # AIC improved when MCD and property are removed as predictor variables
slimmod_Fs<-lm(Sall_Fs_devs~free) #reduced model
lm.beta(slimmod_Fs) #to calculate standardized betas

#This code is for the multiple regression of large button average quality 
shapiroTest(Sall_Fl_devs) #not significantly different from normal
fullmod_Fl_test<-lm(Sall_Fl_devs~MCD*free*prop)
Anova(fatmod_Fl_test)
#no significant interaction between predictor variables

fullmod_Fl<-lm(Sall_Fl_devs~MCD+free+prop) #model with all variables
vif(fatmod_Fl) #no co-linearity between predictor varaiables
step(fatmod_Fl, direction = "backward") # AIC improved when MCD and property are removed as predictor variables
slimmod_Fl<-lm(Sall_Fl_devs~free) #reduced model
lm.beta(slimmod_Fl) #to calculate standardized betas

#Next I used a partial pooling model where average button face diameter is pulled from a normal distribution to smooth the average face diameter of Small and Large buttons
#I pulled the data from the matrix of small buttons divided by site and face diameter
nObs<-length(Ds_all$Site)
nSites<-length(unique(Ds_all$Site))
site<-Ds_all$Site
diam<-Ds_all$`Average Diameter`
muMean<-mean(Ds_all$`Average Diameter`) #I used the overall mean of small button face diameters as the prior
muSD<-sd(Ds_all$`Average Diameter`) #I used the overall standard deviation of small button face diameters as the prior
sigmaSD<-0.5
Dsdat<-list(nObs=nObs,nSites=nSites,site=site,diam=diam,muMean=muMean, muSD=muSD, sigmaSD=sigmaSD)

Dsmod<-stan(file="MeanPartialPool.stan", data=Dsdat, chains=4, iter=2000)
launch_shinystan(Dsmod)
summary(Dsmod)

Sall_Ds_est<-c(mean(as.matrix(Dsmod, "mu[1]")),mean(as.matrix(Dsmod, "mu[2]")),mean(as.matrix(Dsmod, "mu[3]")),mean(as.matrix(Dsmod, "mu[4]")),mean(as.matrix(Dsmod, "mu[5]")),mean(as.matrix(Dsmod, "mu[6]")),mean(as.matrix(Dsmod, "mu[7]")),mean(as.matrix(Dsmod, "mu[8]")),mean(as.matrix(Dsmod, "mu[9]")),mean(as.matrix(Dsmod, "mu[10]"))) #Finally, I created a vecor of the mean posterior estimates of each site's average face diameter

#Next I pulled the data from the matrix of large buttons divided by site and face diameter
nObs<-length(Dl_all$Site)
nSites<-length(unique(Dl_all$Site))
site<-Dl_all$Site
diam<-Dl_all$`Average Diameter`
muMean<-mean(Dl_all$`Average Diameter`)#I used the overall mean of large button face diameters as the prior
muSD<-1.5 #I used the overall standard deviation of large button face diameters as the prior
sigmaSD<-0.5
Dldat<-list(nObs=nObs,nSites=nSites,site=site,diam=diam,muMean=muMean, muSD=muSD, sigmaSD=sigmaSD)

Dlmod<-stan(file="MeanPartialPool.stan", data=Dldat, chains=4, iter=2000)
launch_shinystan(Dlmod)

Sall_Dl_est<-c(mean(as.matrix(Dlmod, "mu[1]")),mean(as.matrix(Dlmod, "mu[2]")),mean(as.matrix(Dlmod, "mu[3]")),mean(as.matrix(Dlmod, "mu[4]")),mean(as.matrix(Dlmod, "mu[5]")),mean(as.matrix(Dlmod, "mu[6]")),mean(as.matrix(Dlmod, "mu[7]")),mean(as.matrix(Dlmod, "mu[8]")),mean(as.matrix(Dlmod, "mu[9]")),mean(as.matrix(Dlmod, "mu[10]"))) #Finally, I created a vecor of the mean posterior estimates of each site's average face diameter

#This code is for the multiple regression of small button face diameter 
shapiroTest(Sall_Ds_est) #no significant deviations from normality
fullmod_Ds_test<-lm(Sall_Ds_est~MCD*free*prop)
Anova(fullmod_Ds_test)
#This test found a significant interaction between freedom and property so instead of making one full model with all three variables I created two separate models, one with MCD and freedom variables and the other with MCD and property variables, and compared their AIC scores 

fullmod_Ds<-lm(Sall_Ds_est~MCD+free) #model with MCD and freedom variables
vif(fullmod_Ds) #no co-linearity between predictor varaiables
step(fullmod_Ds, direction = "backward") # AIC not improved by removing predictor variables
slimmod_Ds<-lm(Sall_Ds_est~MCD+free) #reduced model
lm.beta(slimmod_Ds) #to calculate stadardized betas

fullmod_Ds2<-lm(Sall_Ds_est~MCD+prop) #model with MCD and property variables
vif(fullmod_Ds2) #no co-linearity between predictor varaiables
step(fatmod_Ds2, direction = "backward") # AIC improved by removing property as a predictor variable
slimmod_Ds2<-lm(Sall_Ds_est~MCD) #reduced model

AIC(slimmod_Ds)-AIC(slimmod_Ds2) #AIC difference between the two models is less than 2 so neither is significantly better than the other. I selected the first model because it had a lower AIC score and had more significant predictor variables

#This code is for the multiple regression of large button face diameter 
shapiroTest(Sall_Dl_est[-3]) #no significant difference from normality when site 3 (which only contained 2 large buttons) was removed from the analysis
fullmod_Dl_test<-lm(Sall_Dl_est[-3]~MCD[-3]*free[-3]*prop[-3])
Anova(fullmod_Dl_test)
#no significant interaction between variables

fullmod_Dl<-lm(Sall_Dl_est[-3]~MCD[-3]+free[-3]+prop[-3]) #model with all predictor variables
vif(fullmod_Dl) #no co-linearity between predictor varaiables
step(fatmod_Dl, direction = "backward") # AIC improved by removing MCD and property as predictor variables
slimmod_Dl<-lm(Sall_Dl_est[-3]~free[-3]) #reduced model
lm.beta(slimmod_Dl) #to calculate stadardized betas

#Next I used beysian models to smooth the percent of backstamped buttons of Small and Large buttons
#First I used a complete pooling binomial model to calculate the most accurate a and b values to use as priors on a beta distribution of the proportion of backstamped small buttons in the overall data set
N<-sum(subset(bdat, Size == 1, select = 7) >= 0)
obs<-sum(subset(bdat, Size == 1, select = 7) == 1)
alpha<-1 #for the prior on this model I just used a beta distribution with alpha of 1 and a beta of 1, the binomical equivalent of a flat prior.
beta<-1
sigma<-2.5
Sspop_dat<-list(N=N,obs=obs,alpha=alpha,beta=beta,sigma=sigma)

Sspop_mod<-stan(file="completePooling.stan", data=Sspop_dat, chains=4, iter=2000)

#Next I used a partial pooling binomial model with priors set but the previous model to smooth the small button backstamp proportions for each site
nObs<-length(Fs_N) 
nSites<-length(site)
site<-site
N<-Fs_N
obs<-Ss_B
alpha<-mean(as.matrix(Sspop_mod, "a")) #for the prior of this model I used a beta distribution with the alpha and beta parameters defined by earlier model
beta<-mean(as.matrix(Sspop_mod, "b"))
sigma<-2.5
Ssdat<-list(nObs=nObs,nSites=nSites,site=site,N=N,obs=obs,alpha=alpha,beta=beta,sigma=sigma)

Ssmod<-stan(file="PartialPooling.stan", data=Ssdat, chains=4, iter=2000) 

Sall_Ss_est<-c(mean(as.matrix(Ssmod, "theta[1]")),mean(as.matrix(Ssmod, "theta[2]")),mean(as.matrix(Ssmod, "theta[3]")),mean(as.matrix(Ssmod, "theta[4]")),mean(as.matrix(Ssmod, "theta[5]")),mean(as.matrix(Ssmod, "theta[6]")),mean(as.matrix(Ssmod, "theta[7]")),mean(as.matrix(Ssmod, "theta[8]")),mean(as.matrix(Ssmod, "theta[9]")),mean(as.matrix(Ssmod, "theta[10]"))) #Finally, I created a vecor of the mean posterior estimates of each site's proportion of backstamped buttons

#First I used a complete pooling binomial model to calculate the most accurate a and b values to use as priors on a beta distribution of the proportion of backstamped large buttons in the overall data set
N<-sum(subset(bdat, Size == 2, select = 7) >= 0)
obs<-sum(subset(bdat, Size == 2, select = 7) == 1)
alpha<-1 #for the prior on this model I just used a beta distribution with alpha of 1 and a beta of 1, the binomical equivalent of a flat prior.
beta<-1
sigma<-2.5
Slpop_dat<-list(N=N,obs=obs,alpha=alpha,beta=beta,sigma=sigma)

Slpop_mod<-stan(file="completePooling.stan", data=Slpop_dat, chains=4, iter=2000)

#Next I used a partial pooling binomial model with priors set but the previous model to smooth the large button backstamp proportions for each site
nObs<-length(Fl_N) 
nSites<-length(site)
site<-site
N<-Fl_N
obs<-Sl_B
alpha<-mean(as.matrix(Slpop_mod, "a")) #for the prior of this model I used a beta distribution with the alpha and beta parameters defined by earlier model
beta<-mean(as.matrix(Slpop_mod, "b"))
sigma<-2.5
Sldat<-list(nObs=nObs,nSites=nSites,site=site,N=N,obs=obs,alpha=alpha,beta=beta,sigma=sigma)

Slmod<-stan(file="PartialPooling.stan", data=Sldat, chains=4, iter=2000)

Sall_Sl_est<-c(mean(as.matrix(Slmod, "theta[1]")),mean(as.matrix(Slmod, "theta[2]")),mean(as.matrix(Slmod, "theta[3]")),mean(as.matrix(Slmod, "theta[4]")),mean(as.matrix(Slmod, "theta[5]")),mean(as.matrix(Slmod, "theta[6]")),mean(as.matrix(Slmod, "theta[7]")),mean(as.matrix(Slmod, "theta[8]")),mean(as.matrix(Slmod, "theta[9]")),mean(as.matrix(Slmod, "theta[10]")))#Finally, I created a vecor of the mean posterior estimates of each site's proportion of backstamped buttons

#This code is for the multiple regression of the proportion of backstamped small buttons at each site  
shapiroTest(Sall_Ss_est) #no significant deviations from normality
fullmod_Ss_test<-lm(Sall_Ss_est~MCD*free*prop) 
Anova(fullmod_Ss_test)
#no significant interaction terms between independent variables

fullmod_Ss<-lm(Sall_Ss_est~MCD+free+prop) #model with all predictor variables
vif(fullmod_Ss) #no co-linearity between predictor varaiables
step(fullmod_Ss, direction = "backward") # AIC improved by removing freedom as a predictor variable
slimmod_Ss<-lm(Sall_Ss_est~prop+MCD) #reduced model
lm.beta(slimmod_Ss) #to calculate stadardized betas

#This code is for the multiple regression of the proportion of backstamped large buttons at each site  
shapiroTest(Sall_Sl_est) #no significant deviations from normality
fatmod_Sl_test<-lm(Sall_Sl_est~MCD*free*prop)
Anova(fatmod_Sl_test)
#no significant interaction terms between independent variables

fullmod_Sl<-lm(Sall_Sl_est~MCD+free+prop) #model with all predictor variables
vif(fatmod_Sl) #no co-linearity between predictor varaiables
step(fatmod_Sl, direction = "backward") # AIC improved by removing freedom and MCD as predictor variables
slimmod_Sl<-lm(Sall_Sl_est~prop) #reduced model
lm.beta(slimmod_Sl) #to calculate stadardized betas

#Finally, I used beysian models to smooth the percent of small sized buttons at each site
#First I used a complete pooling binomial model to calculate the most accurate a and b values to use as priors on a beta distribution of the proportion of small sized buttons in the overall data set
N<-sum(bdat$Size >= 0)
obs<-sum(bdat$Size == 1)
alpha<-1 #for the prior on this model I just used a beta distribution with alpha of 1 and a beta of 1, the binomical equivalent of a flat prior.
beta<-1
sigma<-2.5
Tpop_dat<-list(N=N,obs=obs,alpha=alpha,beta=beta,sigma=sigma)

Tpop_mod<-stan(file="completePooling.stan", data=Tpop_dat, chains=4, iter=2000)

#site level data
nObs<-length(T_N) 
nSites<-length(site)
site<-site
N<-T_N
obs<-Fs_N
alpha<-mean(as.matrix(Tpop_mod, "a")) #for the prior of this model I used a beta distribution with the alpha and beta parameters defined by earlier model
beta<-mean(as.matrix(Tpop_mod, "b"))
sigma<-2.5
Tdat<-list(nObs=nObs,nSites=nSites,site=site,N=N,obs=obs,alpha=alpha,beta=beta,sigma=sigma)

Tmod<-stan(file="PartialPooling.stan", data=Tdat, chains=4, iter=2000)

Sall_T_est<-c(mean(as.matrix(Tmod, "theta[1]")),mean(as.matrix(Tmod, "theta[2]")),mean(as.matrix(Tmod, "theta[3]")),mean(as.matrix(Tmod, "theta[4]")),mean(as.matrix(Tmod, "theta[5]")),mean(as.matrix(Tmod, "theta[6]")),mean(as.matrix(Tmod, "theta[7]")),mean(as.matrix(Tmod, "theta[8]")),mean(as.matrix(Tmod, "theta[9]")),mean(as.matrix(Tmod, "theta[10]"))) #Finally, I created a vecor of the mean posterior estimates of each site's proportion of small sized buttons

#This code is for the multiple regression of the proportion of small sized buttons at each site 
shapiroTest(Sall_T_est) #no significant deviations from normality
fullmod_T_test<-lm(Sall_T_est~MCD*free*prop)
Anova(fullmod_T_test)
#no significant interaction terms between indpendent variables

fullmod_T<-lm(Sall_T_est~MCD+free+prop) #model with all predictor variables
vif(fullmod_T) #no co-linearity between predictor varaiables
step(fatmod_T, direction = "backward") # AIC improved by removing freedom and MCD as predictor variables
slimmod_T<-lm(Sall_T_est~prop) #reduced modle
lm.beta(slimmod_T) #to calculate stadardized betas
