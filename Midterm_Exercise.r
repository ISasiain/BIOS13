#title: "BIOS13 Midterm Exercise"
#subtitle: "Effect of precipitation on the Eulaema nigrita bee population"
#author: "Iñaki Sasiain"
#date: "01/12/2022"
#output: png_document
#fig_caption: yes


#The glmmTMB, MuMIn and ggplot2 libraries are imported
library(glmmTMB)
library(MuMIn)

#The csv file containing the data is opened and the information stored. The bee
#capture method is stored as a factor.
Eu_data <- read.csv("/home/isc/Current_courses/Processing_and_Analysis_of_Biological_Data/Eulaema.csv")
Eu_data$method <- as.factor(Eu_data$method)

#Two possible models are created to explain the effect of the precipitation in
#the bee abundance, one considering the capture method as a random variable (m1), 
#and the second one taking just into account the fixed variables (m2).
m1 <- glmmTMB(Eulaema_nigrita ~ MAP + Pseason + (1|method), data=Eu_data, family=nbinom2())
m2 <- glmmTMB(Eulaema_nigrita ~ MAP + Pseason, data=Eu_data, family=nbinom2())

#In order to select the most appropriate model, the AIC is determined for all.
#According to this, m1 (the one with the random variable) is the ones that fits 
#better to our data.
mlist <- list(m1,m2) #A list containing both models is created
AICTab = AIC(m1, m2) #The AIC value is determined and stored in AICTab
AICTab$logLik <- unlist(lapply(mlist, logLik)) # The log likelyhood value is determined and added to AICTab
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2) #The delta AIC is calculated and added to AICTab
lh = exp(-0.5*AICTab$delta) #The ln value is determined
AICTab$w = round(lh/sum(lh), 2) #The weights of the models are determined and added to AICTab
AICTab = AICTab[order(AICTab$AIC, decreasing=F),] #AICTab is sorted in higher quality order
print(AICTab) #AICTab is printed 

#The following piece of code determined the pseudo marginal and conditional R squared values.
#The percentage of the variance explained by the random and fixed variables is determined according 
#to the lognormal method and printed.
rs <- r.squaredGLMM(m1)
print(rs)
var_ex_by_fix <- rs["lognormal","R2m"]/rs["lognormal","R2c"]
var_ex_by_ran <- (rs["lognormal","R2c"]-rs["lognormal","R2m"])/rs["lognormal","R2c"]
cat("The", var_ex_by_fix*100, "% of the variance is explained by the fixed variables")
cat("The", var_ex_by_ran*100, "% of the variance is explained by the random variables")

#Plotting the answers.

#The output plot will be saved in the next directory. CHANGE THE DIRECTORY TO CHOOSE WHERE TO DOWNLOAD THE PNG FILE.
png(file="/home/isc/Downloads/Eu_precip_plot.png",width=800, height=400)
par(mfrow=c(1,2))
#The effect of the precipitation seasonality (keeping the mean annual precipitation constant as the mean of the data) will be plotted firstly. 
new_Pseason <- seq(min(Eu_data$Pseason),max(Eu_data$Pseason),by=0.25) #The Pseason values to be predicted are created
mean_MAP <- mean(Eu_data$MAP) #The mean of the MAP variable is calculated

#The predicted Eu_nigrita abundance values are determined per each group of the random variable
new_Eu_nig_1N <- predict(m1, list(Pseason = new_Pseason,MAP = rep(mean_MAP,length(new_Pseason)),method = rep(as.factor("Net"),length(new_Pseason))),type="response")
new_Eu_nig_1T <- predict(m1, list(Pseason = new_Pseason,MAP = rep(mean_MAP,length(new_Pseason)),method = rep(as.factor("Traps"),length(new_Pseason))),type="response")
new_Eu_nig_1NT <- predict(m1, list(Pseason = new_Pseason,MAP = rep(mean_MAP,length(new_Pseason)),method = rep(as.factor("NetTraps"),length(new_Pseason))),type="response")

#The values of the Eu_nigrita count VS the Pseason are plotted.
plot(Eu_data$Pseason, Eu_data$Eulaema_nigrita, pch=16, ylab="Eulaema nigrita count", xlab="Precipitation seasonality (%)",cex.lab=1.2)
#The different models for the 3 groups of the random variable are plotted as lines.
lines(new_Pseason, new_Eu_nig_1T, col="red",lwd=2) 
lines(new_Pseason, new_Eu_nig_1N, col="green",lwd=2)
lines(new_Pseason, new_Eu_nig_1NT, col="blue",lwd=2)
legend(66,1075,legend=c("Net", "Traps", "NetTraps"), col=c("red","green", "blue"), lty=1:1:1, lwd=2, cex=1)

#The effect of the mean annual precipitation (keeping the seasonality constant as the mean of the data) will be plotted secondly. 
new_MAP <- seq(min(Eu_data$MAP),max(Eu_data$MAP),by=5) #The MAP values to be predicted are created
mean_Pseason <- mean(Eu_data$Pseason) #The mean of the Pseason variable is calculated

#The predicted Eu_nigrita abundance values are determined per each group of the random variable
new_Eu_nig_2N <- predict(m1, list(MAP = new_MAP,Pseason = rep(mean_Pseason,length(new_MAP)),method = rep(as.factor("Net"),length(new_MAP))),type="response")
new_Eu_nig_2T <- predict(m1, list(MAP = new_MAP,Pseason = rep(mean_Pseason,length(new_MAP)),method = rep(as.factor("Traps"),length(new_MAP))),type="response")
new_Eu_nig_2NT <- predict(m1, list(MAP = new_MAP,Pseason = rep(mean_Pseason,length(new_MAP)),method = rep(as.factor("NetTraps"),length(new_MAP))),type="response")

#The values of the Eu_nigrita count VS the MAP are plotted.
plot(Eu_data$MAP, Eu_data$Eulaema_nigrita, pch=16, ylab="Eulaema nigrita count", xlab="Mean annual precipitation (mm)",cex.lab=1.2)
#The different models for the 3 groups of the random variable are plotted as lines.
lines(new_MAP, new_Eu_nig_2T,col="red",lwd=2)
lines(new_MAP, new_Eu_nig_2N, col="green",lwd=2)
lines(new_MAP, new_Eu_nig_2NT, col="blue", lwd=2)
legend(2300,1075,legend=c("Net", "Traps", "NetTraps"), col=c("red","green", "blue"), lty=1:1:1, lwd=2, cex=1)

dev.off()

#The percentage of change in the Eulaema nigrita count per unit of MAP and Pseason is determined.
cat("The change of Eulaema nigrita count per mm unit in MAP is", (exp(summary(m1)$coef$cond[2,1])-1)*100, "%")
cat("The change of Eulaema nigrita count per % unit in Pseason is",(exp(summary(m1)$coef$cond[3,1])-1)*100, "%")
