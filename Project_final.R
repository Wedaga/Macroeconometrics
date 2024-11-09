setwd("~/time series/Econ901/Econ 910/Project")
library(tidyverse)
library(zoo)
library(readr)
library(openxlsx)
library(dplyr)
library(rio)
library(extrafont)
library("ggplot2")
library(dplyr)
library(rugarch)
library(fGarch)
library(forecast)
library(tidyverse)
library(tstools)
library(timeSeries)
library(broom)
library("urca")
library("vars")
library("stargazer")
library(lpirfs)
library(ggpubr)
library(gridExtra)
library(tseries)

#loading data
project_data <- read_csv("/Users/preciousallor/Desktop/Fall 2022/Econ 910/Project/data.csv")
View(project_data)
head(project_data)
rop<- ts(project_data[,3], start = c(2000, 1),end = c(2022, 5), frequency = 12)
rop
c_real<- ts(project_data[,4], start = c(2000, 1), end = c(2022, 5), frequency = 12)
oilprod<- ts(project_data[,5], start = c(2000, 1), end = c(2022, 5),frequency = 12)
rea<- ts(project_data[,6], start = c(2000, 1), end = c(2022, 5),frequency = 12)
reer<- ts(project_data[,7], start = c(2000, 1),end = c(2022, 5), frequency = 12)
inf_cpi<- ts(project_data[,8], start = c(2000, 1),end = c(2022, 5), frequency = 12)
ir_tb<- ts(project_data[,9], start = c(2000, 1),end = c(2022, 5), frequency = 12)
ir_policy<- ts(project_data[,10], start = c(2000, 1), end = c(2022, 5),frequency = 12)
m1<- ts(project_data[,11], start = c(2000, 1), end = c(2022, 5),frequency = 12)
c_oilprod<-pctChange(oilprod)
c_rop<-pctChange(rop)
c_rea<-pctChange(rea)
c_rop<-log(rop)
lreal<-log(c_real)
lreer<-log(reer)
linf<-log(inf_cpi)
lir<-log(ir_tb)
lm1<-log(m1)

#######################################
#Plots of initial series.
attach(mtcars)
par(mfrow=c(2,2))
plot(c_oilprod, col="black",
     main="Percent Change in World Oil Production",
     sub="Fig 1a",
     xlab="Monthly",
     ylab="c_oil prod",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)

plot(lrop, col="black",
     main="Log of Spot Brent oil Price",
     sub="Fig 1b",
     xlab="Monthly",
     ylab="lrop",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)

plot(rea, col="black",
     main=" Global Economic Activity Index",
     sub="Fig 1c",
     xlab="Monthly",
     ylab="rea",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)
plot(lreal, col="black",
     main="Log of Real Economic Activity Index (Ghana) ",
     sub="Fig 1d",
     xlab="Monthly",
     ylab="lreal",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)

par(mfrow=c(2,2))
plot(lreer, col="black",
     main="Log of Real Effective Exchange ",
     sub="Fig 2a",
     xlab="Monthly",
     ylab="lreer",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)
plot(linf, col="black",
     main="Log of Inflation Rate ",
     sub="Fig 2b",
     xlab="Monthly",
     ylab="linf",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)
plot(lir, col="black",
     main="Log of Treasury Bill Rate ",
     sub="Fig 2c",
     xlab="Monthly",
     ylab="lir",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)
########################################
# Stationarity test of all variables
#######################################
#oil production
oil_prod.df <- ur.df(y=c_oilprod, type="none", 
                     selectlags="AIC")
summary(oil_prod.df)
oil_prod.df <- ur.df(y=c_oilprod, type="trend", 
                     selectlags="AIC")
summary(oil_prod.df)
oil_prod.df <- ur.df(y=c_oilprod, type="drift", 
                     selectlags="AIC")
summary(oil_prod.df)

# Oil price

c_rop.df <- ur.df(y=c_rop, type="none", 
                 selectlags="AIC")
summary(c_rop.df)

c_rop.df <- ur.df(y=c_rop, type="trend", 
                  selectlags="AIC")
summary(c_rop.df)
c_rop.df <- ur.df(y=c_rop, type="drift", 
                  selectlags="AIC")
summary(c_rop.df)

#Global economic activiy
rea.df <- ur.df(y=rea, type="none", 
                selectlags="AIC")
summary(rea.df)
rea.df <- ur.df(y=rea, type="trend", 
                selectlags="AIC")
summary(rea.df)
rea.df <- ur.df(y=rea, type="drift", 
                selectlags="AIC")
summary(rea.df)
#stationary at 5%

#Ghana economic activity
lreal.df <- ur.df(y=lreal, type="none", 
                  selectlags="AIC")
summary(lreal.df)
lreal.df <- ur.df(y=lreal, type="trend", 
                  selectlags="AIC")
summary(lreal.df)
lreal.df <- ur.df(y=lreal, type="drift", 
                  selectlags="AIC")
summary(lreal.df)
diff_real<-diff(lreal)
diff_real.df <- ur.df(y=diff_real, type="none", 
                      selectlags="AIC")
summary(diff_real.df)

diff_real.df <- ur.df(y=diff_real, type="trend", 
                      selectlags="AIC")
summary(diff_real.df)
diff_real.df <- ur.df(y=diff_real, type="drift", 
                      selectlags="AIC")
summary(diff_real.df)

#real effective exchange rate

lreer.df <- ur.df(y=lreer, type="none", 
                  selectlags="AIC")
summary(lreer.df)
lreer.df <- ur.df(y=lreer, type="trend", 
                  selectlags="AIC")
summary(lreer.df)

lreer.df <- ur.df(y=lreer, type="drift", 
                  selectlags="AIC")
summary(lreer.df)
diff_lreer<-diff(lreer)
diff_lreer.df <- ur.df(y=diff_lreer, type="none", 
                       selectlags="AIC")
summary(diff_lreer.df)

diff_lreer.df <- ur.df(y=diff_lreer, type="trend", 
                       selectlags="AIC")
summary(diff_lreer.df)
diff_lreer.df <- ur.df(y=diff_lreer, type="drift", 
                       selectlags="AIC")
summary(diff_lreer.df)


# Inflation
linf.df <- ur.df(y=linf, type="none", 
                 selectlags="AIC")
summary(linf.df)
linf.df <- ur.df(y=linf, type="trend", 
                 selectlags="AIC")
summary(linf.df)
linf.df <- ur.df(y=linf, type="drift", 
                 selectlags="AIC")
summary(linf.df)

diff_linf<-diff(linf)
diff_linf.df <- ur.df(y=diff_linf, type="none", 
                      selectlags="AIC")
summary(diff_linf.df)
diff_linf.df <- ur.df(y=diff_linf, type="trend", 
                      selectlags="AIC")
summary(diff_linf.df)

diff_linf.df <- ur.df(y=diff_linf, type="drift", 
                      selectlags="AIC")
summary(diff_linf.df)

#T-bill rates
lir.df <- ur.df(y=lir, type="none", 
                selectlags="AIC")
summary(lir.df)
lir.df <- ur.df(y=lir, type="trend", 
                selectlags="AIC")
summary(lir.df)
lir.df <- ur.df(y=lir, type="drift", 
                selectlags="AIC")
summary(lir.df)

diff_lir<-diff(lir)
diff_lir.df <- ur.df(y=diff_lir, type="none", 
                     selectlags="AIC")
summary(diff_lir.df)
diff_lir.df <- ur.df(y=diff_lir, type="trend", 
                     selectlags="AIC")
summary(diff_lir.df)
diff_lir.df <- ur.df(y=diff_lir, type="drift", 
                     selectlags="AIC")
summary(diff_lir.df)

#Money supply (M1)
lm1.df <- ur.df(y=lm1, type="none", 
                selectlags="AIC")
summary(lm1.df)
lm1.df <- ur.df(y=lm1, type="trend", 
                selectlags="AIC")
summary(lm1.df)
lm1.df <- ur.df(y=lm1, type="drift", 
                selectlags="AIC")
summary(lm1.df)

diff_lm1<-diff(lm1)

diff_lm1.df <- ur.df(y=diff_lm1, type="none", 
                     selectlags="AIC")
summary(diff_lm1.df)
diff_lm1.df <- ur.df(y=diff_lm1, type="trend", 
                     selectlags="AIC")
summary(diff_lm1.df) 
diff_lm1.df <- ur.df(y=diff_lm1, type="drift", 
                     selectlags="AIC")
summary(diff_lm1.df)

######################################################################################################
#VAR TO RECOVER OIL MARKET SHOCKS with Cholesky decomposition.
######################################################################################################
oil_data<-ts.combine(c_oilprod,c_rea,c_rop )
oil_data

VARselect(oil_data, lag.max=4)
#SVAR MODEL WITH CHOLESKY DECOMPOSITION
prodeq<-tsreg(c_oilprod, ts.combine(lags(c_oilprod,1:2), lags(c_rea,1:2), lags(c_rop,1:2)))
demandeq<-tsreg(c_rea, ts.combine(lags(c_oilprod,0:2), lags(c_rea,1:2), lags(c_rop,1:2)))
priceeq<-tsreg(c_rea, ts.combine(lags(c_oilprod,0:2), lags(c_rea,0:2), lags(c_rop,1:2)))
summary(prodeq)
summary(demandeq)
summary(priceeq)
stargazer(prodeq,demandeq,priceeq , type="latex", title = "Structural VAR Results")
varm<-VAR(oil_data, lag.max = 4, ic="AIC")
summary(varm)
res<-residuals(varm)
M <- t(chol(cov(res)))
M
D <- diag(3)
diag(D) <- diag(M)
D
Ainv.hat <- M %*% solve(D)
A.hat <- solve(Ainv.hat)
A.hat
res
s.hat <- t(A.hat %*% t(res))
s.hat

oil_shocks<- s.hat[,1]
oil_shocks
demand_shocks<- s.hat[,2]
price_shocks<- s.hat[,3]


oil_shocks<- ts(s.hat[,1], start = c(2000, 2),end = c(2022, 5), frequency = 12)
demand_shocks<- ts(s.hat[,2], start = c(2000, 2),end = c(2022, 5), frequency = 12)
price_shocks<- ts(s.hat[,3], start = c(2000, 2),end = c(2022, 5), frequency = 12)

#plots of shock series



par(mfrow=c(2,2))

plot(oil_shocks, col="black",
     main="Oil Supply Shocks",
     sub="Fig 2a",
     xlab="Monthly",
     ylab="oil_supply",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)

plot(demand_shocks, col="black",
     main="Aggregate Demand Shocks",
     sub="Fig 2b",
     xlab="Monthly",
     ylab="ag_demand",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)

plot(price_shocks, col="black",
     main="Oil-Specific Demand Shocks",
     sub="Fig 2c",
     xlab="Monthly",
     ylab="oil_specific",
     col.axis="black",
     family="Times New Roman",
     cex.main=0.7,
     cex.axis=0.5,
     cex.lab=0.5)

#irfs

oil_data<-ts.combine(c_oilprod,c_rea,c_rop )

irf_1 <- irf(varm, impulse = "c_oilprod", response = "c_oilprod", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
irf_2 <- irf(varm, impulse = "c_oilprod", response = "c_rea", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
irf_3 <- irf(varm, impulse = "c_oilprod", response = "c_rop", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
irf_4 <- irf(varm, impulse = "c_rea", response = "c_oilprod", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
irf_5 <- irf(varm, impulse = "c_rea", response = "c_rea", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
irf_6 <- irf(varm, impulse = "c_rea", response = "c_rop", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
irf_7 <- irf(varm, impulse = "c_rop", response = "c_oilprod", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
irf_8 <- irf(varm, impulse = "c_rop", response = "c_rea", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)
irf_9 <- irf(varm, impulse = "c_rop", response = "c_rop", n.ahead = 24, ortho = TRUE,
             cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 1000)

plot(irf_1)
plot(irf_2)
plot(irf_3)
plot(irf_4)
plot(irf_5)
plot(irf_6)
plot(irf_7)
plot(irf_8)
plot(irf_9)
#######################################################################

#Local projections Model
#creating switching dummy
swtchingdata<- ifelse(project_data$year==2000 | project_data$year==2004 |project_data$year==2008 | project_data$year==2012 | project_data$year==2016 | project_data$year==2020, 
                      1,0)
swtchingdata <- ts(swtchingdata, start = c(2000, 1), end = c(2022, 5), frequency = 12)
swtchingdata


datafinal<- ts.combine(lreal, linf, lir, lreer,oil_shocks, demand_shocks, price_shocks,swtchingdata )

sample_start  <- 1
sample_end    <- dim(datafinal)[1]
#Dependent variables
Log_CIEA_Real<-datafinal[sample_start:sample_end, 1]
Log_CIEA_Real<-as.data.frame(Log_CIEA_Real)
Log_Inflation<-datafinal[sample_start:sample_end, 2]
Log_Inflation<-as.data.frame(Log_Inflation)
Log_T_bill_rate<-datafinal[sample_start:sample_end, 3]
Log_T_bill_rate<-as.data.frame(Log_T_bill_rate)
Log_REER<-datafinal[sample_start:sample_end, 4]
Log_REER<-as.data.frame(Log_REER)
#SHOCKS
oil_supply_shock<- as.data.frame(datafinal[sample_start:sample_end, 5])
aggregate_demand_shock<-as.data.frame(datafinal[sample_start:sample_end, 6])
oi_specific_demand_shock<- as.data.frame(datafinal[sample_start:sample_end, 7])
switching<-datafinal[sample_start:sample_end, 8]

#local projections
#Shock effect on Economic activity

results_nl_iv1 <- lp_nl_iv(Log_CIEA_Real,
                           lags_endog_nl     = 1,
                           shock             = oil_supply_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)

# Make and save plots
plots_nl_iv <- plot_nl(results_nl_iv1)
combine_plots <- list()
# Save nonlinear plots for low govt spending
combine_plots[[1]] <- plots_nl_iv$gg_s1[[1]]
# Save nonlinear plots  high govt spending
combine_plots[[2]] <- plots_nl_iv$gg_s2[[1]]

results_nl_iv2 <- lp_nl_iv(Log_CIEA_Real,
                           lags_endog_nl     = 1,
                           shock             = aggregate_demand_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)

# Make and save plots
plots_nl_iv2 <- plot_nl(results_nl_iv2)
# Save nonlinear plots for  low govt spending
combine_plots[[3]] <- plots_nl_iv2$gg_s1[[1]]
# Save nonlinear plots for high govt spending
combine_plots[[4]] <- plots_nl_iv2$gg_s2[[1]]



results_nl_iv3 <- lp_nl_iv(Log_CIEA_Real,
                           lags_endog_nl     = 1,
                           shock             = oi_specific_demand_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)

# Make and save plots
plots_nl_iv3 <- plot_nl(results_nl_iv3)
# Save nonlinear plots for  low govt spending
combine_plots[[5]] <- plots_nl_iv3$gg_s1[[1]]
# Save nonlinear plots for  high govt spending
combine_plots[[6]] <- plots_nl_iv3$gg_s2[[1]]

lin_plots_all     <- sapply(combine_plots, ggplotGrob)
combine_plots_all_CieaReal <- marrangeGrob(lin_plots_all, nrow = 2, ncol = 3, top = NULL)
combine_plots_all_CieaReal


#local projections
#Shock effect on Inflation 

results_nl_iv4 <- lp_nl_iv(Log_Inflation,
                           lags_endog_nl     = 1,
                           shock             = oil_supply_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)
# Make and save plots
plots_nl_iv4 <- plot_nl(results_nl_iv4)
combine_plots2 <- list()
# Save nonlinear plots for  low govt spending
combine_plots2[[1]] <- plots_nl_iv4$gg_s1[[1]]
# Save nonlinear plots for high govt spending
combine_plots2[[2]] <- plots_nl_iv4$gg_s2[[1]]

results_nl_iv5 <- lp_nl_iv(Log_Inflation,
                           lags_endog_nl     = 1,
                           shock             = aggregate_demand_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)

# Make and save plots
plots_nl_iv5 <- plot_nl(results_nl_iv5)
# Save nonlinear plots for low govt spending
combine_plots2[[3]] <- plots_nl_iv5$gg_s1[[1]]
# Save nonlinear plots for  high govt spending
combine_plots2[[4]] <- plots_nl_iv5$gg_s2[[1]]



results_nl_iv6 <- lp_nl_iv(Log_Inflation,
                           lags_endog_nl     = 1,
                           shock             = oi_specific_demand_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)

# Make and save plots
plots_nl_iv6 <- plot_nl(results_nl_iv6)
# Save nonlinear plots for  low govt spending
combine_plots2[[5]] <- plots_nl_iv6$gg_s1[[1]]
# Save nonlinear plots for high govt spending
combine_plots2[[6]] <- plots_nl_iv6$gg_s2[[1]]

lin_plots_all2     <- sapply(combine_plots2, ggplotGrob)
combine_plots_all_Inflation <- marrangeGrob(lin_plots_all2, nrow = 2, ncol = 3, top = NULL)
combine_plots_all_Inflation


#local projections
#Shock effect on T_Bill_rate 

results_nl_iv7 <- lp_nl_iv(Log_T_bill_rate,
                           lags_endog_nl     = 1,
                           shock             = oil_supply_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)
# Make and save plots
plots_nl_iv7 <- plot_nl(results_nl_iv7)
combine_plots3 <- list()
# Save nonlinear plots for  low govt spending
combine_plots3[[1]] <- plots_nl_iv7$gg_s1[[1]]
# Save nonlinear plots for  high govt spending
combine_plots3[[2]] <- plots_nl_iv7$gg_s2[[1]]

results_nl_iv8 <- lp_nl_iv(Log_T_bill_rate,
                           lags_endog_nl     = 1,
                           shock             = aggregate_demand_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)

# Make and save plots
plots_nl_iv8 <- plot_nl(results_nl_iv8)
# Save nonlinear plots for  low govt spending
combine_plots3[[3]] <- plots_nl_iv8$gg_s1[[1]]
# Save nonlinear plots for  high govt spending
combine_plots3[[4]] <- plots_nl_iv8$gg_s2[[1]]



results_nl_iv9 <- lp_nl_iv(Log_T_bill_rate,
                           lags_endog_nl     = 1,
                           shock             = oi_specific_demand_shock,
                           trend             = 0,
                           confint           = 1.96,
                           hor               = 36,
                           switching         = switching,
                           use_hp            = FALSE,
                           gamma             = 3)

# Make and save plots
plots_nl_iv9 <- plot_nl(results_nl_iv9)
# Save nonlinear plots for  low govt spending
combine_plots3[[5]] <- plots_nl_iv9$gg_s1[[1]]
# Save nonlinear plots for high govt spending
combine_plots3[[6]] <- plots_nl_iv9$gg_s2[[1]]

lin_plots_all3     <- sapply(combine_plots3, ggplotGrob)
combine_plots_all_T_bill <- marrangeGrob(lin_plots_all3, nrow = 2, ncol = 3, top = NULL)
combine_plots_all_T_bill


#local projections
#Shock effect on real effective exchange rate 

results_nl_iv13 <- lp_nl_iv(Log_REER,
                            lags_endog_nl     = 1,
                            shock             = oil_supply_shock,
                            trend             = 0,
                            confint           = 1.96,
                            hor               = 36,
                            switching         = switching,
                            use_hp            = FALSE,
                            gamma             = 3)
# Make and save plots
plots_nl_iv13 <- plot_nl(results_nl_iv13)
combine_plots5 <- list()
# Save nonlinear plots for low govt spending
combine_plots5[[1]] <- plots_nl_iv13$gg_s1[[1]]
# Save nonlinear plots for high govt spending
combine_plots5[[2]] <- plots_nl_iv13$gg_s2[[1]]

results_nl_iv14 <- lp_nl_iv(Log_REER,
                            lags_endog_nl     = 1,
                            shock             = aggregate_demand_shock,
                            trend             = 0,
                            confint           = 1.96,
                            hor               = 36,
                            switching         = switching,
                            use_hp            = FALSE,
                            gamma             = 3)

# Make and save plots
plots_nl_iv14 <- plot_nl(results_nl_iv14)
# Save nonlinear plots for low govt spending
combine_plots5[[3]] <- plots_nl_iv14$gg_s1[[1]]
# Save nonlinear plots for high govt spending
combine_plots5[[4]] <- plots_nl_iv14$gg_s2[[1]]



results_nl_iv15 <- lp_nl_iv(Log_REER,
                            lags_endog_nl     = 1,
                            shock             = oi_specific_demand_shock,
                            trend             = 0,
                            confint           = 1.96,
                            hor               = 36,
                            switching         = switching,
                            use_hp            = FALSE,
                            gamma             = 3)

# Make and save plots
plots_nl_iv15 <- plot_nl(results_nl_iv15)
# Save nonlinear plots for low govt spending
combine_plots5[[5]] <- plots_nl_iv15$gg_s1[[1]]
# Save nonlinear plots for high govt spending
combine_plots5[[6]] <- plots_nl_iv15$gg_s2[[1]]

lin_plots_all5     <- sapply(combine_plots5, ggplotGrob)
combine_plots_all_reer <- marrangeGrob(lin_plots_all5, nrow = 2, ncol = 3, top = NULL)
combine_plots_all_reer

# Without switching model.

#Oil supply shock
data<- as.data.frame(datafinal[sample_start:sample_end, 1:4])
VARselect(data, lag.max=4)
results_lin_iv <- lp_lin_iv(data,
                            lags_endog_lin = 3,
                            shock          = oil_supply_shock,
                            trend          = 0,
                            confint        = 1.96,
                            hor            = 36)
iv_lin_plots    <- plot_lin(results_lin_iv)
complots<-list()
complots[[1]]<- iv_lin_plots[[1]]
complots[[2]]<- iv_lin_plots[[2]]
complots[[3]]<- iv_lin_plots[[3]]
complots[[4]]<- iv_lin_plots[[4]]


results_lin_iv1 <- lp_lin_iv(data,
                             lags_endog_lin = 3,
                             shock          = aggregate_demand_shock,
                             trend          = 0,
                             confint        = 1.96,
                             hor            = 36)
iv_lin_plots1    <- plot_lin(results_lin_iv1)
complots[[5]]<- iv_lin_plots1[[1]]
complots[[6]]<- iv_lin_plots1[[2]]
complots[[7]]<- iv_lin_plots1[[3]]
complots[[8]]<- iv_lin_plots1[[4]]


results_lin_iv2 <- lp_lin_iv(data,
                             lags_endog_lin = 3,
                             shock          = oi_specific_demand_shock,
                             trend          = 0,
                             confint        = 1.96,
                             hor            = 36)

iv_lin_plots2    <- plot_lin(results_lin_iv2)
complots[[9]]<- iv_lin_plots2[[1]]
complots[[10]]<- iv_lin_plots2[[2]]
complots[[11]]<- iv_lin_plots2[[3]]
complots[[12]]<- iv_lin_plots2[[4]]

lin_plots1     <- sapply(complots, ggplotGrob)
combine1 <- marrangeGrob(lin_plots1, nrow = 4, ncol = 3, top = NULL)
combine1
