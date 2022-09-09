### Start Date of Simulation =====================

startDate <- as.Date("2020-03-02")


### Loading Data =================================

## Number of hospital beds in India (National Health Profile, 2019)

India_GH <- 713986
Bed_MC <- read_csv("data/india/Bed_MC.csv")
India_MC <- sum(Bed_MC$Bed_MC, na.rm = TRUE)
bed_count <- India_GH + India_MC
rm(India_GH, India_MC, Bed_MC)

library(scales)

## Loading India's population data (Age proportions)-----------------
indiapop = read.csv('data/india/indiapop.csv',as.is = TRUE)


## Writing function to normalize contact matrices for India -------------------
# (projected) contact matrices

# Acknowlegments: codes from Petra Klepac (petrakle)--- --- --- 

normalize.contact.matrices <- function(C, popv, make.sym = F)
{
  # FUN normalize them so that
  # the matrix with all the contacts is normalized so that its dominant eigenvalue is 1 
  # and other matrices keep their contributions 
  if (make.sym)
  {
    Csym <- lapply(C, function(x, popv) (x + t(x)*((popv)%*%t(1/popv)))/2, popv) # make sure contacts are reciprocal
  } else {
    Csym <- C # if make.sym = F leave it as is
  }
  eig1 <- Re(eigen(Csym["all"]$all)$value[1])  # save dominant eigenvalue of the matrix with all contacts
  
  # divide all matrices by the real part of the dominant matrix of all of the contacts
  # that way all the rescaled matrices still sum to C_all = C_work + C_home + C_school + C_other
  Cnorm <- lapply(Csym, function(x,eig1) x/eig1, eig1)
  
  return(Cnorm)
}

## Synthetic contact matrices for India from Prem, Cook and Jit (2017) https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697 ---------------------
load(paste0('data/india/contacts_india.rdata'))
contacts <- normalize.contact.matrices(contacts_india, indiapop$popage, make.sym = TRUE)
rm(contacts_india)

## Loading R0 Posterior Data from Kucharski et. al (2020)----------------------
# --- read in R0 posterior

R0_plot     <- read.csv(paste0("data/out_R0.csv"))
R0_dates    <- read.csv(paste0('data/out_date.csv'))
start_date  <- as.Date(R0_dates[1,1]) # first case
end_date    <- as.Date(R0_dates[nrow(R0_dates),1]) # period to forecast ahead
date_range  <- seq.Date(start_date,end_date,1)

# extract all estimates from 01.01.2020 - 23.01.2020
R0_posterior <- R0_plot[which(date_range == as.Date("2020-01-01") ):which(date_range == as.Date("2020-01-23")),]
range(R0_posterior)
r0posterior = as.vector((unlist(R0_posterior))) # TODO The data that will be used further in the model

rm(R0_plot,R0_posterior,date_range,end_date,start_date)



### Custom summarisation function ======================

summarise_scenario <- function(simulation)
{
  S_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["S"]]))
  S_2 <- do.call(cbind.data.frame, S_2)
  colnames(S_2) <- paste0("Var", 1:nsim)
  columns <- c(paste0("Var", 1:nsim))
  S <-  S_2 %>% 
    mutate(Mean_S = rowMeans(.[columns]), 
           SD_S = rowSds(as.matrix(.[columns])),
           Median_S = apply(S_2[, columns], 1, median)) %>% 
    mutate(Index = 1:length(S_2$Var1))%>% 
    select(Index, Mean_S, SD_S, Median_S) %>% 
    mutate(Date = startDate + Index - 1)
  rm(S_2)
  
  E_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["E"]]))
  E_2 <- do.call(cbind.data.frame, E_2)
  colnames(E_2) <- paste0("Var", 1:nsim)
  columns <- c(paste0("Var", 1:nsim))
  E <-  E_2 %>% 
    mutate(Mean_E = rowMeans(.[columns]), 
           SD_E = rowSds(as.matrix(.[columns])),
           Median_E = apply(E_2[, columns], 1, median)) %>% 
    mutate(Index = 1:length(E_2$Var1)) %>% 
    select(Index, Mean_E, SD_E, Median_E) %>% 
    mutate(Date = startDate + Index - 1)
  rm(E_2)
  
  Ic_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["Ic"]]))
  Ic_2 <- do.call(cbind.data.frame, Ic_2)
  colnames(Ic_2) <- paste0("Var", 1:nsim)
  columns <- c(paste0("Var", 1:nsim))
  Ic <-  Ic_2 %>% 
    mutate(Mean_Ic = rowMeans(.[columns]), 
           SD_Ic = rowSds(as.matrix(.[columns])),
           Median_Ic = apply(Ic_2[, columns], 1, median)) %>% 
    mutate(Index = 1:length(Ic_2$Var1)) %>% 
    select(Index, Mean_Ic, SD_Ic, Median_Ic) %>% 
    mutate(Date = startDate + Index - 1)
  rm(Ic_2)
  
  Isc_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["Isc"]]))
  Isc_2 <- do.call(cbind.data.frame, Isc_2)
  colnames(Isc_2) <- paste0("Var", 1:nsim)
  columns <- c(paste0("Var", 1:nsim))
  Isc <-  Isc_2 %>% 
    mutate(Mean_Isc = rowMeans(.[columns]), 
           SD_Isc = rowSds(as.matrix(.[columns])),
           Median_Isc = apply(Isc_2[, columns], 1, median)) %>% 
    mutate(Index = 1:length(Isc_2$Var1)) %>% 
    select(Index, Mean_Isc, SD_Isc, Median_Isc) %>% 
    mutate(Date = startDate + Index - 1)
  rm(Isc_2)
  
  R_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["R"]]))
  R_2 <- do.call(cbind.data.frame, R_2)
  colnames(R_2) <- paste0("Var", 1:nsim)
  columns <- c(paste0("Var", 1:nsim))
  R <-  R_2 %>% 
    mutate(Mean_R = rowMeans(.[columns]), 
           SD_R = rowSds(as.matrix(.[columns])),
           Median_R = apply(R_2[, columns], 1, median)) %>% 
    mutate(Index = 1:length(R_2$Var1)) %>% 
    select(Index, Mean_R, SD_R, Median_R) %>% 
    mutate(Date = startDate + Index - 1)
  rm(R_2)
  
  inc_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["incidence"]]))
  inc_2 <- do.call(cbind.data.frame, inc_2)
  colnames(inc_2) <- paste0("Var", 1:nsim)
  columns <- c(paste0("Var", 1:nsim))
  inc <-  inc_2 %>% 
    mutate(Mean_inc = rowMeans(.[columns]), 
           SD_inc = rowSds(as.matrix(.[columns])),
           Median_inc = apply(inc_2[, columns], 1, median)) %>% 
    mutate(Index = 1:length(inc_2$Var1)) %>% 
    select(Index, Mean_inc, SD_inc, Median_inc) %>% 
    mutate(Date = startDate + Index - 1)
  rm(inc_2)
  
  sub_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["subclinical"]]))
  sub_2 <- do.call(cbind.data.frame, sub_2)
  colnames(sub_2) <- paste0("Var", 1:nsim)
  columns <- c(paste0("Var", 1:nsim))
  sub <-  sub_2 %>% 
    mutate(Mean_sub = rowMeans(.[columns]), 
           SD_sub = rowSds(as.matrix(.[columns])),
           Median_sub = apply(sub_2[, columns], 1, median)) %>% 
    mutate(Index = 1:length(sub_2$Var1)) %>% 
    select(Index, Mean_sub, SD_sub, Median_sub) %>% 
    mutate(Date = startDate + Index - 1)
  rm(sub_2)
  
  consolidated <- full_join(S, E, by = "Date")
  consolidated <- full_join(consolidated, Ic, by = "Date")
  consolidated <- full_join(consolidated, Isc, by = "Date")
  consolidated <- full_join(consolidated, R, by = "Date")
  consolidated <- full_join(consolidated, inc, by = "Date")
  consolidated <- full_join(consolidated, sub, by = "Date")
  consolidated <- consolidated %>% 
    select(Date, Index.x, 
           Mean_S, SD_S, Median_S,
           Mean_E, SD_E, Median_E,
           Mean_Ic, SD_Ic, Median_Ic,
           Mean_Isc, SD_Isc, Median_Isc,
           Mean_R, SD_R, Median_R,
           Mean_inc, SD_inc, Median_inc,
           Mean_sub, SD_sub, Median_sub) %>% 
    rename(Index = Index.x) %>% 
    mutate(CumInc = cumsum(Mean_inc),
           CumInc_median = cumsum(Median_inc))
  
  rm(S, E, Ic, Isc, R, inc, sub)
  
  return(consolidated)
}

get_incidence <- function(simulation)
{
  inc_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["incidence"]]))
  inc_2 <- do.call(cbind.data.frame, inc_2)
  colnames(inc_2) <- paste0("Var", 1:nsim)
  inc_2$Index <- 1:length(inc_2$Var1)
  inc_2 <- inc_2 %>% 
    mutate(Date = startDate + Index - 1)%>% 
    gather(Var1:Var200, key = "Sim_No", value = "incidence")
  inc_2 <- inc_2 %>% 
    group_by(Sim_No) %>% 
    mutate(CumInc = cumsum(incidence))
  
  inc_3 <- inc_2 %>% 
    group_by(Index) %>% 
    summarise(Mean_inc = mean(incidence),
              SD_inc = sd(incidence),
              Median_inc = median(incidence),
              Max_inc = max(incidence),
              Min_inc = min(incidence),
              Q25_inc = quantile(incidence, 0.25),
              Q75_inc = quantile(incidence, 0.75),
              IQR_inc = IQR(incidence),
              Mean_cuminc = mean(CumInc),
              SD_cuminc = sd(CumInc),
              Median_cuminc = median(CumInc),
              Max_cuminc = max(CumInc),
              Min_cuminc = min(CumInc),
              Q25_cuminc = quantile(CumInc, 0.25),
              Q75_cuminc = quantile(CumInc, 0.75),
              IQR_cuminc = IQR(CumInc)) %>% 
    mutate(Date = startDate + Index - 1)
  
  inc <- inc_3 %>% 
    gather(Mean_inc:IQR_cuminc, key = "Indicator", value = "Value")
  rm(inc_2, inc_3)
  
  return(inc)
}

get_prevalence <- function(simulation)
{
  inc_2 <-  lapply(simulation, FUN = function(x) rowSums(x[["Ic"]]))
  inc_2 <- do.call(cbind.data.frame, inc_2)
  colnames(inc_2) <- paste0("Var", 1:nsim)
  inc_2$Index <- 1:length(inc_2$Var1)
  inc_2 <- inc_2 %>% 
    mutate(Date = startDate + Index - 1)%>% 
    gather(Var1:Var200, key = "Sim_No", value = "prevalence")
  
  inc_3 <- inc_2 %>% 
    group_by(Index) %>% 
    summarise(Mean_prev = mean(prevalence),
              SD_prev = sd(prevalence),
              Median_prev = median(prevalence),
              Max_prev = max(prevalence),
              Min_prev = min(prevalence),
              Q25_prev = quantile(prevalence, 0.25),
              Q75_prev = quantile(prevalence, 0.75),
              IQR_prev = IQR(prevalence)) %>% 
    mutate(Date = startDate + Index - 1)
  
  prev <- inc_3 %>% 
    gather(Mean_prev:IQR_prev, key = "Indicator", value = "Value")
  rm(inc_2, inc_3)
  
  return(prev)
}

### Number of Simulations per scenario ====================

nsim <- 200 # Set number of simulations to run for any scenario.


### Dealing with R0 ==================================

set.seed(123)
r0postCrI = r0posterior
# hist(r0postCrI)
# summary(r0postCrI)
r0postCrI <- r0posterior[r0posterior >= quantile(r0posterior, 0.25) & r0posterior <= quantile(r0posterior, 0.75)]
R0est = sample(x = r0postCrI,size = nsim)
# print(R0est)


### R0 is not that used by Kucharski ==================================
# set.seed(521)
# R0est <- get_sample_R0(m = 2.2, CI_Upper = 3.9, CI_Lower = 1.4, n = 5000)
# summary(R0est)
# setseed(123)
# R0est <- sample(x = R0est, size = nsim)

### Deriving the number of initial infected persons in the population =========================
library(tidyverse)
library(lubridate)
sk <- read_csv("data/india/SouthKorea.csv")
ind_test <- read_csv("data/india/India_testing.csv")

ind_test <- ind_test %>% 
  filter(Tests_per_million <= 250)
population_sk <- 51269000

sk <- sk %>% 
  mutate(Tests_per_million = Tests * 10^6 / population_sk,
         Cases_per_million = Cases * 10^6 / population_sk,
         Tests_pd_pm = Tests_per_day * 10^6 / population_sk,
         Index = 1:69,
         Date = dmy("01-02-2020") + Index - 1)

sk <- sk %>% 
  filter(Date >= dmy("17-02-2020") & Date <= dmy("17-03-2020"))

test_lm2 <- lm(Cases ~ Tests_per_million, data = ind_test)
test_lm2
summary(test_lm2)


Ind_ideal_testrates <- median(sk$Tests_per_million)

initialI_derived <- test_lm2$coefficients[2] * Ind_ideal_testrates - test_lm2$coefficients[1]
initialI_derived <- as.numeric(initialI_derived)
rm(test_lm2)

### Visualisations for estimating initial incidence ======================================
ggplot(data = sk, aes(x = Tests_per_million, y = Cases_per_million)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = sk, aes(x = Tests_per_day, y = Cases_per_day)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = sk, aes(x = Tests_per_million, y = Cases)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(sk, aes(x = Date, y = Tests_per_million)) +
  geom_line() +
  scale_x_date(breaks = breaks_pretty(length(unique(sk$Date))))+
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1))

ggplot(data = ind_test, aes(x = Tests_per_million, y = Cases)) +
  geom_point() +
  geom_smooth(method = "lm")

test_lm1 <- lm(Cases ~ Tests_per_million, data = ind_test)
test_lm1
summary(test_lm1)

test_lm2 <- lm(Cases ~ Tests_per_million, data = ind_test)
test_lm2
summary(test_lm2)


Ind_ideal_testrates <- median(sk$Tests_per_million)

initialI_derived <- test_lm2$coefficients[2] * Ind_ideal_testrates - test_lm2$coefficients[1]
initialI_derived <- as.numeric(initialI_derived)
rm(test_lm2)

### Post-processing functions - leave as is ====================================
summariseSimulations = function(VAR,CI,SIMS)
{
  temp = lapply(SIMS,FUN = function(x) rowSums(x[[VAR]]))
  temp1 = do.call(cbind.data.frame, temp)
  var_p_median = apply(temp1,1,function(x) quantile(x,0.5))
  var_p_lci = apply(temp1,1,function(x) quantile(x,(1-CI/100)/2))
  var_p_uci = apply(temp1,1,function(x) quantile(x,1-(1-CI/100)/2))
  SUMMARY = list(median = var_p_median,lci = var_p_lci,uci=var_p_uci)
  rm(temp,temp1,var_p_median,var_p_lci,var_p_uci)
  
  
  results = list(summary=SUMMARY, Sim1 = SIMS[[1]])
  return(results)
  
}

summarisePeakTimePeakSize= function(VAR = 'incidence',SIMS)
{
  time = SIMS[[1]]$time
  temp = lapply(SIMS,FUN = function(x) rowSums(x[[VAR]]))
  temp1 = do.call(cbind.data.frame, temp)
  peaksize = as.numeric(apply(temp1,2,max))
  peaktime = time[as.numeric(apply(temp1,2,function(x) which.max(x)))]
  results = list(time = time, peaktime = peaktime,peaksize = peaksize)
  return(results)
}

summariseSimulations_mid = function(CI,SIMS)
{
  temp = lapply(SIMS,FUN = function(x) rowSums(x[['S']]))
  temp1 = do.call(cbind.data.frame, temp)
  i = which.min(abs(as.numeric(temp1[nrow(temp1),])-as.numeric(quantile(temp1[nrow(temp1),],0.5))))
  j = which.min(abs(as.numeric(temp1[nrow(temp1),])-as.numeric(quantile(temp1[nrow(temp1),],0.25))))
  k = which.min(abs(as.numeric(temp1[nrow(temp1),])-as.numeric(quantile(temp1[nrow(temp1),],0.75))))
  
  S = data.frame(med = temp[[i]],lci = temp[[k]],uci = temp[[j]],time = SIMS[[1]]$time)
  S_age = list(med = SIMS[[i]]$S,lci = SIMS[[k]]$S,uci = SIMS[[j]]$S)
  inc = list(med = SIMS[[i]]$incidence,lci = SIMS[[k]]$incidence,uci = SIMS[[j]]$incidence)
  time = SIMS[[1]]$time
  N_age = SIMS[[1]]$N_age
  results = list(S=S,inc = inc, time = time,N_age=N_age,S_age = S_age)
  return(results)
}

summariseSimulationsAGE = function(VAR,CI,SIMS)
{
  var_p_median = var_p_lci = var_p_uci = array(NA,c(length(SIMS[[1]]$time),16))
  for(age in 1:16)
  {
    temp = lapply(SIMS,FUN = function(x) (x[[VAR]][,age]))
    temp1 = do.call(cbind.data.frame, temp)
    var_p_median[,age] = apply(temp1,1,function(x) quantile(x,0.5))
    var_p_lci[,age] = apply(temp1,1,function(x) quantile(x,(1-CI/100)/2))
    var_p_uci[,age] = apply(temp1,1,function(x) quantile(x,1-(1-CI/100)/2))
    rm(temp,temp1)
  }
  
  SUMMARY = list(median = var_p_median,lci = var_p_lci,uci=var_p_uci)
  rm(var_p_median,var_p_lci,var_p_uci)
  
  
  results = list(summary=SUMMARY, Sim1 = SIMS[[1]])
  return(results)
  
}


