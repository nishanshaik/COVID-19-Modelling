### Functions for the Model ===================================
## {ACKNOWLEDGEMENTS: Prem, Keisha; LSHTM. Codes downloaded form github}

## Function for population age structure
loadPopInfo = function(POP)
{
  pop = list()
  pop$N = sum(POP$popage)    # Population size 
  pop$p_age = POP$propage    # Population age structure of India
  return(pop)
}

### RUN THIS BLOCK BEFORE EACH SIMUMATION==============================

## TODO Load Interventions ----- Make amendments SOS. ## No changes necessary in NO LOCKDOWN scenario - the ideal base case assessment. 
loadInterventions = function(p_workopen)
{
  list(
    # constraints under a DO-NOTHING scenario 
    base =list(home = diag(1,16,16),
               work = diag(1,16,16),
               school = diag(1,16,16),
               others = diag(1,16,16)),
    # Region's lockdown--assume from XX Jan to XX Feb ## (Read Wuhan as the region under assessment) ##
    wuhanlockdown = list(home = diag(1,16,16),
                         work = diag(0.5,16,16),
                         school = diag(0.5,16,16),
                         others = diag(c(rep(0.5,4),rep(0.5,12)))),
    # TODO constraints under school closure + some social distancing for school-age going children but 100% workplace. This scenario could be India's base case given the potential lifting of lockdown on 14 April 2020.
    schcloseonly = list(home = diag(c(rep(1,4),rep(1,12))),
                        work = diag(1,16,16),
                        school = diag(0,16,16),
                        others = diag(c(rep(0.5,4),rep(1,12)))), 
    # constraints under work place distancing only (MAYBE UNREALISTIC, should close schools too) 
    # [Insufficient data to declare children as less susceptible, less infectious. Consider as equally infectious, equally susceptible to clinical infection. Ethics supercedes in status quo. ## NOT RECOMMENDED]
    workplacedistonly = list(home = diag(1,16,16),
                             work = diag(0.5,16,16),
                             school = diag(1,16,16),
                             others = diag(0.75,16,16)) ,
    
    # constraints under work place distancing + schoolclosure #[The workplace distancing measures are opted as part of the SEIcIscR function while running the simulation :: p_workopen]
    schcloseworkplacedist = list(home = diag(1,16,16),
                                 work = diag(p_workopen,16,16),
                                 school = diag(0,16,16),
                                 others = diag(c(rep(0.5,4),rep(0.5,12)))),
    # Post Outbeak, people still cautious # What happens when the interventions end and people get back to normal lives - R0 Post Outbreak has to be fitted with a mutually agreed number - This phase could see usual pre-outbreak mixing, with caution applied through behaviour change as less crowding, staggered office timings, hand hygiene, cough etiquette, etc. 
    postoutbreak = list(home = diag(1,16,16),
                        work = diag(1.0,16,16),
                        school = diag(1.0,16,16),
                        others = diag(c(rep(1.0,4),rep(1.0,12)))))
  
}


### Support Block ========================

## Get Beta ## Stable function, hence keep as is and sacrosanct. DO NOT ALTER.
getbeta = function(R0t,constraints,gamma,p_age,calculate_transmission_probability=1,CONTACTMATRIX = contacts)
{
  # 1) R0
  # 2) gamma = removal rate  
  # 3) f = population age proportion 
  # 4) constraints = a scale matrix contstraint age- and location-specific contact matrices (a linear combination over all locations; TODO to specify carefully based on interventions)
  # 5) calculate_transmission_probability if this is 1, then calculate the transmission probability from R0 otherwise, assume it is beta=0.05 
  # 6) npop = population size 
  
  # constraints for age-specific contacts at home, work, school, others
  n = 16 #length(p_age)
  constraints_base = list(home = diag(1,n),
                          work = diag(1,n), 
                          school = diag(1,n), 
                          others = diag(1,n)) # constraints under a DO-NOTHING scenario
  
  Csym <- lapply(CONTACTMATRIX, function(x, p_age) (x + t(x)*((p_age)%*%t(1/p_age)))/2, p_age) # make sure contacts are reciprocal
  CONTACTMATRIX=Csym
  C = constraints_base[[1]]%*%CONTACTMATRIX[[1]]+
    constraints_base[[2]]%*%CONTACTMATRIX[[2]]+
    constraints_base[[3]]%*%CONTACTMATRIX[[3]]+
    constraints_base[[4]]%*%CONTACTMATRIX[[4]]
  
  
  if (calculate_transmission_probability==1){
    M = C
    for(i in 1:n)
    {
      for(j in 1:n){
        M[i,j] = C[i,j]*p_age[i]/p_age[j]
      }
    }
    eig = eigen(M)
    beta = R0t*gamma/max(Re(eig$values))  # reverse engineer beta from the R0 and gamma 
    beta = beta
  }else{
    beta = 0.025#0.05
  }
  results = list(beta)
  names(results) =c('beta')
  return(results)
}

## SEIcIscR Simulator function tl;dr --- --- 

simulateOutbreakSEIcIscR = function(R0t,
                                    rho = c(rep(0.31,16)), # is the probability that an exposed person could be symptomatic. Based on the best available evidence, this can be changed. EXPLAIN
                                    Case_Fatality_Rate = 0.04,
                                    R0tpostoutbreak = 1.5, # The R0 we assume would be after people start getting back to normal lives. [Could be played around with to explore changes!]
                                    dateEndIntenseIntervention, # date we begin relaxing intense intervention 
                                    pWorkOpen = c(0.1, 0.33, 0.67, 1), # pWorkOpen: proportion of the work force that is working (will be time-varying)
                                    dateStartSchoolClosure = as.Date('2020-03-14'), # cause winter term break 
                                    dateStartIntenseIntervention = as.Date('2020-03-25'), # Intense intervention: starts at Wuhan Lockdown
                                    dateStart, # Date of outbreak - Best is to set this date as 2 March 2020 than Early February.
                                    POP = indiapop, # Enter the population dataset - indiapop for India.
                                    numWeekStagger = c(3, 6, 9),
                                    pInfected, # Poportion of indiviuals infected at the beginning of the outbreak [Could be less than 100 for 1.3 billion. as on 2 March, we had 2 confirmed cases]
                                    durInf = 10, # Duration of infectious period - choose based on best evidence. Diamond Princess data could be valuable here. 
                                    contacts_china = contacts, # No need to change this, just feeds in the earlier contact matrices into the simulation. 
                                    isolation_efficiency = 1) # The percentage of symptomatic cases that the health system can identify and isolate. Affects force of infection. 
{
  # debug dateStartIntenseIntervention = as.Date('2020-01-23')  
  # debug dateEndIntenseIntervention = as.Date('2020-03-01')
  # debug R0est = rep(2,3660) 
  # debug rho = rep(0.8,3660) 
  # debug pWorkOpen =  c(0.1,0.25,0.5,1)
  
  
  # Load population information
  # pop = loadPopInfo(POP = indiapop)
  pop = list()
  pop$N = sum(POP$popage)
  pop$p_age = POP$propage
  N_age = pop$N*pop$p_age                                        # Population age structure (in numbers)
  # contacts_china = CONTACTS
  
  
  # Specify epidemiological info
  durLat = 5.2;   	                                             # TODO Mean latent period (days) from DOI: 10.1056/NEJMoa2002032
  durInf = durInf;                                               # Mean duration of infectiousness (days)
  gamma = 1-exp(-1/durInf);                                      # removal rate
  alpha = 1-exp(-1/durLat);                                      # infection rate
  dt = 1;                                                        # Time step (days)
  tmax = 1095;                                                   # TODO Time horizon (days) Till 31 Dec 2020.
  numSteps = tmax/dt;  	                                         # Total number of simulation time steps
  # dateStart = as.Date('2019-12-01')                            # included as a function argument 
  CFR = Case_Fatality_Rate
  dateEnd = dateStart + (tmax-1)
  isolation_efficiency = isolation_efficiency
  isolation_gap = 1 - isolation_efficiency
  dateStartCNY = as.Date('2020-04-01')                           # Chinese New Year, this can be omitted
  dateEndCNY = as.Date('2020-04-01')                             # Chinese New Year, this can be omitted
  
  # Declare the state variables and related variables:
  # The values of these variables change over time
  S = E = Isc = Ic = R = array(0,c(numSteps,length(pop$p_age)))
  lambda = incidence = subclinical = cumulativeIncidence = totalIncidence = array(0,c(numSteps,length(pop$p_age)))
  deaths = prevalence = array(0,c(numSteps,length(pop$p_age)))
  time = array(0,numSteps)
  
  
  # Initialise the time-dependent variables, i.e. setting the values of the variables at time 0
  E[1,] = 0 
  Ic[1,] =  pInfected*sum(N_age)*0.821/16 #rpois(length(N_age),lambda = pInfected*sum(N_age)/16)  # 100 # Assign 100 infected person in each age group (TODO RELAX?)
  Isc[1,] = pInfected*sum(N_age)*(1-0.821)/16 
  R[1,] = 0 
  S[1,] = N_age - E[1,] - Ic[1,] - Isc[1,] - R[1,]
  incidence[1,] = pInfected*sum(N_age)*0.821/16;
  subclinical[1,] = 0;
  totalIncidence[1,] = pInfected*sum(N_age)/16;
  cumulativeIncidence[1,] = pInfected*sum(N_age)*0.821/16;
  deaths[1,] = 0;
  prevalence[1, ] = pInfected*sum(N_age)*0.821/16;
  time[1] = 0;
  
  
  ## INTERVENTIONS 
  # School closed 2020-02-10, lockdown (intense intervention) started 2020-01-23, end of intense intervention: user-specified 
  # note that intense intervention is time-varying control by pWorkOpen: proportion of the work force that is working
  # debug pWorkOpen = c(0.1,0.25,0.5,1)
  tStartSchoolClosure = as.vector(dateStartSchoolClosure - dateStart)+1
  tStartIntenseIntervention = as.vector(dateStartIntenseIntervention - dateStart)+1 # for pw = 0.1
  tEndIntenseIntervention = as.vector(dateEndIntenseIntervention - dateStart)+1     # for pw = 0.1
  tRelaxIntervention1 = tEndIntenseIntervention + numWeekStagger[1]*7                               # for pw = 0.25
  tRelaxIntervention2 = tEndIntenseIntervention + numWeekStagger[2]*7                               # for pw = 0.5
  tRelaxIntervention3 = tEndIntenseIntervention + numWeekStagger[3]*7                               # for pw = 1
  # tStartEndClosure = as.vector(dateEndSchoolClosure - dateStart)+1
  pwork = array(1,numSteps)
  pwork[1:tRelaxIntervention3] =c(rep(1,(tStartIntenseIntervention-0)), # dont know there is outbreak 
                                  rep(pWorkOpen[1],(tEndIntenseIntervention-tStartIntenseIntervention)),
                                  rep(pWorkOpen[2],(tRelaxIntervention1-tEndIntenseIntervention)),
                                  rep(pWorkOpen[3],(tRelaxIntervention2-tRelaxIntervention1)),
                                  rep(pWorkOpen[4],(tRelaxIntervention3-tRelaxIntervention2)))
  R0tpostoutbreak = R0t #overwrites the default reduction in R0 post-outbreak
  beta = getbeta(R0t = R0t,constraints = constraintsIntervention$base,gamma = gamma,p_age = pop$p_age)
  if(pWorkOpen[2]<1) beta_postfirstwave = getbeta(R0t = R0tpostoutbreak,constraints = constraintsIntervention$base,gamma = gamma,p_age = pop$p_age)
  if(pWorkOpen[2]>=1) beta_postfirstwave = beta#getbeta(R0t = R0t[2],constraints = constraintsIntervention$base,gamma = gamma,p_age = pop$p_age)
  for (stepIndex in 1: (numSteps-1))
  { 
    
    # load plausible intervetions 
    constraintsIntervention = loadInterventions(p_workopen = pwork[stepIndex])
    
    ## Age- and location-specific contact rates for the given interventions 
    
    # I0: before school winter break intervention period, use base-case
    if(time[stepIndex] < tStartSchoolClosure)  
    {
      CONSTRAINT = constraintsIntervention$base
    }
    # I1:  When school winter break but before lockdown period, use 'schcloseonly'
    if(time[stepIndex] >= tStartSchoolClosure & time[stepIndex] < tStartIntenseIntervention) 
    {
      INTERVENTION = "schcloseonly"   
      CONSTRAINT = constraintsIntervention[[INTERVENTION]] 
    }  
    # I2:  Intense intervention
    if(time[stepIndex] >= tStartIntenseIntervention & time[stepIndex] < tRelaxIntervention3) 
    {
      INTERVENTION = "schcloseworkplacedist"   
      CONSTRAINT = constraintsIntervention[[INTERVENTION]] 
    }  
    # I3: post outbreak 
    if(time[stepIndex] >= tRelaxIntervention3)  
    {
      CONSTRAINT = constraintsIntervention$postoutbreak
    }
    # 
    
    C = CONSTRAINT[[1]]%*%contacts_china[[1]]+
      CONSTRAINT[[2]]%*%contacts_china[[2]]+
      CONSTRAINT[[3]]%*%contacts_china[[3]]+
      CONSTRAINT[[4]]%*%contacts_china[[4]]
    
    # calculate the force of infection
    
    # beta = getbeta(R0t = R0t[stepIndex], constraints = constraintsIntervention$base, gamma = gamma, p_age = pop$p_age)
    if(time[stepIndex] < tEndIntenseIntervention+0) 
      lambda[stepIndex,] = as.numeric(beta)* 
      (as.matrix(C)%*%
         (isolation_gap * as.matrix(Ic[stepIndex,] / N_age) + 
            as.matrix(Isc[stepIndex,] / N_age)));
    if(time[stepIndex] >= tEndIntenseIntervention+0)
      lambda[stepIndex,] = as.numeric(beta_postfirstwave)*
      (as.matrix(C)%*%
         (isolation_gap * as.matrix(Ic[stepIndex,]/N_age) + 
            as.matrix(Isc[stepIndex,]/N_age)));
    # calculate the number of infections and recoveries between time t and t+dt
    
    numStoE   = lambda[stepIndex,]*S[stepIndex,]*dt;                  # S to E
    numEtoIc  = alpha*rho*E[stepIndex,]*dt;                           # E to Ic
    numEtoIsc = alpha*(1-rho)*E[stepIndex,]*dt;                       # E to Isc
    numIctoR  = gamma*Ic[stepIndex,]*dt;                              # Ic to R
    numIsctoR = gamma*Isc[stepIndex,]*dt;                             # Isc to R
    
    # Difference equations 
    S[stepIndex+1,]   = S[stepIndex,]-numStoE;
    E[stepIndex+1,]   = E[stepIndex,]+numStoE-numEtoIc-numEtoIsc;
    Ic[stepIndex+1,]  = Ic[stepIndex,]+numEtoIc-numIctoR;
    Isc[stepIndex+1,] = Isc[stepIndex,]+numEtoIsc-numIsctoR;
    R[stepIndex+1,]   = R[stepIndex,]+numIctoR+numIsctoR;
    
    incidence[stepIndex+1,] = numEtoIc/dt;
    subclinical[stepIndex+1,] = numEtoIsc/dt;
    totalIncidence[stepIndex+1,] = (numEtoIc/dt) + (numEtoIsc/dt);
    cumulativeIncidence[stepIndex+1, ] = cumulativeIncidence[stepIndex,] + numEtoIc/dt;
    deaths[stepIndex+1, ] = (deaths[stepIndex, ] + numIctoR) * CFR;
    prevalence[stepIndex+1, ] = prevalence[stepIndex, ] + numEtoIc - numIctoR;
    time[stepIndex+1] = time[stepIndex]+dt;
    
    
  }
  output = list(S = S, 
                E = E, 
                Ic = Ic, 
                Isc = Isc, 
                R = R, 
                time = time, 
                lambda=lambda,
                incidence = incidence, 
                N_age= N_age, 
                subclinical = subclinical,
                totalIncidence = totalIncidence,
                cumulativeIncidence = cumulativeIncidence,
                deaths = deaths,
                prevalence = prevalence,
                R0t = R0t,#rho = rho,
                dateStart = dateStart, 
                dateEnd = dateEnd,
                dateStartIntenseIntervention = dateStartIntenseIntervention, 
                dateEndIntenseIntervention = dateEndIntenseIntervention,
                dateStartSchoolClosure = dateStartSchoolClosure, 
                dateStartCNY = dateStartCNY,
                dateEndCNY = dateEndCNY)
  return(output)
}

## TODO Function to fecth R0 from median and 95% CI
get_sample_R0 <- function(m, CI_Upper, CI_Lower, n)
{
  k <- (m / (CI_Upper - CI_Lower))^ 2  # Finds the shape parameter
  lambda <- (m / ((CI_Upper - CI_Lower)^2)) # Finds the rate parameter
  R0 <- rgamma(n, shape = k, rate = lambda) # Takes a random sample from a gamma distibution with the abover derived k and lambda
  return(R0)
}

### Defining the Scenarios ===============================

# Restricting to one scenario per page - tweaked fucntions for each scenario - Run separately. 
scenario_4 = vector('list',nsim)      # Define Scenario_0: Current lockdown with only 50% lockdown efficiency to include the migrations that have happened and the lockdown violations rampant in the country. Exit strategy of 3-weekly staggers of 65%, 85% and 100%. 

### Running the Simulations ===========================
durInfSim = 10 # Duration of infection is around 10 days.
initialI = initialI_derived / sum(indiapop$popage)
for(sim in 1:nsim)
{
  scenario_4[[sim]] = simulateOutbreakSEIcIscR(R0t = R0est[sim],
                                               dateStart = as.Date("2020-03-02"),
                                               dateStartIntenseIntervention = as.Date('2020-03-25'), 
                                               dateEndIntenseIntervention = as.Date('2020-05-03'),
                                               pWorkOpen = c(0.5, 0.65, 0.85, 1),
                                               numWeekStagger = c(3, 6, 9),
                                               pInfected = initialI,
                                               durInf = durInfSim,
                                               isolation_efficiency = 0.5)
  
  if(sim%%10 == 0) print(paste0('Done with simulation ',sim))
}
