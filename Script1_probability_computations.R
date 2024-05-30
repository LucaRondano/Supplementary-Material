library(mvtnorm)

#######################################################################
# Functions
#######################################################################
# Alpha spending functions
OBrien_Fleming_alpha<-function(I, alpha)
  return(2-2*pnorm(qnorm(1-alpha/2)/sqrt(I)))

Pocock_alpha<-function(I, alpha)
  return(alpha*log(1+(exp(1)-1)*I))

# Treshold for statistical significance at the final analysis
efficacy_boundary_function<-function(alpha_int, alpha_fin, theta_int_eff)
{
  theta_fin_suc<-theta_int_eff
  while(alpha_fin - alpha_int >
        pmvnorm(lower=c(-Inf, theta_fin_suc),
                upper=c(theta_int_eff, Inf),
                mean=c(0, 0), sigma=sigma)[1])
    theta_fin_suc<-theta_fin_suc-0.0001
  
  return(theta_fin_suc+0.0001)
}

# Find intersection between functions
intersection_function<-function(vec1=PoS, vec2=PoS_post)
{
  return(which.min(abs(vec1-vec2)))
}

# Find confidence interval for simulated power
probability_interval_function<-function(vec, gamma)
{
  AUC<-0
  i<-1
  while(AUC<(1-gamma)/2)
  {
    AUC<-AUC+vec[i]*width
    i<-i+1
  }
  left<-intervals[i-1]
  
  AUC<-0
  i<-length(vec)
  while(AUC<(1-gamma)/2)
  {
    AUC<-AUC+vec[i]*width
    i<-i-1
  }
  right<-intervals[i+1]
  
  return(c(left,right))
}

# Find futility for an acceptable choice of PoS lost
find_futility_bound_for_PoS_spent<-function(vec1=PoS, vec2=theta_int_fut, PoS_okToLose)
{
  if(PoS_okToLose>vec1[1])
    stop("PoS_okToLose is greater than maximum PoS")
  
  first_above<-length(vec1)
  for(i in 1:first_above)
  {
    if(vec1[1]-vec1[i] > PoS_okToLose)
    {
      first_above<-i
      break
    }
  }
  
  return(vec2[first_above])
}

#######################################################################
# Fixed parameters
#######################################################################
# Study parameters
power<-0.9
alpha<-0.025
sd<-1
delta<-0.3

# patients per arm (total sample size = 2n)
n_pow<-2*sd^2/delta^2*((qnorm(1-alpha)+qnorm(power)))^2
n_fin<-234
n_int<-117

#n_int<-47 #20% Information Fraction
#n_int<-70 #30% Information Fraction
#n_int<-94 #40% Information Fraction
#n_int<-117 #50% Information Fraction
#n_int<-140 #60% Information Fraction
#n_int<-164 #70% Information Fraction
#n_int<-187 #80% Information Fraction

I<-n_int/n_fin

# sd of test statistics and covariance matrix of interim analysis/final outcome
sd_fin<-sqrt(2*sd^2/n_fin)
sd_int<-sqrt(2*sd^2/n_int)
sigma<-matrix(c(sd_int^2, sd_fin^2, sd_fin^2, sd_fin^2), nrow=2)

# Normal prior parameters
n0<-10
sd_prior<-sqrt(2*sd^2/n0)

# Unconditional test statistic
sd_fin_U<-sqrt(2*sd^2*(n_fin+n0)/(n_fin*n0))
sd_int_U<-sqrt(2*sd^2*(n_int+n0)/(n_int*n0))
sigma_U<-matrix(c(sd_int_U^2, sd_fin_U^2, sd_fin_U^2, sd_fin_U^2), nrow=2)

#######################################################################
# Boundary for efficacy
#######################################################################

# Alpha spent at interim ##############################################
#alpha_int<-0                                #No early stop for efficacy
alpha_int<-OBrien_Fleming_alpha(I, alpha)   #O'Brien Fleming type efficacy bound
#alpha_int<-Pocock_alpha(I, alpha)           #Pocock type efficacy boundary 

# Efficacy and final boundaries
if(alpha_int==0)
{
  theta_fin_suc<-qnorm(1-alpha)*sd_fin
  theta_int_fut_limit<-0.4
}else
{
  theta_int_eff<-qnorm(1-alpha_int)*sd_int
  theta_fin_suc<-efficacy_boundary_function(alpha_int, alpha, theta_int_eff)
  
  theta_int_fut_limit<-round(theta_int_eff, 4)
}

#######################################################################
# Probabilities
#######################################################################
step<-10^(-4)
theta_int_fut<-seq(-0.25, theta_int_fut_limit-step, step)

for(theta0 in c(delta-0.2, delta, delta+0.2))
  {
  if(alpha_int==0)
    {
    P_stop_for_eff<-0
    P_not_early_stop<-pnorm(theta_int_fut, mean=theta0, sd=sd_int_U, lower.tail = FALSE)
    P_success_final_analysis<-c()
    for(i in 1:length(theta_int_fut))
      P_success_final_analysis<-c(P_success_final_analysis,
                                  pmvnorm(lower=c(theta_int_fut[i], theta_fin_suc),
                                  upper=c(Inf, Inf),
                                  mean=c(theta0, theta0), sigma=sigma_U)[1])
  
    PoS<-P_stop_for_eff + P_success_final_analysis
  
    PoS_post<-P_success_final_analysis/P_not_early_stop
    }else
      {
      P_stop_for_eff<-pnorm(theta_int_eff, mean=theta0, sd=sd_int_U, lower.tail = FALSE)
      
      P_not_early_stop<-pnorm(theta_int_eff, mean=theta0, sd=sd_int_U)-
                        pnorm(theta_int_fut, mean=theta0, sd=sd_int_U)
      
      P_success_final_analysis<-c()
      for(i in 1:length(theta_int_fut))
        P_success_final_analysis<-c(P_success_final_analysis,
                                pmvnorm(lower=c(theta_int_fut[i], theta_fin_suc),
                                        upper=c(theta_int_eff, Inf),
                                        mean=c(theta0, theta0), sigma=sigma_U)[1])
      
      PoS<-P_stop_for_eff + P_success_final_analysis
      
      PoS_post<-P_success_final_analysis/P_not_early_stop
      }

  #make a matrix with Probabilities from the 3 scenarios
  if(theta0==delta-0.2)
  {
  P_stop_for_eff_M<-matrix(P_stop_for_eff, ncol=1)
  P_not_early_stop_M<-matrix(P_not_early_stop, ncol=1)
  P_success_final_analysis_M<-matrix(P_success_final_analysis, ncol=1)
  PoS_M<-matrix(PoS, ncol=1)
  PoS_post_M<-matrix(PoS_post, ncol=1)
  }else
    {
    P_stop_for_eff_M<-cbind(P_stop_for_eff_M, P_stop_for_eff)
    P_not_early_stop_M<-cbind(P_not_early_stop_M, P_not_early_stop)
    P_success_final_analysis_M<-cbind(P_success_final_analysis_M, P_success_final_analysis)
    PoS_M<-cbind(PoS_M, PoS)
    PoS_post_M<-cbind(PoS_post_M, PoS_post)
    }
  }
