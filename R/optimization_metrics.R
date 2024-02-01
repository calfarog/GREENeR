# 'br2': Weighted R2
# Coef. of  determination multiplied by the coef. of the regression line#
# This index allows accounting for the discrepancy in the magnitude of two signals
# under or overpredictions, (depicted by 'b') as well as their dynamics (depicted by R2).
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': weighted R2 between 'sim' and 'obs'
#' @importFrom stats coefficients cor lm
br2 <- function (sim, obs, na.rm=TRUE){

  x.lm <- stats::lm(sim ~ obs - 1)

  b <- as.numeric(stats::coefficients(x.lm)["obs"]   )

  r2 <- (rPearson(sim, obs))^2

  br2 <- ifelse(b <= 1, r2*abs(b), r2/abs(b))

  return(br2)

}

# 'P': Coefficient of Persistence
# PME ranges from 0 to 1, with PME = 1 being the optimal value.
# PME values should be larger than 0.0 to indicate a minimally acceptable
# model performance (Gupta et al., 1999
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Persistence Index Efficiency between 'sim' and 'obs'
cp <- function (sim, obs, na.rm=TRUE){

  n <- length(obs)

  denominator <- sum( ( obs[2:n] - obs[1:(n-1)] )^2 )

  if (denominator != 0) {

    cp <- ( 1 - ( sum( (obs[2:n] - sim[2:n])^2 ) / denominator ) )

  } else stop("'sum((obs[2:n]-obs[1:(n-1))^2)=0', it is not possible to compute 'P'")

  return(cp)

}

# 'IoA': Index of Agreement
# Index of Agreement (Willmott et al., 1984) range from 0.0 to 1.0
# and the closer to 1 the better the performance of the model
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Index of Agreement between 'sim' and 'obs'
d <- function (sim, obs, na.rm=TRUE){

  Om <- mean(obs)

  denominator <- sum( ( abs(sim - Om) + abs(obs - Om)  )^2 )

  if (denominator != 0) {

    d <- 1 - ( sum( (obs - sim)^2 ) / denominator )

  } else stop("'sum((abs(sim-Om)+abs(obs-Om))^2)=0', it is not possible to compute 'IoA'")

  return(d)

}

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
gof <- function(sim, obs, na.rm=TRUE, digits=2){
  ME     <- me(sim, obs, na.rm=na.rm)
  MAE    <- mae(sim, obs, na.rm=na.rm)
  MSE    <- mse(sim, obs, na.rm=na.rm)
  RMSE   <- rmse(sim, obs, na.rm=na.rm)
  NRMSE  <- nrmse(sim, obs, na.rm=na.rm)
  RSR    <- rsr(sim, obs, na.rm=na.rm)
  rSD    <- rSD(sim, obs, na.rm=na.rm)
  PBIAS  <- pbias(sim, obs, na.rm=na.rm)
  NSE    <- NSE(sim, obs, na.rm=na.rm)
  mNSE   <- mNSE(sim, obs, na.rm=na.rm)
  rNSE   <- rNSE(sim, obs, na.rm=na.rm)
  d      <- d(sim, obs, na.rm=na.rm)
  md     <- md(sim, obs, na.rm=na.rm)
  rd     <- rd(sim, obs, na.rm=na.rm)
  cp     <- cp(sim, obs, na.rm=na.rm)
  r      <- rPearson(sim, obs)
  bR2    <- br2(sim, obs, na.rm=na.rm)
  KGE    <- KGE(sim, obs, na.rm=na.rm)
  VE     <- VE(sim, obs, na.rm=na.rm)
  R2 <- r^2

  gof <- rbind(ME, MAE, MSE, RMSE, NRMSE, PBIAS, RSR, rSD, NSE, mNSE, rNSE, d, md, rd, cp, r, R2, bR2, KGE, VE)

  # rownames(gof)[5] <- "NRMSE %"
  # rownames(gof)[6] <- "PBIAS %"

  gof <- round(gof, digits)

  return(gof)

}

# 'KGE': Kling-Gupta Efficiency
# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 's'   : scaling factors.
# 'Result': Kling-Gupta Efficiency between 'sim' and 'obs'
KGE <- function(sim, obs, s=c(1,1,1), na.rm=TRUE,
                        method="2009") {

  # Mean values
  mean.sim <- mean(sim, na.rm=na.rm)
  mean.obs <- mean(obs, na.rm=na.rm)

  # Standard deviations
  sigma.sim <- sd(sim, na.rm=na.rm)
  sigma.obs <- sd(obs, na.rm=na.rm)

  # Pearson product-moment correlation coefficient
  r <- rPearson(sim, obs)

  # Alpha is a measure of relative variability between simulated and observed values (See Ref1)
  Alpha <- sigma.sim / sigma.obs

  # Beta is the ratio between the mean of the simulated values to the mean of observations
  Beta <- mean.sim / mean.obs

  # CV.sim is the coefficient of variation of the simulated values [dimensionless]
  # CV.obs is the coefficient of variation of the observations [dimensionless]
  CV.sim <- sigma.sim / mean.sim
  CV.obs <- sigma.obs / mean.obs

  # Gamma is the variability ratio, which is used instead of Alpha (See Ref2)
  Gamma <- CV.sim / CV.obs

  # Variability ratio depending on 'method'
  if(method=="2012") {
    vr     <- Gamma
    vr.stg <- "Gamma"
  } else {
    vr     <- Alpha
    vr.stg <- "Alpha"
  } # ELSE end

  # KGE Computation
  if ( (mean.obs != 0) | (sigma.obs != 0) ) {
    KGE <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2 )
  } else {
    if ( mean.obs != 0)  warning("Warning: 'mean(obs)==0'. Beta = Inf")
    if ( sigma.obs != 0) warning("Warning: 'sd(obs)==0'. ", vr.stg, " = Inf")
    KGE <- NA
  }

  out <- KGE

  return(out)

}

# 'mae': Mean Absolute Error
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
mae <- function(sim, obs, na.rm = TRUE){
  mae <- mean( abs(sim - obs), na.rm = na.rm)

  return(mae)
}

# 'md': Modified Index of Agreement
# Index of Agreement (Willmott et al., 1984) range from 0.0 to 1.0
# and the closer to 1 the better the performance of the model
# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Modified Index of Agreement between 'sim' and 'obs'
md <- function (sim, obs, j=1, na.rm=TRUE){

  Om <- mean(obs)

  denominator <- sum( ( abs(sim - Om) + abs(obs - Om)  )^j )

  if (denominator != 0) {

    d1 <- 1 - ( sum( ( abs(obs - sim) )^j ) / denominator )

  } else {
    d1 <- NA
    warning("'sum((abs(sim-Om)+abs(obs-Om))^j)=0', it is not possible to compute 'md'")
  }

  return(d1)

}

# 'me': Mean Error
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
me <- function(sim, obs, na.rm = TRUE){
  me <- mean( sim - obs, na.rm = na.rm)

  return(me)
}

# 'mNSE': Modified Nash-sutcliffe Efficiency
# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'
mNSE <- function (sim, obs, j=1, na.rm=TRUE){

  denominator <- sum( abs(obs - mean(obs))^j )

  if (denominator != 0) {

    NS1 <- 1 - ( sum( abs(obs - sim)^j ) / denominator )

  } else {
    NS1 <- NA
    warning("'sum(abs(obs - mean(obs))^j)=0', it is not possible to compute 'mNSE'")
  }

  return(NS1)

}

# 'mse': Mean Squared Error
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
mse <- function(sim, obs, na.rm = TRUE){
  mse <- mean( (sim - obs)^2, na.rm = na.rm)

  return( mse )
}

# 'nrmse': Normalized Root Mean Square Error
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
nrmse <- function(sim, obs, na.rm=TRUE){
  cte <- sd(obs, na.rm=na.rm)

  rmse <- rmse(sim, obs, na.rm)

  if (max(obs, na.rm= na.rm) - min(obs, na.rm= na.rm) != 0) {

    nrmse <- rmse / cte

  } else {
    nrmse <- NA
    warning("'obs' is constant, it is not possible to compute 'nrmse'")
  } # ELSE end

  return( round( 100*nrmse, 1) )
}

# 'NSE': Nash-sutcliffe Efficiency
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
NSE <- function(sim, obs, na.rm=TRUE){
  denominator <- sum( (obs - mean(obs))^2 )

  if (denominator != 0) {

    NS <- 1 - ( sum( (obs - sim)^2 ) / denominator )

  } else {
    NS <- NA
    warning("'sum((obs - mean(obs))^2)=0' => it is not possible to compute 'NSE'")
  } # ELSE end

  return(NS)
}

# 'pbias': Percent Bias
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
pbias <- function (sim, obs, na.rm=TRUE){

  n <- length(obs)

  denominator <- sum( obs )

  if (denominator != 0) {

    pbias <- 100 * ( sum( sim - obs ) / denominator )

  } else {
    pbias <- NA
    warning("'sum((obs)=0', it is not possible to compute 'pbias'")
  }

  return( round(pbias, 1) )

}

# 'rd': Relative Index of Agreement
# Relative Index of Agreement (Willmott et al., 1984) range from 0.0 to 1.0
# and the closer to 1 the better the performance of the model
# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Modified Index of Agreement between 'sim' and 'obs'
# This index was developed to  be sensitive to systematic over- or
# under-prediction, in particular during low flow conditions.
# This index quantify the difference between simulated and observed values
# in a relative way, in order to significatively reduce the influence of
# the absolute differences of high flows and to give more weight to
# over- or under-prediction of low flows. At the same time, differences in
# low flows become more important, because they are looked in a relative way.
rd <- function (sim, obs, na.rm=TRUE){

  Om <- mean(obs)

  denominator <- sum( ( ( abs(sim - Om) + abs(obs - Om) ) / Om )^2 )

  if (denominator != 0) {

    rd <- 1 - ( sum( ( (obs - sim) / obs)^2 ) / denominator )

  } else {
    rd <- NA
    warning("'sum( ( ( abs(sim-Om) + abs(obs-Om) ) / Om )^2 ) = 0', it is not possible to compute 'rd'")
  }

  return(rd)

}

# 'rmse': Root Mean Square Error
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
rmse <- function(sim, obs, na.rm=TRUE){
  rmse <- sqrt(mean((sim - obs)^2, na.rm = na.rm))

  return(rmse)
}

# 'rNSE': Relative Nash-sutcliffe Efficiency
# Nash-Sutcliffe efficiency not "inflated" by squared values
# Essentially, the closer the model efficiency is to 1, the more accurate the model is.
# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'
rNSE <- function (sim, obs, na.rm=TRUE){

  zero.index <- which(obs==0)
  if (length(zero.index > 0) ) {
    warning("'rNSE' can not be computed: some elements in 'obs' are zero !", call.=FALSE)
  }

  denominator <- sum( ( ( obs - mean(obs) ) / mean(obs) )^2 )

  if (denominator != 0) {

    rNSE <- 1 - ( sum( ( (obs - sim) / obs )^2 ) / denominator )

  } else {
    rNSE <- NA
    warning("'sum( ( ( obs - mean(obs) ) / mean(obs) )^2 ) = 0', it is not possible to compute 'rNSE'")
  }

  return(rNSE)

}

#   rPearson
# The 'r.Pearson' coefficient ranges from −1 to 1.
# A value of 1 shows that a linear equation describes the relationship
# perfectly and positively, with all data points lying on the same line
# and with Y increasing with X.
# A score of −1 shows that all data points lie on a single line but
# that Y increases as X decreases.
# A value of 0 shows that a linear model is not needed – that there
# is no linear relationship between the variables.
# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
#' @importFrom stats coefficients cor lm
rPearson <- function(sim, obs, na.rm=TRUE) {

  rPearson <- stats::cor(sim, obs, method="pearson", use="pairwise.complete.obs")

  return(rPearson)

}

# 'rSD': Ratio of Standard Deviations
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
rSD <- function (sim, obs, na.rm=TRUE){

  denominator <- sd(obs, na.rm = na.rm)

  if (denominator != 0) {

    rSD <- sd(sim, na.rm= na.rm) / sd(obs, na.rm= na.rm)

  } else {
    rSD <- NA
    warning("'sd(obs)=0', it is not possible to compute 'rSD'")
  }

  return(rSD)

}

# 'rsr': Ratio of RMSE to the Standard Deviation of the Observations
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
rsr <- function(sim, obs, na.rm=TRUE){
  rmse <- rmse(sim=sim, obs=obs, na.rm=na.rm)

  #Standard deviation of the observations
  sd.obs <- sd(obs, na.rm=na.rm)

  if ( sd.obs > 0 ) {

    rsr <- rmse / sd.obs

  } else {
    rsr <- NA
    warning("'sd(obs)=0', it is not possible to compute 'RSR'")
  } # ELSE end

  return( rsr )
}

# 'VE': Volumetric Efficiency
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Mean Absolute Error between 'sim' and 'obs', in the same units of 'sim' and 'obs'
VE <- function (sim, obs, na.rm=TRUE){

  denominator <- sum(obs, na.rm=na.rm)

  if (denominator != 0) {
    ve <- 1 - ( sum( abs(sim-obs) ) / denominator )
  } else {
    ve <- NA
    warning("'sum((obs)=0' => it is not possible to compute 'VE'")
  }

  return(ve)

}
