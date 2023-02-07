\name{SLCARE}
\alias{SLCARE}
\title{Semiparametric Latent Class Analysis for Recurrent Event}
\usage{
SLCARE(
  alpha = NULL,
  beta = NULL,
  dat,
  K = NULL,
  gamma = 0,
  max_epoches = 500,
  conv_threshold = 0.01,
  boot = NULL)
}
\description{
Conduct Semiparametric Latent Class Analysis for Recurrent Event.
}
\arguments{
  \item{alpha}{initial values for alpha for estimation procedure. This should be NULL or a numberic matirx. NULL means obtain initial value with k-means.}
  \item{beta}{initial value for beta for estimation procedure. This should be NULL or a numberic matirx. NULL means obtain initial value with k-means.}
  \item{dat}{a data frame contating the data in the model}
  \item{K}{number of latent classes}
  \item{gamma}{individual frailty. 0 represents the frailty equals 1 and k reprsents the frailty follows gamma(k,k)}
  \item{max_epoches}{maximum iteration epoches for estimation procedure}
  \item{conv_threshold}{converge threshold for estimation procedure}
  \item{boot}{bootstrap sample size}
}
\value{
  SLCARE returns a list containg the following components:
  \item{alpha}{point estimates for alpha}
  \item{beta}{point estimates for beta}
  \item{convergeloss}{converge loss in estimation procedure}
  \item{PosteriorPrediction}{posterior prediction for observed events for subjects of interest}
  \item{EstimatedTau}{posterior probability of latent class membership}
  \item{ModelChecking}{plot for model checking}
  \item{Estimated_mu0t}{plot for estimated mu0(t)}
  \item{est_mu0()}{a function allows to calculate mu0(t) for specific time points}
  \item{Estimated_Mean_Function}{Plot of estimated mean functions}
  \item{RelativeEntropy}{relative entropy}
  \item{InitialAlpha}{initial alpha for estimation procedure}
  \item{InitialBeta}{initial beta for estimation procedure}

  If argument 'boot' is non-NULL, then SLCARE returns two additional components:
  \item{alpha_bootse}{Bootstrap standard error for alpha}
  \item{beta_bootse}{Bootstrap standard error for beta}
}
\examples{
# model <- SLCARE(dat = dat, K = 3)
}
