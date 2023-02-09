# SLCARE

## Semiparametric Latent Class Analysis for Recurrent Events

**SLCARE** implements a latent class analysis of recurrent event data based on a semeparametric multiplicative modeling.

You can install SLCARE from github with:

```R
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install_github("qyxxx/SLCARE")
```

### Data format and SLCARE function

**SLCARE**  needs a regulated data format as input. You can check R help document for an example by input

```{r}
? SLCARE_simdat
```

You can check the simulated dataset by input

```{r}
data(SLCARE_simdat)
```

You can conduct an analysis with a single function 'SLCARE()'. You can check R helpe document for an instruction by inputting

```{r}
? SLCARE
```

### Exmaples

```{r}
library(SLCARE)
data(SLCARE_simdat)
# Example 1: number of latent classes k = 2, default settings, generate initial values in estimation procedure with Kmeans
model1 <- SLCARE(dat = SLCARE_simdat, K=2)
# contents of output
names(model1)
# point estimates
model1$alpha
model1$beta
# converge loss in estimation procedure
model1$convergeloss
# Posterior prediction
model1$PosteriorPrediction
# Posterior probability of latent class membership
model1$EstimatedTau
# model checking plot
model1$ModelChecking
# Plot of estimated \eqn(\mu_0 (t)) for all observed time
model1$Estimated_mu0t
# Estimated \eqn(\mu_0 (t))
# You may input multiple time points of interest
model1$est_mu0(c(100, 1000, 5000))
# Plot of estimated mean function
model1$Estimated_Mean_Function
# Relative entropy
model1$RelativeEntropy
# Initial values for estimation procedure
model1$InitialAlpha
model1$InitialBeta
# You can select initial value in estimation procedure manually
alpha <- matrix(c(0, 0, 0.5, -2, 2, -4), nrow = 3, ncol = 2, byrow = T)
beta <- matrix(c(2.5, -0.5, -0.3, 1.5, -0.2, -0.5, 2.5,  0.1, 0.2), nrow = 3 , ncol = 2+1 , byrow = T)
model2 <- SLCARE(alpha, beta, dat)
# You can define individual frailty with gamma(p,p). Below is an example with manually defined initial value and frailty gamma(3,3)
model3 <- SLCARE(alpha, beta, dat, gamma = 3)
# You can use bootstrap for bootstrap standard error. Bootstrap sample size = 100 (time consuming procedure)
# model4 <- SLCARE(alpha, beta, dat, boot = 100)
# SLCARE() with "boot" argument will return to two additional contents: "alpha_bootse", "beta_bootse" which represent bootsrap standard errors for alpha and beta
model5$alpha_bootse
model5$beta_bootse
```

