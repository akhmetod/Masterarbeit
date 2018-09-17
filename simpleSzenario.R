##### This is a first simple model
##### For A Variable containing 4 categories


randCat <- function(nObs, nCovar, nCat, coefM) {
  # create matrix of covariates
  designM <- matrix(c(rep(1, nObs), rnorm(nObs * (nCovar - 1))), 
                    nrow = nObs, ncol = nCovar)
  
  # create pseudo probs (not normalized)
  probM <- exp(designM %*% coefM)
  
  # obtain categories (as dummies), normalization is done internally in rmultinorm
  catM = t(apply(probM, 1, rmultinom, n = 1, size = 1))
  
  # dataframe
  modelData <- data.frame(apply(catM, 1, function(x) {which(as.logical(x))} ),
                          designM)
  colnames(modelData) <- c("y", "intecept", paste0("X_", seq_len(nCovar - 1)))
  
  # simple random Sampling
  smpFilter <- sample(x = seq_len(nObs), size = floor(nObs / 10))
  
  return(list(smp = modelData[smpFilter,], pop = modelData))
}

set.seed(100)

nObs <- 10000
nCovar <- 3
nCat <- 4
simRuns <- 200


# create vector of coefficients
coefM <- matrix(rnorm(nCovar * nCat), nrow = nCovar, ncol = nCat)

popList <- replicate(n = simRuns, expr = randCat(nObs = nObs, 
                                                 nCovar = nCovar, 
                                                 nCat = nCat, 
                                                 coefM = coefM), 
                     simplify = FALSE)

