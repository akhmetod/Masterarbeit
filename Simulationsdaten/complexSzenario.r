##### This is a first simple model
##### For A Variable containing 4 categories


randCat <- function(nObs, nCovar, nCat, coefM, form, nX) {
  
  dataM <- matrix(c(rnorm(nObs * (nX - 3)),
                    as.numeric(runif(nObs) < 0.3), 
                    as.numeric(runif(nObs) < 0.6)
                    ), 
                  nrow = nObs, ncol = nX - 1)
  colnames(dataM) <- paste0("x_", seq_len(nX - 1))
    
  # create matrix of covariates
  designM <- model.matrix(form, 
                          data = data.frame(y = rnorm(nObs), dataM)) 
  
  # create pseudo probs (not normalized)
  probM <- exp(designM %*% coefM)
  
  # obtain categories (as dummies), normalization is done internally in rmultinorm
  catM = t(apply(probM, 1, rmultinom, n = 1, size = 1))
  
  # dataframe
  modelData <- data.frame(apply(catM, 1, function(x) {which(as.logical(x))} ),
                          intercept = 1,
                          dataM)
  colnames(modelData) <- c("y", "intecept", paste0("X_", seq_len(nX - 1)))
  
  # simple random Sampling
  smpFilter <- sample(x = seq_len(nObs), size = floor(nObs / 10))
  
  return(list(smp = modelData[smpFilter,], pop = modelData))
}

set.seed(100)

nObs <- 10000
nCovar <- 11
nX <- 7
nCat <- 4
simRuns <- 200
form <- y ~ x_1 + poly(x_2, 3) + x_3 + x_5 + x_6 + x_3 * x_1 + x_2 : x_6 + x_5 * x_6

# create vector of coefficients
coefM <- matrix(rnorm(nCovar * nCat), nrow = nCovar, ncol = nCat)

popList <- replicate(n = simRuns, expr = randCat(nObs = nObs, 
                                                 nCovar = nCovar, 
                                                 nCat = nCat, 
                                                 coefM = coefM,
                                                 form = form,
                                                 nX = nX), 
                     simplify = FALSE)

save(popList, file = "data/complexSimData.Rdata")
