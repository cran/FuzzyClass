#' Fuzzy Poisson Naive Bayes Classifier with Fuzzy parameters
#'
#' \code{PoiNBFuzzyParam} Fuzzy Poisson Naive Bayes Classifier with Fuzzy parameters
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param metd Method of transforming the triangle into scalar, It is the type of data entry for the test sample, use metd 1 if you want to use the Baricentro technique and use metd 2 if you want to use the Q technique of the uniformity test (article: Directional Statistics and Shape analysis).
#' @param cores  how many cores of the computer do you want to use (default = 2)
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{soares2016assessment}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' class1 <- data.frame(vari1 = rpois(100,lambda = 2),
#'                      vari2 = rpois(100,lambda = 2),
#'                      vari3 = rpois(100,lambda = 2), class = 1)
#' class2 <- data.frame(vari1 = rpois(100,lambda = 1),
#'                      vari2 = rpois(100,lambda = 1),
#'                      vari3 = rpois(100,lambda = 1), class = 2)
#' class3 <- data.frame(vari1 = rpois(100,lambda = 5),
#'                      vari2 = rpois(100,lambda = 5),
#'                      vari3 = rpois(100,lambda = 5), class = 3)
#' data <- rbind(class1,class2,class3)
#'
#' # Splitting into Training and Testing
#' split <- caTools::sample.split(t(data[, 1]), SplitRatio = 0.7)
#' Train <- subset(data, split == "TRUE")
#' Test <- subset(data, split == "FALSE")
#' # ----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -4]
#' fit_FPoiNB <- PoiNBFuzzyParam(
#'   train = Train[, -4],
#'   cl = Train[, 4], metd = 1, cores = 2
#' )
#'
#' pred_FPoiNB <- predict(fit_FPoiNB, test)
#'
#' head(pred_FPoiNB)
#' head(Test[, 4])
#' @importFrom stats cov dpois qnorm
#' @importFrom foreach '%dopar%'
#' @importFrom Rdpack reprompt
#' @importFrom MASS fitdistr
#'
#' @export
PoiNBFuzzyParam <- function(train, cl, metd = 1, cores = 2) {
  UseMethod("PoiNBFuzzyParam")
}

#' @export
PoiNBFuzzyParam.default <- function(train, cl, metd = 1, cores = 2) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if (is.null(cols)) {
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  M <- factor(M, labels = unique(M))
  # --------------------------------------------------------
  # Verify data types
  verifyNumbers <- sapply(1:cols, function(i){
    n = 3
    subset <- sample(dados[,i],size = n, replace = F)
    result <- subset == floor(subset)
    if(sum(result) == n){
      result <- 1
    }else{
        result <- 0
    }
    return(result)
  })

  # --------------------------------------------------------
  if(sum(verifyNumbers) != cols){ stop("All variables must be discrete values.") }
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Finding lambda estimated
  lambda_estimated <- lapply(1:length(unique(M)), function(i) {
    Sub <- subset(dados, M == unique(M)[i])
    sapply(1:cols, function(j) {
      saida <- MASS::fitdistr(Sub[, j], "poisson")[1:2]#mean(Sub[, j])
      return(saida)
    })
  })
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Triangular Parameters
  alpha <- seq(0.0001, 1.1, 0.1)
  # -------------------------------
  N <- nrow(dados) # Number of observations
  # -------------------------------
  #  Alphas Parameters
  # ------------------
  Parameters_lambdas <- lapply(1:length(lambda_estimated), function(i) { # loop to groups
    lapply(1:ncol(lambda_estimated[[1]]), function(k) { # loop to dimensions
      round(
        t(sapply(1:length(alpha), function(j) {
          c(
            lambda_estimated[[i]][,k]$estimate - alpha[j]*lambda_estimated[[i]][,k]$sd,
            lambda_estimated[[i]][,k]$estimate + alpha[j]*lambda_estimated[[i]][,k]$sd
          )
        })),
        3
      )
    })
  })
  # -------------------------------------------------------
  structure(list(
    Parameters_lambdas = Parameters_lambdas,
    lambda_estimated = lambda_estimated,
    cols = cols,
    M = M,
    alpha = alpha,
    metd = metd,
    cores = cores
  ),
  class = "PoiNBFuzzyParam"
  )
}
# -------------------------


#' @export
print.PoiNBFuzzyParam <- function(x, ...) {
  # -----------------
  cat("\nFuzzy Poisson Naive Bayes Classifier for Discrete Predictors\n\n")
  # -----------------
  #cat("Variables:\n")
  #print(names(x$lambda_estimated[[1]]))
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.PoiNBFuzzyParam <- function(object,
                                    newdata,
                                    type = "class",
                                    ...) {
  # --------------------------------------------------------
  # type <- match.arg("class")
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  Parameters_lambdas <- object$Parameters_lambdas
  lambda_estimated <- object$lambda_estimated
  cols <- object$cols
  M <- object$M
  alpha <- object$alpha
  metd <- object$metd
  cores <- object$cores
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Calculation of triangles for each test observation
  # sum of Logs and calculation of Barycenter
  # --------------
  N_test <- nrow(test)
  # --------------
  # Defining how many CPU cores to use
  core <- parallel::makePSOCKcluster(cores)
  doParallel::registerDoParallel(core)
  # --------------
  # loop start
  R_M_obs <- foreach::foreach(h = 1:N_test, .combine = rbind) %dopar% {
    # ------------
    x <- test[h, ]
    # ------------
    triangulos_obs <-
      lapply(1:length(unique(M)), function(i) { # loop to groups
        trian <- lapply(1:cols, function(k) { # loop to dimensions
          t(sapply(1:length(alpha), function(j) {
            # ------------
            a <- dpois(x = as.numeric(x[k]), lambda =  as.numeric(Parameters_lambdas[[i]][[k]][j, 1]))
            b <- dpois(x = as.numeric(x[k]), lambda =  as.numeric(Parameters_lambdas[[i]][[k]][j, 2]))
            # ------------
            c(min(a, b), max(a, b))
            # ------------
          }))
        })
        if (length(trian) > 1) {
          return(Reduce("+", trian))
        } else {
          return(trian)
        }
      })
    # ------------
    # Center of Mass Calculation
    vec_trian <- lapply(1:length(unique(M)), function(i) c(triangulos_obs[[i]][1, 1], triangulos_obs[[i]][11, 1], triangulos_obs[[i]][1, 2]))
    # --------------------------------------------------------
    # Transforming Vector to Scalar
    # ------------
    R_M <- switch(metd,
      # ------------
      # Barycenter
      "1" = {
        # ------------
        sapply(1:length(unique(M)), function(i) vec_trian[[i]][2] * (((vec_trian[[i]][2] - vec_trian[[i]][1]) * (vec_trian[[i]][3] - vec_trian[[i]][2]) + 1) / 3))
        # ------------
      },
      "2" = {
        # ------------
        # Using distance Q
        sapply(1:length(unique(M)), function(i) {
          # ------------
          # Start 3 values
          y <- vec_trian[[i]]
          # ------------
          # getting the product zz*
          S <- y %*% t(Conj(y)) # matrix k x k
          # ------------
          # getting the eigenvalues
          l <- eigen(S)$values
          # Calculating Q
          Q <- 3 * (l[1] - l[2])^2
          # ------------
          return(Q)
        })
        # ------------
      }
    )
    # --------------------------------------------------------
    # R_M_class <- which.max(produto)
    R_M_class <- R_M
    # --------------------------------------------------------
    return(R_M_class)
  }
  # ------------
  # -------------------------
  parallel::stopCluster(core)
  # ---------
  if (type == "class") {
    # -------------------------
    R_M_obs <- sapply(1:nrow(R_M_obs), function(i) which.max(R_M_obs[i, ]))
    resultado <- unique(M)[R_M_obs]
    return(as.factor(c(resultado)))
    # -------------------------
  } else {
    # -------------------------
    Infpos <- which(R_M_obs == Inf)
    R_M_obs[Infpos] <- .Machine$integer.max
    R_M_obs <- matrix(unlist(R_M_obs),ncol = cols)
    R_M_obs <- R_M_obs/rowSums(R_M_obs,na.rm = T)
    # ----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    # -------------------------
  }
}
