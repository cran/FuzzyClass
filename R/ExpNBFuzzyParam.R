#' Fuzzy Exponential Naive Bayes Classifier with Fuzzy parameters
#'
#' \code{ExpNBFuzzyParam} Fuzzy Exponential Naive Bayes Classifier with Fuzzy parameters
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
#' \insertRef{rodrigues2016new}{FuzzyClass}
#'
#'
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' data(VirtualRealityData)
#'
#' # Splitting into Training and Testing
#' split <- caTools::sample.split(t(VirtualRealityData[, 1]), SplitRatio = 0.7)
#' Train <- subset(VirtualRealityData, split == "TRUE")
#' Test <- subset(VirtualRealityData, split == "FALSE")
#'
#' # ----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -4]
#' fit_FENB <- ExpNBFuzzyParam(
#'   train = Train[, -4],
#'   cl = Train[, 4], metd = 1, cores = 2
#' )
#'
#' pred_FENB <- predict(fit_FENB, test)
#'
#' head(pred_FENB)
#' head(Test[, 4])
#' @importFrom stats dexp
#' @importFrom foreach '%dopar%'
#' @importFrom Rdpack reprompt
#'
#' @export
ExpNBFuzzyParam <- function(train, cl, metd = 1, cores = 2) {
  UseMethod("ExpNBFuzzyParam")
}

#' @export
ExpNBFuzzyParam.default <- function(train, cl, metd = 1, cores = 2) {

  # --------------------------------------------------------
  # Estimating class parameters
  train <- as.data.frame(train)
  cols <- ncol(train) # Number of variables
  if(is.null(cols)){
    cols <- 1
  }
  dados <- train # training data matrix
  M <- c(unlist(cl)) # true classes
  M <- factor(M, labels = unique(M))
  # --------------------------------------------------------
  # Finding lambdas for each class
  lambdas <- lapply(1:length(unique(M)), function(i) colMeans(subset(dados, M == i)))
  # --------------------------------------------------------
  # --------------------------------------------------------
  # Estimating Triangular Parameters
  alpha <- seq(0.0001, 1.1, 0.1)
  # -------------------------------
  N <- nrow(dados) # Number of observations
  # -------------------------------
  #  Lambda Parameters
  # ------------------
  Parameters_lambda <- lapply(1:length(lambdas), function(i) { # Pra grupos
    lapply(1:length(lambdas[[1]]), function(k) { # Para dimensao
      round(
        t(sapply(1:length(alpha), function(j) {
          c(
            (sqrt(N) * (lambdas[[i]][k])) / (sqrt(N) + qnorm(1 - alpha[j] / 2)),
            (sqrt(N) * (lambdas[[i]][k])) / (sqrt(N) - qnorm(1 - alpha[j] / 2))
          )
        })),
        3
      )
    })
  })
  # -------------------------------------------------------
  structure(list(
    Parameters_lambda = Parameters_lambda,
    lambdas = lambdas,
    cols = cols,
    M = M,
    alpha = alpha,
    metd = metd,
    cores = cores
  ),
  class = "ExpNBFuzzyParam"
  )
}
# -------------------------

#' @export
print.ExpNBFuzzyParam <- function(x, ...) {
  # -----------------
  cat("\nFuzzy Exponential Naive Bayes Classifier for Discrete Predictors\n\n")
  # -----------------
  cat("Variables:\n")
  print(names(x$lambdas[[1]]))
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}


#' @export
predict.ExpNBFuzzyParam <- function(object,
                                    newdata,
                                    type = "class",
                                    ...) {
  # --------------------------------------------------------
  # type <- match.arg("class")
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  Parameters_lambda <- object$Parameters_lambda
  lambdas <- object$lambdas
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
      lapply(1:length(lambdas), function(i) { # loop to groups
        trian <- lapply(1:length(lambdas[[1]]), function(k) { # loop to dimensions
          t(sapply(1:length(alpha), function(j) {
            a <- log(dexp(x = as.numeric(x[k]), rate = 1 / as.numeric(Parameters_lambda[[i]][[k]][j, 1])))
            b <- log(dexp(x = as.numeric(x[k]), rate = 1 / as.numeric(Parameters_lambda[[i]][[k]][j, 2])))

            a <- ifelse(a == -Inf, 1e-06, a)
            b <- ifelse(b == -Inf, 1e-06, b)

            c(min(a, b), max(a, b))
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
    Infpos <- which(R_M_obs==Inf)
    R_M_obs[Infpos] <- .Machine$integer.max;
    R_M_obs <- matrix(unlist(R_M_obs),ncol = length(unique(M)))
    R_M_obs <- R_M_obs/rowSums(R_M_obs,na.rm = T)
    # ----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    # -------------------------
  }
}
