#' \code{FuzzyGammaNaiveBayes} Fuzzy Gamma Naive Bayes
#'
#'
#' @param train matrix or data frame of training set cases.
#' @param cl factor of true classifications of training set
#' @param cores  how many cores of the computer do you want to use (default = 2)
#' @param fuzzy boolean variable to use the membership function
#'
#' @return A vector of classifications
#'
#' @references
#' \insertRef{de2018fuzzy}{FuzzyClass}
#'
#' @examples
#'
#' set.seed(1) # determining a seed
#' data(iris)
#'
#' # Splitting into Training and Testing
#' split <- caTools::sample.split(t(iris[, 1]), SplitRatio = 0.7)
#' Train <- subset(iris, split == "TRUE")
#' Test <- subset(iris, split == "FALSE")
#' # ----------------
#' # matrix or data frame of test set cases.
#' # A vector will be interpreted as a row vector for a single case.
#' test <- Test[, -5]
#' fit_NBT <- FuzzyGammaNaiveBayes(
#'   train = Train[, -5],
#'   cl = Train[, 5], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 5])
#' @importFrom stats dgamma
#'
#' @export
FuzzyGammaNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE) {
  UseMethod("FuzzyGammaNaiveBayes")
}

#' @export
FuzzyGammaNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T) {

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

  # --------------------------------------------------------
  # Estimating Gamma Parameters
  parametersC <- lapply(1:length(unique(M)), function(i) {
    lapply(1:cols, function(j) {
      # print(c(i,j))
      SubSet <- dados[M == unique(M)[i], j]
      param <- MASS::fitdistr(SubSet, "gamma", lower = 0.001, upper = max(SubSet) + 1e-2)$estimate
      return(param)
    })
  })
  # --------------------------------------------------------
  Sturges <- Sturges(dados, M);
  Comprim_Intervalo <- Comprim_Intervalo(dados, M, Sturges);
  minimos <- minimos(dados, M, cols);
  Freq <- Freq(dados, M, Comprim_Intervalo, Sturges, minimos, cols);
  Pertinencia <- Pertinencia(Freq, dados, M);
  # ------
  # Probabilidade a priori das classes - consideradas iguais
  pk <- rep(1 / length(unique(M)), length(unique(M)))
  # -------------------------------------------------------


  # -------------------------------------------------------
  structure(list(
    parametersC = parametersC,
    minimos = minimos,
    cols = cols,
    M = M,
    cores = cores,
    Comprim_Intervalo = Comprim_Intervalo,
    Pertinencia = Pertinencia,
    Sturges = Sturges,
    pk = pk,
    fuzzy = fuzzy
  ),
  class = "FuzzyGammaNaiveBayes"
  )
}
# -------------------------


#' @export
print.FuzzyGammaNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Gamma Naive Bayes Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nNaive Gamma  Bayes Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyGammaNaiveBayes <- function(object,
                                         newdata,
                                         type = "class",
                                         ...) {
  # --------------------------------------------------------
  # type <- match.arg("class")
  test <- as.data.frame(newdata)
  # --------------------------------------------------------
  parametersC <- object$parametersC
  minimos <- object$minimos
  cols <- object$cols
  M <- object$M
  cores <- object$cores
  Comprim_Intervalo <- object$Comprim_Intervalo
  Pertinencia <- object$Pertinencia
  Sturges <- object$Sturges
  pk <- object$pk
  fuzzy <- object$fuzzy
  # --------------------------------------------------------

  # --------------------------------------------------------
  # Classification
  # --------------
  P <- lapply(1:length(unique(M)), function(i) {
    densidades <- sapply(1:cols, function(j) {
      stats::dgamma(test[, j], shape = parametersC[[i]][[j]][1], scale = parametersC[[i]][[j]][2])
    })
    densidades <- apply(densidades, 1, prod)
    # Calcula a P(w_i) * P(X_k | w_i)
    p <- pk[[i]] * densidades
    # ---
    return(p)
  })

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

    if (fuzzy == T) {

      ACHOU_t <- pertinencia_predict(M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols,  x);

      f <- sapply(1:length(unique(M)), function(i) {
        P[[i]][h] * ACHOU_t[i]
      })

    } else {

      f <- sapply(1:length(unique(M)), function(i) {
        P[[i]][h]
      })

    }
    # -------------------------------------------------------

    return(f)
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
    R_M_obs <- matrix(unlist(R_M_obs),ncol = cols)
    R_M_obs <- R_M_obs/rowSums(R_M_obs,na.rm = T)
    # ----------
    colnames(R_M_obs) <- unique(M)
    return(R_M_obs)
    # -------------------------
  }
}
