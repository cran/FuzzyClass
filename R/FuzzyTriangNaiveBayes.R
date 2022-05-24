#' \code{FuzzyTriangularNaiveBayes} Naive Bayes Trianglar Classifier
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
#' \insertRef{de2020online}{FuzzyClass}
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
#' fit_NBT <- FuzzyTriangularNaiveBayes(
#'   train = Train[, -5],
#'   cl = Train[, 5], cores = 2
#' )
#'
#' pred_NBT <- predict(fit_NBT, test)
#'
#' head(pred_NBT)
#' head(Test[, 5])
#' @export
FuzzyTriangularNaiveBayes <- function(train, cl, cores = 2, fuzzy = TRUE) {
  UseMethod("FuzzyTriangularNaiveBayes")
}

#' @export
FuzzyTriangularNaiveBayes.default <- function(train, cl, cores = 2, fuzzy = T) {

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
  # intervalos = 10 # Division to memberships
  # --------------------------------------------------------
  # # --------------------------------------------------------
  # # Estimating class memberships
  # pertinicesC <- lapply(1:length(unique(M)), function(i){
  #   lapply(1:cols, function(j){
  #     SubSet <- dados[M==unique(M)[i],j]
  #     getMembershipsTrapezoidal(SubSet, intervalos)
  #   })
  # })
  # # --------------------------------------------------------

  # --------------------------------------------------------
  # Estimating Triangular Parameters
  parametersC <- lapply(1:length(unique(M)), function(i) {
    SubSet <- dados[M == unique(M)[i], ]
    getParametersTriangular(SubSet)
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
  class = "FuzzyTriangularNaiveBayes"
  )
}
# -------------------------


#' @export
print.FuzzyTriangularNaiveBayes <- function(x, ...) {
  if (x$fuzzy == T) {
    # -----------------
    cat("\nFuzzy Naive Bayes Triangular Classifier for Discrete Predictors\n\n")
    # -----------------
  } else {
    # -----------------
    cat("\nNaive Bayes Triangular Classifier for Discrete Predictors\n\n")
    # -----------------
  }
  cat("Class:\n")
  print(levels(x$M))
  # -----------------
}

#' @export
predict.FuzzyTriangularNaiveBayes <- function(object,
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
      EnvStats::dtri(test[, j], min = parametersC[[i]][1, j], max = parametersC[[i]][2, j], mode = parametersC[[i]][3, j] + 1e-8)
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
    ACHOU_t <- c()
    ACHOU <- 0

    if (fuzzy == T) {
      # ---------------
      for (classe in 1:length(unique(M))) {
        # --
        # --
        for (coluna in 1:cols) { # coluna da classe
          for (linhaF in 1:Sturges[[classe]]) { # linha da classe
            faixa <- minimos[[classe]][coluna] + Comprim_Intervalo[[classe]][coluna] # faixa de frequencia inicial
            if (x[coluna] < faixa) { # ve se valor da classe pertence aaquela faixa
              ACHOU[coluna] <- Pertinencia[[classe]][linhaF, coluna] # acumula valor na faixa de frequencia e interrompe este ultimo for
              break
            }
            if (linhaF == Sturges[[classe]]) {
              ACHOU[coluna] <- Pertinencia[[classe]][linhaF, coluna]
              break
            }
            faixa <- faixa + Comprim_Intervalo[[classe]][coluna] # troca de faixa -> proxima
          }
        }
        # ---
        ACHOU_t <- rbind(ACHOU_t, ACHOU) # Classes são as linhas
        # ---
      }
      # -----
      row.names(ACHOU_t) <- unique(M)
      # --------------------------------------------------------
      ACHOU_t <- apply(ACHOU_t, 1, prod)

      f <- sapply(1:length(unique(M)), function(i) {
        P[[i]][h] * ACHOU_t[i]
      })
    } else {
      f <- sapply(1:length(unique(M)), function(i) {
        P[[i]][h] #* ACHOU_t[i]
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

# --------------------------------------------------
getParametersTriangular <- function(sample) {

  # --------------------------------------------------
  # Estimativa dos parametros a partir do método dado em Werner's Blog
  # https://wernerantweiler.ca/blog.php?item=2019-06-05
  # Verificar uma alternativa mais confiavel e melhor citada na literatura
  # --------------------------------------------------
  qc <- sapply(1:ncol(sample), function(i) stats::quantile(sample[, i], probs = c(0.0625, 0.25, 0.75, 0.9375)))
  uc <- (qc[2, ] - qc[1, ])^2
  vc <- (qc[4, ] - qc[3, ])^2
  ac <- 2 * qc[1, ] - qc[2, ]
  bc <- 2 * qc[4, ] - qc[3, ]
  mc <- (uc * bc + vc * ac) / (uc + vc)
  # ----------
  names(ac) <- colnames(sample)
  names(bc) <- colnames(sample)
  names(mc) <- colnames(sample)
  # -------------------------
  # Parameters Return
  return(rbind(ac, bc, mc))
}
