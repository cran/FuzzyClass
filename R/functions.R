#---- Sturges -----#

#' @noRd
Sturges <- function(dados, M) {

  Output <- lapply(1:length(unique(M)), function(i) {
    SubSet <- dados[M == unique(M)[i], ]
    return(round(sqrt(nrow(SubSet))))
  })

  return(Output)
}


#---- Comprimento do Intervalo ----#

#' @noRd
Comprim_Intervalo <- function(dados, M, Sturges) {

  Output <- lapply(1:length(unique(M)), function(i) {
    SubSet <- dados[M == unique(M)[i], ]
    comp <- (apply(SubSet, 2, max) - apply(SubSet, 2, min)) / Sturges[[i]]
  })

  return(Output)
}
#--------------------------------------------------------

#' @noRd
minimos <- function(dados, M, cols) {

  Output <- lapply(1:length(unique(M)), function(i) {
    sapply(1:cols, function(j) {
      SubSet <- dados[M == unique(M)[i], ]
      return(min(SubSet[, j]))
    })
  })

  return(Output)
}
#---------------



#' @noRd
Freq <- function(dados, M, Comprim_Intervalo, Sturges, minimos, cols) {

  #--------------------------------------------------------
  Freq <- lapply(1:length(unique(M)), function(i) {
    ara <- array(0, dim = c(Sturges[[i]], cols))
    return(ara)
  })
  for (classe in 1:length(unique(M))) {
    # --
    SubSet <- dados[M == unique(M)[classe], ]
    # --
    for (coluna in 1:cols) { # coluna da classe
      for (linhaClasse in 1:nrow(SubSet)) { # linha da classe
        faixa <- minimos[[classe]][coluna] + Comprim_Intervalo[[classe]][coluna] # faixa de frequencia inicial
        for (linhaFreq in 1:Sturges[[classe]]) { # linha da Freq
          # --
          if (SubSet[linhaClasse, coluna] < faixa) { # ve se valor da classe pertence aaquela faixa
            Freq[[classe]][linhaFreq, coluna] <- Freq[[classe]][linhaFreq, coluna] + 1 # acumula valor na faixa de frequencia e interrompe este ultimo for
            break
          }
          if (linhaFreq == Sturges[[classe]] && SubSet[linhaClasse, coluna] >= faixa) {
            Freq[[classe]][linhaFreq, coluna] <- Freq[[classe]][linhaFreq, coluna] + 1
            break
          }
          faixa <- faixa + Comprim_Intervalo[[classe]][coluna] # troca de faixa -> proxima
        }
      }
    }
  }
  #----------
  return(Freq)
  #----------
}



#' @noRd
Intervalos_Valores <- function(dados, M, Comprim_Intervalo, Sturges, minimos, cols) {

  #--------------------------------------------------------
  Freq <- lapply(1:length(unique(M)), function(i) {
    ara <- array(0, dim = c(Sturges[[i]], cols))
    return(ara)
  })
  for (classe in 1:length(unique(M))) {
    # --
    SubSet <- dados[M == unique(M)[classe], ]
    # --
    for (coluna in 1:cols) { # coluna da classe
      for (linhaClasse in 1:nrow(SubSet)) { # linha da classe
        faixaant <- minimos[[classe]][coluna]
        faixa <- minimos[[classe]][coluna] + Comprim_Intervalo[[classe]][coluna] # faixa de frequencia inicial
        for (linhaFreq in 1:Sturges[[classe]]) { # linha da Freq
         # --
           Freq[[classe]][linhaFreq, coluna] <- paste0(round(faixaant,3),",",round(faixa,3));
           # --
          faixaant <- faixa
          faixa <- faixa + Comprim_Intervalo[[classe]][coluna] # troca de faixa -> proxima
        }
      }
    }
  }
  #----------
  return(Freq)
  #----------
}


#--------------------------------------------------------
#---- Pertinencia ----#

#' @noRd
Pertinencia <- function(Freq, dados, M) {

  Output <- lapply(1:length(unique(M)), function(i) {
    Freq[[i]] / nrow(dados[M == unique(M)[i], ])
  })

  return(Output)
}


#' @noRd
pertinencia_predict <- function(M, Sturges, minimos, Comprim_Intervalo, Pertinencia, cols, x){

  #---------------
  ACHOU_t <- c()
  ACHOU <- 0
  #---------------
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
  #-----
  row.names(ACHOU_t) <- unique(M)
  #--------------------------------------------------------
  ACHOU_t <- apply(ACHOU_t, 1, prod)

  return(ACHOU_t)

}

#' @noRd
log_ver_Gamma <- function(theta,y){

  # Gamma Likelihood Function

  beta = theta[2]
  alpha = theta[1]

  saida <- - sum(log(dgamma(y,shape = alpha, rate = beta)),na.rm=T)

  return(saida)
}
