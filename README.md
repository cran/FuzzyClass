
# FuzzyClass <img src="man/figures/logo.png" style="float: right" height="139"/>

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
[![CRAN
Download](https://cranlogs.r-pkg.org/badges/grand-total/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
<!-- badges: end -->

Last update: 24-08-2022

## A family of probabilities-based classifiers Fuzzy and Non-Fuzzy

### Installation

``` r
# Installation
install.packages("devtools")
devtools::install_github("Jodavid/FuzzyClass")
```

### Usage

``` r
# package import
library(FuzzyClass)
```

### Data reading and preparation for use

``` r

library(FuzzyClass)
library(caret)
#> Carregando pacotes exigidos: ggplot2
#> Carregando pacotes exigidos: lattice
#> Warning in system("timedatectl", intern = TRUE): execução do comando
#> 'timedatectl' teve status 1

#' ---------------------------------------------
#' The following shows how the functions are used:
#' --------------
#' Reading a database:
#'
#' Actual training data:
data(VirtualRealityData)

VirtualRealityData <- as.data.frame(VirtualRealityData)

# Splitting into Training and Testing
split <- caTools::sample.split(t(VirtualRealityData[,1]), SplitRatio = 0.7)
Train <- subset(VirtualRealityData, split == "TRUE")
Test <- subset(VirtualRealityData, split == "FALSE")
# ----------------

test = Test[,-4]
```

#### Fuzzy Gaussian Naive Bayes with Fuzzy Parameters

``` r
# --------------------------------------------------
# Fuzzy Gaussian Naive Bayes with Fuzzy Parameters


fit_FGNB <- GauNBFuzzyParam(train =  Train[,-4],
                                    cl = Train[,4], metd = 1, cores = 1)


print(fit_FGNB)
#> 
#> Fuzzy Gaussian Naive Bayes Classifier for Discrete Predictors
#> 
#> Variables:
#> [1] "V1" "V2" "V3"
#> Class:
#> [1] "1" "2" "3"
saida <- predict(fit_FGNB, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 53  4  0
#>          2  7 48 10
#>          3  1 12 45
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8111          
#>                  95% CI : (0.7462, 0.8655)
#>     No Information Rate : 0.3556          
#>     P-Value [Acc > NIR] : <2e-16          
#>                                           
#>                   Kappa : 0.7163          
#>                                           
#>  Mcnemar's Test P-Value : 0.5724          
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.8689   0.7500   0.8182
#> Specificity            0.9664   0.8534   0.8960
#> Pos Pred Value         0.9298   0.7385   0.7759
#> Neg Pred Value         0.9350   0.8609   0.9180
#> Prevalence             0.3389   0.3556   0.3056
#> Detection Rate         0.2944   0.2667   0.2500
#> Detection Prevalence   0.3167   0.3611   0.3222
#> Balanced Accuracy      0.9176   0.8017   0.8571

saida <- predict(fit_FGNB, test, type = "matrix")
```

<!--

#### Fuzzy Gaussian Naive Bayes based in Zadeh


```r
fit_FGNB <- FuzzyGaussianNaiveBayes(train =  Train[,-4],
                                    cl = Train[,4], cores = 1,
                                    fuzzy = T)
print(fit_FGNB)
#> 
#> Fuzzy Gaussian Naive Bayes Classifier for Discrete Predictors Zadeh-based
#> 
#> Variables:
#> [1] "V1" "V2" "V3"
#> Class:
#> [1] "1" "2" "3"
saida <- predict(fit_FGNB, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 55  1  1
#>          2  4 57  4
#>          3  0 12 46
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8778          
#>                  95% CI : (0.8208, 0.9218)
#>     No Information Rate : 0.3889          
#>     P-Value [Acc > NIR] : < 2e-16         
#>                                           
#>                   Kappa : 0.8161          
#>                                           
#>  Mcnemar's Test P-Value : 0.07855         
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9322   0.8143   0.9020
#> Specificity            0.9835   0.9273   0.9070
#> Pos Pred Value         0.9649   0.8769   0.7931
#> Neg Pred Value         0.9675   0.8870   0.9590
#> Prevalence             0.3278   0.3889   0.2833
#> Detection Rate         0.3056   0.3167   0.2556
#> Detection Prevalence   0.3167   0.3611   0.3222
#> Balanced Accuracy      0.9578   0.8708   0.9045

# -----
fit_GNB <- FuzzyGaussianNaiveBayes(train =  Train[,-4],
                               cl = Train[,4], cores = 2,
                               fuzzy = F)
print(fit_GNB)
#> 
#> Gaussian Naive Bayes Classifier for Discrete Predictors
#> 
#> Variables:
#> [1] "V1" "V2" "V3"
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_GNB, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 56  1  0
#>          2  4 57  4
#>          3  0 11 47
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8889          
#>                  95% CI : (0.8336, 0.9308)
#>     No Information Rate : 0.3833          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.8328          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9333   0.8261   0.9216
#> Specificity            0.9917   0.9279   0.9147
#> Pos Pred Value         0.9825   0.8769   0.8103
#> Neg Pred Value         0.9675   0.8957   0.9672
#> Prevalence             0.3333   0.3833   0.2833
#> Detection Rate         0.3111   0.3167   0.2611
#> Detection Prevalence   0.3167   0.3611   0.3222
#> Balanced Accuracy      0.9625   0.8770   0.9181

saida <- predict(fit_GNB, test, type = "matrix")
```

#### Fuzzy Naive Bayes Triangular


```r
fit_FNBT <- FuzzyTriangularNaiveBayes(train =  Train[,-4],
                                  cl = Train[,4], cores = 2,
                                  fuzzy = T)

print(fit_FNBT)
#> 
#> Fuzzy Naive Bayes Triangular Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_FNBT, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 53  4  0
#>          2  3 62  0
#>          3  0 17 41
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8667          
#>                  95% CI : (0.8081, 0.9127)
#>     No Information Rate : 0.4611          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7985          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9464   0.7470   1.0000
#> Specificity            0.9677   0.9691   0.8777
#> Pos Pred Value         0.9298   0.9538   0.7069
#> Neg Pred Value         0.9756   0.8174   1.0000
#> Prevalence             0.3111   0.4611   0.2278
#> Detection Rate         0.2944   0.3444   0.2278
#> Detection Prevalence   0.3167   0.3611   0.3222
#> Balanced Accuracy      0.9571   0.8580   0.9388

saida <- predict(fit_FNBT, test, type = "matrix")

# ----------------

fit_NBT <- FuzzyTriangularNaiveBayes(train =  Train[,-4],
                                 cl = Train[,4], cores = 2,
                                 fuzzy = F)
print(fit_NBT)
#> 
#> Naive Bayes Triangular Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_NBT, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 55  2  0
#>          2  5 58  2
#>          3  0 12 46
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8833          
#>                  95% CI : (0.8272, 0.9263)
#>     No Information Rate : 0.4             
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.8243          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9167   0.8056   0.9583
#> Specificity            0.9833   0.9352   0.9091
#> Pos Pred Value         0.9649   0.8923   0.7931
#> Neg Pred Value         0.9593   0.8783   0.9836
#> Prevalence             0.3333   0.4000   0.2667
#> Detection Rate         0.3056   0.3222   0.2556
#> Detection Prevalence   0.3167   0.3611   0.3222
#> Balanced Accuracy      0.9500   0.8704   0.9337

saida <- predict(fit_NBT, test, type = "matrix")
```

#### Fuzzy Exponential Naive Bayes Classifier


```r

fit_FENB <- ExpNBFuzzyParam(train =  Train[,-4],
                                    cl = Train[,4], metd = 1, cores = 2)
                              
print(fit_FENB)
#> 
#> Fuzzy Exponential Naive Bayes Classifier for Discrete Predictors
#> 
#> Variables:
#> [1] "V1" "V2" "V3"
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_FENB, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 54  3  0
#>          2  3 53  9
#>          3  0 10 48
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8611          
#>                  95% CI : (0.8018, 0.9081)
#>     No Information Rate : 0.3667          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7912          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9474   0.8030   0.8421
#> Specificity            0.9756   0.8947   0.9187
#> Pos Pred Value         0.9474   0.8154   0.8276
#> Neg Pred Value         0.9756   0.8870   0.9262
#> Prevalence             0.3167   0.3667   0.3167
#> Detection Rate         0.3000   0.2944   0.2667
#> Detection Prevalence   0.3167   0.3611   0.3222
#> Balanced Accuracy      0.9615   0.8489   0.8804

saida <- predict(fit_FENB, test, type = "matrix")
head(saida)
#>              1         2         3
#> [1,] 0.3207259 0.3299960 0.3492781
#> [2,] 0.3157642 0.3350339 0.3492019
#> [3,] 0.2942864 0.3371848 0.3685288
#> [4,] 0.3155302 0.3300264 0.3544433
#> [5,] 0.2971174 0.3357104 0.3671723
#> [6,] 0.2871457 0.3392441 0.3736102
```

#### Fuzzy Gamma Naive Bayes Classifier


```r

fit_NBT <- FuzzyGammaNaiveBayes(train =  Train[,-4],
                                    cl = Train[,4], cores = 2)
                              
print(fit_NBT)
#> 
#> Fuzzy Gamma Naive Bayes Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_NBT, test)
saida <- factor(saida,levels = unique(Test[,4]))
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 57  0  0
#>          2 39 26  0
#>          3  1 57  0
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.4611          
#>                  95% CI : (0.3867, 0.5368)
#>     No Information Rate : 0.5389          
#>     P-Value [Acc > NIR] : 0.9848          
#>                                           
#>                   Kappa : 0.187           
#>                                           
#>  Mcnemar's Test P-Value : <2e-16          
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.5876   0.3133       NA
#> Specificity            1.0000   0.5979   0.6778
#> Pos Pred Value         1.0000   0.4000       NA
#> Neg Pred Value         0.6748   0.5043       NA
#> Prevalence             0.5389   0.4611   0.0000
#> Detection Rate         0.3167   0.1444   0.0000
#> Detection Prevalence   0.3167   0.3611   0.3222
#> Balanced Accuracy      0.7938   0.4556       NA

saida <- predict(fit_NBT, test, type = "matrix")
head(saida)
#>              1            2            3
#> [1,] 0.9995993 4.006780e-04 6.390272e-15
#> [2,] 0.9998230 1.769991e-04 7.199872e-16
#> [3,] 0.9999992 7.907169e-07 4.046193e-19
#> [4,] 0.9998423 1.577194e-04 1.832986e-15
#> [5,] 0.9999999 1.054485e-07 9.065652e-19
#> [6,] 1.0000000 2.649369e-08 1.143123e-20
```

#### Fuzzy Exponential Naive Bayes Classifier


```r

fit_NBE <- FuzzyExponentialNaiveBayes(train =  Train[,-4],
                                    cl = Train[,4], cores = 2)
                              
print(fit_NBE)
#> 
#> Fuzzy Exponential Naive Bayes Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_NBE, test)
saida <- factor(saida,levels = unique(Test[,4]))
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 13 44  0
#>          2  0 54 11
#>          3  0 53  5
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.4             
#>                  95% CI : (0.3278, 0.4755)
#>     No Information Rate : 0.8389          
#>     P-Value [Acc > NIR] : 1               
#>                                           
#>                   Kappa : 0.0706          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity           1.00000   0.3576  0.31250
#> Specificity           0.73653   0.6207  0.67683
#> Pos Pred Value        0.22807   0.8308  0.08621
#> Neg Pred Value        1.00000   0.1565  0.90984
#> Prevalence            0.07222   0.8389  0.08889
#> Detection Rate        0.07222   0.3000  0.02778
#> Detection Prevalence  0.31667   0.3611  0.32222
#> Balanced Accuracy     0.86826   0.4892  0.49466

saida <- predict(fit_NBT, test, type = "matrix")
head(saida)
#>              1            2            3
#> [1,] 0.9995993 4.006780e-04 6.390272e-15
#> [2,] 0.9998230 1.769991e-04 7.199872e-16
#> [3,] 0.9999992 7.907169e-07 4.046193e-19
#> [4,] 0.9998423 1.577194e-04 1.832986e-15
#> [5,] 0.9999999 1.054485e-07 9.065652e-19
#> [6,] 1.0000000 2.649369e-08 1.143123e-20
```
-->
