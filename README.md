
# FuzzyClass <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
[![CRAN
Download](https://cranlogs.r-pkg.org/badges/grand-total/FuzzyClass)](https://cran.r-project.org/package=FuzzyClass)
<!-- badges: end -->

Last update: 19-05-2022

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
#> Carregando pacotes exigidos: lattice
#> Carregando pacotes exigidos: ggplot2

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
#>          1 42 10  0
#>          2  9 42  9
#>          3  3 10 55
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.7722          
#>                  95% CI : (0.7039, 0.8313)
#>     No Information Rate : 0.3556          
#>     P-Value [Acc > NIR] : <2e-16          
#>                                           
#>                   Kappa : 0.6571          
#>                                           
#>  Mcnemar's Test P-Value : 0.3757          
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.7778   0.6774   0.8594
#> Specificity            0.9206   0.8475   0.8879
#> Pos Pred Value         0.8077   0.7000   0.8088
#> Neg Pred Value         0.9062   0.8333   0.9196
#> Prevalence             0.3000   0.3444   0.3556
#> Detection Rate         0.2333   0.2333   0.3056
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.8492   0.7624   0.8737

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
#>          1 49  2  1
#>          2  3 52  5
#>          3  0 14 54
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8611          
#>                  95% CI : (0.8018, 0.9081)
#>     No Information Rate : 0.3778          
#>     P-Value [Acc > NIR] : <2e-16          
#>                                           
#>                   Kappa : 0.791           
#>                                           
#>  Mcnemar's Test P-Value : 0.1409          
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9423   0.7647   0.9000
#> Specificity            0.9766   0.9286   0.8833
#> Pos Pred Value         0.9423   0.8667   0.7941
#> Neg Pred Value         0.9766   0.8667   0.9464
#> Prevalence             0.2889   0.3778   0.3333
#> Detection Rate         0.2722   0.2889   0.3000
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.9594   0.8466   0.8917

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
#>          1 49  3  0
#>          2  3 52  5
#>          3  0 14 54
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8611          
#>                  95% CI : (0.8018, 0.9081)
#>     No Information Rate : 0.3833          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7911          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9423   0.7536   0.9153
#> Specificity            0.9766   0.9279   0.8843
#> Pos Pred Value         0.9423   0.8667   0.7941
#> Neg Pred Value         0.9766   0.8583   0.9554
#> Prevalence             0.2889   0.3833   0.3278
#> Detection Rate         0.2722   0.2889   0.3000
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.9594   0.8408   0.8998

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
#>          1 35 17  0
#>          2  1 56  3
#>          3  0 16 52
#> 
#> Overall Statistics
#>                                          
#>                Accuracy : 0.7944         
#>                  95% CI : (0.728, 0.8509)
#>     No Information Rate : 0.4944         
#>     P-Value [Acc > NIR] : < 2.2e-16      
#>                                          
#>                   Kappa : 0.6895         
#>                                          
#>  Mcnemar's Test P-Value : NA             
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9722   0.6292   0.9455
#> Specificity            0.8819   0.9560   0.8720
#> Pos Pred Value         0.6731   0.9333   0.7647
#> Neg Pred Value         0.9922   0.7250   0.9732
#> Prevalence             0.2000   0.4944   0.3056
#> Detection Rate         0.1944   0.3111   0.2889
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.9271   0.7926   0.9087

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
#>          1 48  4  0
#>          2  4 53  3
#>          3  0 16 52
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.85            
#>                  95% CI : (0.7893, 0.8988)
#>     No Information Rate : 0.4056          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7747          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9231   0.7260   0.9455
#> Specificity            0.9688   0.9346   0.8720
#> Pos Pred Value         0.9231   0.8833   0.7647
#> Neg Pred Value         0.9688   0.8333   0.9732
#> Prevalence             0.2889   0.4056   0.3056
#> Detection Rate         0.2667   0.2944   0.2889
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.9459   0.8303   0.9087

saida <- predict(fit_NBT, test, type = "matrix")
```

#### Fuzzy Naive Bayes Trapezoidal


```r

fit_FNBT <- FuzzyTrapezoidalNaiveBayes(train =  Train[,-4],
                                    cl = Train[,4], cores = 4,
                                  fuzzy = T)
print(fit_FNBT)
#> 
#> Fuzzy Naive Bayes Trapezoidal Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_FNBT, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 36 16  0
#>          2  1 52  7
#>          3  0 14 54
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.7889          
#>                  95% CI : (0.7219, 0.8461)
#>     No Information Rate : 0.4556          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.6805          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9730   0.6341   0.8852
#> Specificity            0.8881   0.9184   0.8824
#> Pos Pred Value         0.6923   0.8667   0.7941
#> Neg Pred Value         0.9922   0.7500   0.9375
#> Prevalence             0.2056   0.4556   0.3389
#> Detection Rate         0.2000   0.2889   0.3000
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.9305   0.7763   0.8838

saida <- predict(fit_FNBT, test, type = "matrix")

# ----------------

fit_NBT <- FuzzyTrapezoidalNaiveBayes(train =  Train[,-4],
                                  cl = Train[,4], cores = 2,
                                  fuzzy = F)
print(fit_NBT)
#> 
#> Naive Bayes Trapezoidal Classifier for Discrete Predictors
#> 
#> Class:
#> [1] "1" "2" "3"

saida <- predict(fit_NBT, test)
confusionMatrix(factor(Test[,4]), saida)
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  2  3
#>          1 38 14  0
#>          2  1 51  8
#>          3  0 15 53
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.7889          
#>                  95% CI : (0.7219, 0.8461)
#>     No Information Rate : 0.4444          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.6807          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9744   0.6375   0.8689
#> Specificity            0.9007   0.9100   0.8739
#> Pos Pred Value         0.7308   0.8500   0.7794
#> Neg Pred Value         0.9922   0.7583   0.9286
#> Prevalence             0.2167   0.4444   0.3389
#> Detection Rate         0.2111   0.2833   0.2944
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.9375   0.7737   0.8714

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
#>          1 44  8  0
#>          2  1 50  9
#>          3  0 11 57
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.8389          
#>                  95% CI : (0.7769, 0.8894)
#>     No Information Rate : 0.3833          
#>     P-Value [Acc > NIR] : < 2.2e-16       
#>                                           
#>                   Kappa : 0.7564          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.9778   0.7246   0.8636
#> Specificity            0.9407   0.9099   0.9035
#> Pos Pred Value         0.8462   0.8333   0.8382
#> Neg Pred Value         0.9922   0.8417   0.9196
#> Prevalence             0.2500   0.3833   0.3667
#> Detection Rate         0.2444   0.2778   0.3167
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.9593   0.8173   0.8836

saida <- predict(fit_FENB, test, type = "matrix")
head(saida)
#>              1         2         3
#> [1,] 0.2894594 0.3391237 0.3714169
#> [2,] 0.3169201 0.3343985 0.3486814
#> [3,] 0.3144604 0.3299162 0.3556233
#> [4,] 0.3349515 0.3214146 0.3436339
#> [5,] 0.2858112 0.3397929 0.3743959
#> [6,] 0.3138168 0.3305043 0.3556789
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
#>          1 52  0  0
#>          2 18 42  0
#>          3  0 68  0
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.5222          
#>                  95% CI : (0.4466, 0.5971)
#>     No Information Rate : 0.6111          
#>     P-Value [Acc > NIR] : 0.9938          
#>                                           
#>                   Kappa : 0.3014          
#>                                           
#>  Mcnemar's Test P-Value : NA              
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity            0.7429   0.3818       NA
#> Specificity            1.0000   0.7429   0.6222
#> Pos Pred Value         1.0000   0.7000       NA
#> Neg Pred Value         0.8594   0.4333       NA
#> Prevalence             0.3889   0.6111   0.0000
#> Detection Rate         0.2889   0.2333   0.0000
#> Detection Prevalence   0.2889   0.3333   0.3778
#> Balanced Accuracy      0.8714   0.5623       NA

saida <- predict(fit_NBT, test, type = "matrix")
head(saida)
#>              1            2            3
#> [1,] 0.9999999 7.189903e-08 3.099129e-22
#> [2,] 0.9956464 4.353580e-03 4.046958e-13
#> [3,] 0.9992847 7.153056e-04 2.848775e-13
#> [4,] 0.9945190 5.480950e-03 5.116515e-10
#> [5,] 0.9999994 5.601865e-07 2.960668e-19
#> [6,] 0.9975604 2.439645e-03 9.723872e-14
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
#>          1  0 50  2
#>          2  0 48 12
#>          3  0 54 14
#> 
#> Overall Statistics
#>                                           
#>                Accuracy : 0.3444          
#>                  95% CI : (0.2753, 0.4188)
#>     No Information Rate : 0.8444          
#>     P-Value [Acc > NIR] : 1               
#>                                           
#>                   Kappa : 0.0064          
#>                                           
#>  Mcnemar's Test P-Value : <2e-16          
#> 
#> Statistics by Class:
#> 
#>                      Class: 1 Class: 2 Class: 3
#> Sensitivity                NA   0.3158  0.50000
#> Specificity            0.7111   0.5714  0.64474
#> Pos Pred Value             NA   0.8000  0.20588
#> Neg Pred Value             NA   0.1333  0.87500
#> Prevalence             0.0000   0.8444  0.15556
#> Detection Rate         0.0000   0.2667  0.07778
#> Detection Prevalence   0.2889   0.3333  0.37778
#> Balanced Accuracy          NA   0.4436  0.57237

saida <- predict(fit_NBT, test, type = "matrix")
head(saida)
#>              1            2            3
#> [1,] 0.9999999 7.189903e-08 3.099129e-22
#> [2,] 0.9956464 4.353580e-03 4.046958e-13
#> [3,] 0.9992847 7.153056e-04 2.848775e-13
#> [4,] 0.9945190 5.480950e-03 5.116515e-10
#> [5,] 0.9999994 5.601865e-07 2.960668e-19
#> [6,] 0.9975604 2.439645e-03 9.723872e-14
```
-->
