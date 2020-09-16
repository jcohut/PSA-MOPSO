library(emoa) # for hypervolume calculations
library(mco) # for HV calculation

## test functions
dtlz2_2 <- function(x) {
    n <- length(x)
    k <- n - 2 + 1
    g <- sum((x[2:n]-0.5)^2)
    f1 <- (1+g) * cos(x[1] * pi/2)
    f2 <- (1+g) * sin(x[1] * pi/2)
    c(f1, f2)
}
dtlz2_3 <- function(x) {
    n <- length(x)
    k <- n - 3 + 1
    g <- sum((x[3:n]-0.5)^2)
    f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2)
    f2 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
    f3 <- (1+g) * sin(x[1] * pi/2)
    c(f1, f2, f3)
}
dtlz2_4 <- function(x) {
    n <- length(x)
    k <- n - 4 + 1
    g <- sum((x[4:n]-0.5)^2)
    f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2)
    f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
    f3 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
    f4 <- (1+g) * sin(x[1] * pi/2)
    c(f1, f2, f3, f4)
}
dtlz2_5 <- function(x) {
    n <- length(x)
    k <- n - 5 + 1
    g <- sum((x[5:n]-0.5)^2)
    f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2)
    f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
    f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
    f4 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
    f5 <- (1+g) * sin(x[1] * pi/2)
    c(f1, f2, f3, f4, f5)
}

dtlz2_6 <- function(x) {
  n <- length(x)
  k <- n - 6 + 1
  g <- sum((x[6:n]-0.5)^2)
  f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2)
  f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * sin(x[5] * pi/2)
  f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
  f4 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
  f5 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
  f6 <- (1+g) * sin(x[1] * pi/2)
  c(f1, f2, f3, f4, f5, f6)
}

dtlz2_7 <- function(x) {
  n <- length(x)
  k <- n - 7 + 1
  g <- sum((x[7:n]-0.5)^2)
  f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2)
  f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * sin(x[6] * pi/2)
  f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * sin(x[5] * pi/2)
  f4 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
  f5 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
  f6 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
  f7 <- (1+g) * sin(x[1] * pi/2)
  c(f1, f2, f3, f4, f5, f6, f7)
}

dtlz2_8 <- function(x) {
  n <- length(x)
  k <- n - 8 + 1
  g <- sum((x[8:n]-0.5)^2)
  f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2)
  f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * sin(x[7] * pi/2)
  f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * sin(x[6] * pi/2)
  f4 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * sin(x[5] * pi/2)
  f5 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
  f6 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
  f7 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
  f8 <- (1+g) * sin(x[1] * pi/2)
  c(f1, f2, f3, f4, f5, f6, f7, f8)
}

dtlz2_10 <- function(x) {
  n <- length(x)
  k <- n - 10 + 1
  g <- sum((x[10:n]-0.5)^2)
  f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2)        
  f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * sin(x[9] * pi/2)
  f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * sin(x[8] * pi/2)
  f4 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * sin(x[7] * pi/2)
  f5 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * sin(x[6] * pi/2)
  f6 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * sin(x[5] * pi/2)
  f7 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
  f8 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
  f9 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
  f10 <- (1+g) * sin(x[1] * pi/2)
  c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
}

dtlz2_12 <- function(x) {
  n <- length(x)
  k <- n - 12 + 1
  g <- sum((x[12:n]-0.5)^2)
  f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2)        
  f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * sin(x[11] * pi/2)        
  f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * sin(x[10] * pi/2)         
  f4 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * sin(x[9] * pi/2)
  f5 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * sin(x[8] * pi/2)
  f6 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * sin(x[7] * pi/2)
  f7 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * sin(x[6] * pi/2)
  f8 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * sin(x[5] * pi/2)
  f9 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
  f10 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
  f11 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
  f12 <- (1+g) * sin(x[1] * pi/2)
  c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
}

dtlz2_15 <- function(x) {
  n <- length(x)
  k <- n - 15 + 1
  g <- sum((x[15:n]-0.5)^2)
  f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2)      
  f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * sin(x[14] * pi/2)        
  f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * sin(x[13] * pi/2)       
  f4 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * sin(x[12] * pi/2)        
  f5 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * sin(x[11] * pi/2)        
  f6 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * sin(x[10] * pi/2)         
  f7 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * sin(x[9] * pi/2)
  f8 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * sin(x[8] * pi/2)
  f9 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * sin(x[7] * pi/2)
  f10 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * sin(x[6] * pi/2)
  f11 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * sin(x[5] * pi/2)
  f12 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
  f13 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
  f14 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
  f15 <- (1+g) * sin(x[1] * pi/2)
  c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
}

dtlz2_16 <- function(x) {
  n <- length(x)
  k <- n - 16 + 1
  g <- sum((x[16:n]-0.5)^2)
  f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2) * cos(x[15] * pi/2)     
  f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2) * sin(x[15] * pi/2)      
  f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * sin(x[14] * pi/2)        
  f4 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * sin(x[13] * pi/2)       
  f5 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * sin(x[12] * pi/2)        
  f6 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * sin(x[11] * pi/2)        
  f7 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * sin(x[10] * pi/2)         
  f8 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * sin(x[9] * pi/2)
  f9 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * sin(x[8] * pi/2)
  f10 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * sin(x[7] * pi/2)
  f11 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * sin(x[6] * pi/2)
  f12 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * sin(x[5] * pi/2)
  f13 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
  f14 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
  f15 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
  f16 <- (1+g) * sin(x[1] * pi/2)
  c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
}

dtlz2_20 <- function(x) {
  n <- length(x)
  k <- n - 20 + 1
  g <- sum((x[20:n]-0.5)^2)
  f1 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2) * cos(x[15] * pi/2) * cos(x[16] * pi/2) * cos(x[17] * pi/2) * cos(x[18] * pi/2) * cos(x[19] * pi/2)      
  f2 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2) * cos(x[15] * pi/2) * cos(x[16] * pi/2) * cos(x[17] * pi/2) * cos(x[18] * pi/2) * sin(x[19] * pi/2)    
  f3 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2) * cos(x[15] * pi/2) * cos(x[16] * pi/2) * cos(x[17] * pi/2) * sin(x[18] * pi/2)     
  f4 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2) * cos(x[15] * pi/2) * cos(x[16] * pi/2) * sin(x[17] * pi/2)      
  f5 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2) * cos(x[15] * pi/2) * sin(x[16] * pi/2)     
  f6 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * cos(x[14] * pi/2) * sin(x[15] * pi/2)      
  f7 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * cos(x[13] * pi/2) * sin(x[14] * pi/2)        
  f8 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * cos(x[12] * pi/2) * sin(x[13] * pi/2)       
  f9 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * cos(x[11] * pi/2) * sin(x[12] * pi/2)        
  f10 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * cos(x[10] * pi/2) * sin(x[11] * pi/2)        
  f11 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * cos(x[9] * pi/2) * sin(x[10] * pi/2)         
  f12 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * cos(x[8] * pi/2) * sin(x[9] * pi/2)
  f13 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * cos(x[7] * pi/2) * sin(x[8] * pi/2)
  f14 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * cos(x[6] * pi/2) * sin(x[7] * pi/2)
  f15 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * cos(x[5] * pi/2) * sin(x[6] * pi/2)
  f16 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * cos(x[4] * pi/2) * sin(x[5] * pi/2)
  f17 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * cos(x[3] * pi/2) * sin(x[4] * pi/2)
  f18 <- (1+g) * cos(x[1] * pi/2) * cos(x[2] * pi/2) * sin(x[3] * pi/2)
  f19 <- (1+g) * cos(x[1] * pi/2) * sin(x[2] * pi/2)
  f20 <- (1+g) * sin(x[1] * pi/2)
  c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)
}

# n - how many solutions we need
dtlz2_2front <- function(n) {
    pf <- array(dim=c(n, 2))
    pf[,1] = seq(0,1,length=n)
    pf[,2] = sqrt(1 - pf[,1]^2)
    pf
}
dtlz2front <- function(n, m, f) {
    pf <- array(dim=c(n, m))
    x <- array(dim=c(n, m-1))
    x[,1] <- seq(0,1,length=n)
    for(i in 2:(m-1)) {
        x[,i] <- runif(n)
    }
    for(i in 1:n) {
        pf[i,] = f(c(x[i,], rep(0.5,10)))
    }
    pf
}
dtlz2_3front <- function(n) {dtlz2front(n, 3, dtlz2_3)}
dtlz2_4front <- function(n) {dtlz2front(n, 4, dtlz2_4)}
dtlz2_5front <- function(n) {dtlz2front(n, 5, dtlz2_5)}
dtlz2_6front <- function(n) {dtlz2front(n, 6, dtlz2_6)}
dtlz2_7front <- function(n) {dtlz2front(n, 7, dtlz2_7)}
dtlz2_8front <- function(n) {dtlz2front(n, 8, dtlz2_8)}
dtlz2_10front <- function(n) {dtlz2front(n, 10, dtlz2_10)}
dtlz2_12front <- function(n) {dtlz2front(n, 12, dtlz2_12)}
dtlz2_15front <- function(n) {dtlz2front(n, 15, dtlz2_15)}
dtlz2_16front <- function(n) {dtlz2front(n, 16, dtlz2_16)}
dtlz2_20front <- function(n) {dtlz2front(n, 20, dtlz2_20)}

test_functions2D <- c(dtlz2_2)
test_functions_fronts2D <- c(dtlz2_2front)
test_functions3D <- c(dtlz2_3)
test_functions_fronts3D <- c(dtlz2_3front)
test_functions4D <- c(dtlz2_4)
test_functions_fronts4D <- c(dtlz2_4front)
test_functions5D <- c(dtlz2_5)
test_functions_fronts5D <- c(dtlz2_5front)
test_functions6D <- c(dtlz2_6)
test_functions_fronts6D <- c(dtlz2_6front)
test_functions7D <- c(dtlz2_7)
test_functions_fronts7D <- c(dtlz2_7front)
test_functions8D <- c(dtlz2_8)
test_functions_fronts8D <- c(dtlz2_8front)
test_functions10D <- c(dtlz2_10)
test_functions_fronts10D <- c(dtlz2_10front)
test_functions12D <- c(dtlz2_12)
test_functions_fronts12D <- c(dtlz2_12front)
test_functions15D <- c(dtlz2_15)
test_functions_fronts15D <- c(dtlz2_15front)
test_functions16D <- c(dtlz2_16)
test_functions_fronts16D <- c(dtlz2_16front)
test_functions20D <- c(dtlz2_20)
test_functions_fronts20D <- c(dtlz2_20front)

## drawing functions
# plot 2d solution
draw2 <- function(m, title="", ...) {
    plot(m, xlab='f1', ylab='f2', pch=20, main=title, ...)
}

# plots for result of mopsocd
draw <- function(s, front, pfpoints=10000) {
    if (is.null(dim(s$objfnvalues))) {
        print("algorithm found no solutions")
        return()
    }
    pf <- front(pfpoints)
    if (s$fncnt == 2)
        drawf = draw2
    else
        drawf = draw3

    par(mfrow=c(2,3), xpd=NA)
    drawf(s$objfnvalues, "Nondominated solutions", col='green')
    #drawf(s$popFit, "Last population", col='black')
    drawf(s$storedFit, "All solutions", col=c(rep('red',dim(s$storedFit)[1])))
    #drawf(pf, "Pareto front", col='black')
    drawf(rbind(s$storedFit, s$objfnvalues), "All and nondominated solutions", col=c(rep('red',dim(s$storedFit)[1]), rep('green',dim(s$objfnvalues)[1])))
    drawf(rbind(s$objfnvalues, pf), "Nondominated solutions and\ntrue Pareto front", col=c(rep('green',dim(s$objfnvalues)[1]),rep('black',dim(pf)[1])))
}
# layout to compare 2 solutions
draw_n <- function(s1, s2, front, pfpoints=10000) {
    if (is.null(dim(s1$objfnvalues))) {
        print("algorithm 1 found no solutions")
        draw(s2, front)
        return()
    }
    if (is.null(dim(s2$objfnvalues))) {
        print("algorithm 2 found no solutions")
        draw(s1, front)
        return()
    }
    pf <- front(pfpoints)
    if (s1$fncnt == 2)
        drawf = draw2
    else
        drawf = draw3

    par(mfrow=c(1,2), xpd=NA)
        drawf(s1$objfnvalues, "Nondominated solutions", col='green')
        drawf(s2$objfnvalues, "Nondominated solutions", col='green')
    #par(mfrow=c(1,2), xpd=NA)
    #   drawf(s1$popFit, "Last population", col='black')
    #    drawf(s2$popFit, "Last population", col='black')
    par(mfrow=c(1,2), xpd=NA)
        drawf((s1$storedFit), "All solutions", col=c(rep('red',dim(s1$storedFit)[1])))
        drawf((s2$storedFit), "All solutions", col=c(rep('red',dim(s2$storedFit)[1])))
    # drawf(pf, "Pareto front", col='black')
    par(mfrow=c(1,2), xpd=NA)
        drawf(rbind(s1$storedFit, s1$objfnvalues), "All and nondominated solutions",
              col=c(rep('red',dim(s1$storedFit)[1]), rep('green',dim(s1$objfnvalues)[1])))
        drawf(rbind(s2$storedFit, s2$objfnvalues), "All and nondominated solutions",
              col=c(rep('red',dim(s2$storedFit)[1]), rep('green',dim(s2$objfnvalues)[1])))
    par(mfrow=c(1,2), xpd=NA)
        drawf(rbind(s1$objfnvalues, pf), "Nondominated solutions and\ntrue Pareto front", col=c(rep('green',dim(s1$objfnvalues)[1]),rep('black',dim(pf)[1])))
        drawf(rbind(s2$objfnvalues, pf), "Nondominated solutions and\ntrue Pareto front", col=c(rep('green',dim(s2$objfnvalues)[1]),rep('black',dim(pf)[1])))
}

# plot 3d solution
draw3 <- function(m, title="", ...) {
    # scatterplot3d has no rotation
    # scatterplot3d::scatterplot3d(s$objfnvalues[,1],s$objfnvalues[,2],s$objfnvalues[,3],
    #     xlab='f1', ylab='f2', zlab='f3',
    #     grid=TRUE, box=FALSE, pch=16)

    plot3D::scatter3D(m[,1],m[,2],m[,3],
        xlab='f1', ylab='f2', zlab='f3',
        theta=120, phi=30,  pch=20,
        bty='b2', axis.scales=TRUE, ticktype='detailed',
        main=title, ...)
}

draw3_rgl <- function(m, title="", ...) {
    # rgl solution
    car::scatter3d(m[,1],m[,2],m[,3],
        xlab='f1', ylab='f2', zlab='f3',
        point.col='black', axis.scales=TRUE, surface=FALSE)
    rgl::axes3d(nticks=20)
}

# mat is matrix of test runs with two algorithms
# NOTE: if there are enough titles is not checked at the moment
draw_boxplot <- function(mat, titles) {
    par(mfrow=c(1,4))
    boxplot(mat[,1:2], names=titles[1:2], na.action=na.omit)
    if (dim(mat)[2] > 2)
        boxplot(mat[,3:4], names=titles[3:4], na.action=na.omit)
    if (dim(mat)[2] > 4)
        boxplot(mat[,5:6], names=titles[5:6], na.action=na.omit)
    if (dim(mat)[2] > 6)
        boxplot(mat[,7:8], names=titles[7:8], na.action=na.omit)
    par(mfrow=c(1,3))
    if (dim(mat)[2] > 8)
        boxplot(mat[,9:10], names=titles[9:10], na.action=na.omit)
    if (dim(mat)[2] > 10)
        boxplot(mat[,11:12], names=titles[11:12], na.action=na.omit)
    if (dim(mat)[2] > 12)
        boxplot(mat[,13:14], names=titles[13:14], na.action=na.omit)
    if (dim(mat)[2] > 14) {
        par(mfrow=c(1,3))
        boxplot(mat[,15:16], names=titles[15:16], na.action=na.omit)
        boxplot(mat[,17:18], names=titles[17:18], na.action=na.omit)
        boxplot(mat[,19:20], names=titles[19:20], na.action=na.omit)
    }
}

## utility function to make comparisons between algorithms based on performance measure
# n - number of runs
# test_func - vector of test functions
# test_func_fronts - vector of test function fronts
# obj - number of objectives
# d - number of variables
# pfpoints - number of points to generate as Pareto Front
# alg_func - functions for 2 algorithms that are being compared
# returns 4-dim matrix with values of performance measures
perf_test <- function(n, test_func, test_func_fronts, obj, d, pfpoints=10000, alg_func=c(mopsocd, mopsopsa_v1)) {
    # list of functions for solution evaluation
    # (ahd is calculated directly, without a function,
    # for the sake of computational efficiency)
    perf_func <- c(gd, igd, hv, gs)
    # matrix to hold values of perf. measures
    mat <- array(0, dim=c(4, n, length(test_func) * 2)) # 4 performance measures, n runs, each test function is run with 2 algorithms
    # true pareto fronts calculated analytically
    truefronts <- list()
    for(t in seq_along(test_func))
        truefronts[[t]] <- test_func_fronts[[t]](pfpoints)

    # run algorithms and calculate & store performance values for n times
    for(i in 1:n) {
        # make runs for each testing function
        for(t in seq_along(test_func)) {
            # find solutions with two given algorithms
            s1 <- alg_func[[1]](test_func[[t]], varcnt=d, fncnt=obj, lowerbound=rep(0, d), upperbound=rep(1,d), opt=0)
            s2 <- alg_func[[2]](test_func[[t]], varcnt=d, fncnt=obj, lowerbound=rep(0, d), upperbound=rep(1,d), opt=0)
            if (is.null(dim(s1$objfnvalues)))
                print('can\'t calculate performance measure: 1st algorithm found no solutions; NA value assigned');
            if (is.null(dim(s2$objfnvalues)))
                print('can\'t calculate performance measure: 2nd algorithm found no solutions; NA value assigned');

            # calculate performance values for found solutions
            k <- 2 * (t-1) + 1
            for(j in seq_along(perf_func)) {
                if (is.null(dim(s1$objfnvalues))) {
                    mat[j, i, k] = NA
                } else {
                    mat[j, i, k] = perf_func[[j]](s1$objfnvalues, truefronts[[t]])
                }
                if (is.null(dim(s2$objfnvalues))) {
                    mat[j, i, k+1] = NA
                } else {
                    mat[j, i, k+1] = perf_func[[j]](s2$objfnvalues, truefronts[[t]])
                }
            }
            # ahd is max of gd and igd
            #mat[5, i, k] = max(mat[1, i, k], mat[2, i, k])
            #mat[5, i, k+1] = max(mat[1, i, k+1], mat[2, i, k+1])
        }
    }
    mat
}

## utility function that draws boxplots for 4 performance measures
# n - number of runs
# mat - matrix of test runs with two algorithms
# titles - names of test functions (and/or algorithms)
perf_boxplots <- function(n, mat, titles) {
    print(paste("GD performance measures on",n,"runs:"))
    draw_boxplot(mat[1,,], titles)
    print(paste("IGD performance measures on",n,"runs:"))
    draw_boxplot(mat[2,,], titles)
    print(paste("HV performance measures on",n,"runs:"))
    infs <- which(is.infinite(mat[3,,]))
    if (length(infs) != 0) {
        mat[3,,][infs] = NA
        print(paste('There were ', length(infs), ' Infs encountered; setting them to NA.', sep=''))
    }
    draw_boxplot(mat[3,,], titles)
    print(paste("GS performance measures on",n,"runs:"))
    draw_boxplot(mat[4,,], titles)
    #print(paste("AHD performance measures on",n,"runs:"))
    #draw_boxplot(mat[5,,], titles)
}

## performance measures
# Generational Distance
gd <- function (front, truefront) {
    d <- sapply(1:nrow(front), function(i) mco:::distanceToFront2(front[i,], truefront))
    return(sqrt(sum(d))/nrow(front))
}
# Inverted Generational Distance
igd <- function (front, truefront) {
    d <- sapply(1:nrow(truefront), function(i) mco:::distanceToFront2(truefront[i,], front))
    return(sqrt(sum(d))/nrow(truefront))
}
# Hypervolume
hv <- function(front, truefront) {
    dominatedHypervolume(front, c(rep(1.1, obj)))
}
# Generalized Spread
gs <- function(front, truefront) {
  generalizedSpread(front, truefront)
}
# Averaged Hausdorff Distance
#ahd <- function(front, truefront) {
    # if one or both values are NA, result will be NA
 #   max(gd(front, truefront), igd(front, truefront))
#}