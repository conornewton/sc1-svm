# Unbalanced data may cause issues!
# TODO: Remove rows that have missing data

diabetes_data <- read.csv("./data/diabetes.csv")

# Data is unbalanced! 

# change y values from 0 to -1
is_zero <- function(x) {
    return(x == 0)
}

diabetes_data$X1[is_zero(diabetes_data$X1)] <- -1

smp_size <- floor(0.75 * nrow(diabetes_data)) # Sample ~75% of data for training
train_ind <- sample(seq_len(nrow(diabetes_data)), size = smp_size)
train <- diabetes_data[train_ind, ]
test  <- diabetes_data[-train_ind, ]

train_svm_ft <- function(x, y, ft, step0 = 1, c = 0.2) {
    w0 <- 0
    w <- rep(0, ncol(x))
    for (i in 1:10) {
        step <- step0 / i
        for (j in 1:(nrow(x))) {
            if (y[j] * (sum(w * ft(x[j, ])) + w0) <= 1) {
                w <- w + step * (y[j] * ft(x[j, ]) - c * w)
                w0 <- w0 + step * y[j]
            }
        }
    }
    return(c(w0, as.vector(w)))
}

test_svm_ft <- function(w, x, y, ft) {
    res <- sapply(1:nrow(x), function(i) {
       fx <- sum(w * c(1, x[i, ]))
        if (fx >= 1 && y[i] == 1 || fx < 1 && y[i] == -1) {
            return(1)
        } else {
            return(0)
        }
    })
    return(sum(res) / length(res))
}

train_svm_ft <- function(x, y, ft, step0 = 1, c = 0.8) {
    w0 <- 0
    w <- rep(0, ncol(x))
    for (i in 1:10000) {
        step <- step0 / i
        for (j in 1:(nrow(x))) {
            if (y[j] * (sum(w * ft(x[j, ])) + w0) <= 1) {
                w <- w + step * (y[j] * ft(x[j, ]) - c * w)
                w0 <- w0 + step * y[j]
            }
        }
    }
    return(c(w0, as.vector(w)))
}

# degree 4 polynomial transform
poly <- function(x) {
    #TODO: make a polynomial transform work
}

w <- train_svm_ft(as.matrix(train[, -ncol(train)]), as.vector(train[,ncol(train)]), ft = function(x) x)
res <- test_svm_ft(w, as.matrix(test[, -ncol(test)]), as.vector(test[,ncol(test)]), ft = function(x) x)

print(res)

# Rougly 70% performance
