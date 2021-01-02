library(ggplot2)

# Test data
data <- data.frame(x1 = c(1, 2, 3, 5, 6, 7), x2 = c(7, 8, 8, 1, -1, 3),  y = c(-1, -1, -1, 1, 1, 1))

# If linearly seperable can use c = 0?
train_svm_linear <- function(x, y, step0 = 0.01, c = 0) {
    w0 <- 0
    w <- rep(0, ncol(x))
    for (i in 1:10) {
        step <- step0 / i
        for (j in 1:(nrow(x))) {
            if (y[j] * (sum(w * x[j, ]) + w0) <= 1) {
                w <- w + step * (y[j] * x[j, ] - c * w)
                w0 <- w0 + step * y[j]
            }
        }
    }
    return(c(w0, as.vector(w)))
}

w <- train_svm_linear(as.matrix(data[,-3]), as.vector(data[,3]))

boundary <- function(x) {
    -(w[1] + w[2] * x) / w[3]
}


ggplot() + geom_point(data = data, aes(x1, x2, color = y)) + stat_function(fun = boundary)

data1 <- data.frame(x1 = c(rnorm(n = 100, mean = 1, sd = 0.8), rnorm(n = 100, mean = -1, sd = 0.8)),
                   x2 = c(rnorm(n = 100, mean = 1, sd = 0.8), rnorm(n = 100, mean = -1, sd = 0.8)),
                   y = c(rep(1, 100), rep(-1, 100)))

w <- train_svm_linear(as.matrix(data1[,-3]), as.vector(data1[,3]), c = 0.5)

ggplot() + geom_point(data = data1, aes(x1, x2, color = y)) + stat_function(fun = boundary)

r <- runif(n = 100, min = 0, max = 2 * pi)
data2 <- data.frame(x1 = c(3 * sin(r) + rnorm(n = 100, mean = 0, sd = 0.5), rnorm(n = 100, mean = 0, sd = 0.8)),
                   x2 = c(3 * cos(r) + rnorm(n = 100, mean = 0, sd = 0.5), rnorm(n = 100, mean = 0, sd = 0.8)),
                   y = c(rep(1, 100), rep(-1, 100)))


ft <- function(x) c(x[1]^2, x[2]^2)

# Support vector machine with feature transform
train_svm_ft <- function(x, y, ft, step0 = 1, c = 0.2) {
    w0 <- 0
    w <- rep(0, ncol(x))
    for (i in 1:10000) {
        step <- step0 / i
        for (j in 1:(nrow(x))) {
            if (y[j] * (sum(w * ft(x[j, ])) + w0) <= 1) {
                w <- w + step * (y[j] * ft(x[j, ]) - c * w)
                w0 <- w0 + step * y[j]
            } else {
                # w <- w + step * (-2 * (1 / i) * w)
                # w0 <- w0 + step * (-2 * (1 / i) * w0)
            }
        }
    }
    return(c(w0, as.vector(w)))
}

w <- train_svm_ft(as.matrix(data2[,-3]), as.vector(data2[,3]), ft)

r <- runif(n = 10000, min = 0, max = 2 * pi)
ellipse_data <- data.frame(x1 = sqrt((1 - w[1]) / w[2]) * cos(r), x2 = sqrt((1 - w[1])/ w[3]) * sin(r))

ggplot()+ geom_point(data = ellipse_data, aes(x1, x2), size = 1, color = "green") + geom_point(data = data2, aes(x1, x2, color = y)) 

test_svm_ft <- function(w, x, y, ft) {
    res <- sapply(1:nrow(x), function(i) {
        fx <- sum(w * c(1, ft(x[i, ])))
        if (fx >= 1 && y[i] == 1 || fx < 1 && y[i] == -1) {
            return(1)
        } else {
            return(0)
        }
    })
    return(sum(res) / length(res))
}


res <- test_svm_ft(w, as.matrix(data2[,-3]), as.vector(data2[,3]), ft)

print(res)
