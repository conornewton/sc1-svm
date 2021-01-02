library(parallel)
library(partitions)

# Partition data into test/train
data <- read.csv("./data/train.csv")
smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test  <- data[-train_ind, ]

# One versus One classifiers

# All unique lable, in this case 0-9
labels <- unique(train$label)

poly_ft_4 <- function(x) {
    parts <- as.matrix(parts(length(4)))
}

train_one_vs_one <- function() {
    pairs <- combn(labels, 2)
    #ws <- apply(pairs, 2, function(x) train_svm_ft(as.matrix(train[,-1]), as.vector(train[,1]), identity, label1 = x[1], label2 = x[2]))
    ws <- mclapply(1:45, function(x) train_svm_ft(as.matrix(train[,-1]), as.vector(train[,1]), identity, label1 = pairs[1, x], label2 = pairs[2, x]), mc.cores = 4)
    return(matrix(unlist(ws), ncol = ncol(pairs)))
}

predict_one_vs_one <- function(ws, x) {
    pairs <- combn(labels, 2)
    votes <- sapply(1:45, function(i) {
        if (sum(ws[, i] * c(1, x)) >= 1) return(pairs[1, i])
        else return(pairs[2, i])
    })

    # Does some magic to return the mode
    return(labels[which.max(tabulate(match(votes, labels)))])
}



# Support vector machine with feature transform
train_svm_ft <- function(x, y, ft, step0 = 1, c = 0.2, label1 = -1, label2 = -1) {
    w0 <- 0
    w <- rep(0, ncol(x))
    for (i in 1:50) {
        step <- step0 / i
        for (j in 1:(nrow(x))) {
            if (y[j] == label1) {
                yy <- 1
            } else if (y[j] == label2) {
                yy <- -1
            } else {
                next # Skip this iteration if y value is not needed
            }


            if (yy * (sum(w * ft(x[j, ])) + w0) <= 1) {
                w <- w + step * (yy * ft(x[j, ]) - c * w)
                w0 <- w0 + step * yy
            } else {
                # w <- w + step * (-2 * (1 / i) * w)
                # w0 <- w0 + step * (-2 * (1 / i) * w0)
            }
        }
    }
    # print(c(w0, as.vector(w)))
    return(c(w0, as.vector(w)))
}

test_svm <- function(ws, x, y) {
    res <- unlist(mclapply(seq_len(nrow(x)), function(i) {
        # print(predict_one_vs_one(ws, x[i,]))
        if (y[i] == predict_one_vs_one(ws, x[i, ])) {
            return(1)
        }
        return(0)
    }, mc.cores = 4))
    print(sum(res))
    print(length(res))
    return(sum(res) / length(res))
}



ptm <- proc.time()
ws <- train_one_vs_one()
print("finished training")
print(proc.time() - ptm)
#write.csv(ws, file = "ws.csv")
#ws <- read.csv("ws.csv")[-1]

ptm <- proc.time()
res <- test_svm(as.matrix(ws), as.matrix(train[,-1]), train$label) 
print("finished testing")
print(res)
print(proc.time() - ptm)
