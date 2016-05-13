library(h2o)
localH2O = h2o.init(nthreads = -1, max_mem_size = "8G")

train = "/Users/navdeepgill/Desktop/ToNewMac/Kaggle/SpringLeaf/train.csv"
test = "/Users/navdeepgill/Desktop/ToNewMac/Kaggle/SpringLeaf/test.csv"

data <- h2o.importFile(train)  
data$target = as.factor(data$target)
rand  <- h2o.runif(data, seed = 1)
train <- data[rand$rnd <= 0.8, ]
valid <- data[rand$rnd > 0.8, ]

y <- "target"
x <- setdiff(names(data), c(y,"ID"))

library(h2oEnsemble)
# Specify the base learner library & the metalearner
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.deeplearning.wrapper"
family <- "binomial"


# Train the ensemble using 5-fold CV to generate level-one data
# More CV folds will take longer to train, but should increase performance
fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train,
                    validation_frame = NULL,
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5, shuffle = TRUE))