## -- download data and extract data set, sample a portion
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip",
              "/tmp/bank.zip")
datfull <- read.csv(unz("/tmp/bank.zip", "bank-full.csv"), sep=";")
set.seed(123)
dat <- datfull[sample(nrow(datfull), 1000, replace=FALSE),]
saveRDS(dat, "bankSample.rds")
