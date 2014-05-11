options( java.parameters = "-Xmx4g" )
library(RWeka)
source("R/data.R")
source("R/fs.R")

m30train <- dt.dmc$M30$train
m30test <- dt.dmc$M30$test
m31train <- dt.dmc$M31$train
m31test <- dt.dmc$M31$test

m30train <- m30train[m30train$deliveryDateMissing == "no", ]
m30trainF <- m30train[m30train$deliveryDateMissing == "no", ]
m31train <- m31train[m31train$deliveryDateMissing == "no", ]

m30test <- m30test[m30test$deliveryDateMissing == "no", ]
m30testF <- m30test[m30test$deliveryDateMissing == "no", ]
m31test <- m31test[m31test$deliveryDateMissing == "no", ]


m30train <- fs.noCustomer(fs.all(m30train))
m31train <- fs.all(m31train)

m30test <- fs.noCustomer(fs.all(m30test))
m31test <- fs.all(m31test)

m30trainAllF <- fs.all(m30trainF)
m30testAllF <- fs.all(m30testF)



write.arff(m30train, file="m30_train.arff")
write.arff(m31train, file="m31_train.arff")
write.arff(m30test, file="m30_test.arff")
write.arff(m31test, file="m31_test.arff")
write.arff(m30trainAllF, file="m30allf_train.arff")
write.arff(m30testAllF, file="m30allf_test.arff")