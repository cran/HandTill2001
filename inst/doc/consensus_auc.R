### R code from vignette source 'consensus_auc.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: consensus_auc.Rnw:20-21
###################################################
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 2: consensus_auc.Rnw:27-33
###################################################
library(MASS)
data(fgl)
set.seed(100)
ind_train <- sample(nrow(fgl), size = floor(nrow(fgl) * 0.7),
                    replace = FALSE)
ind_eval <- setdiff(seq(1:nrow(fgl)), ind_train)


###################################################
### code chunk number 3: consensus_auc.Rnw:36-47
###################################################
library(rpart)
set.seed(123)
fgl_rpart <- rpart(type ~ ., data = fgl[ind_train, TRUE], 
                   parms = list(split = "information"))

newcp <- max(fgl_rpart$cptable[,"CP"] * 
             as.numeric(fgl_rpart$cptable[TRUE ,"xerror"] <
                        sum(fgl_rpart$cptable[dim(fgl_rpart$cptable)[1],
                            c("xerror","xstd")]))
         ) + 1e-13
fgl_rpart_pruned <- prune(fgl_rpart, cp = newcp)


###################################################
### code chunk number 4: consensus_auc.Rnw:50-53
###################################################
library(nnet)
fgl_multinom <- multinom(type ~ ., data = fgl[ind_train, TRUE],
                         trace = FALSE)


###################################################
### code chunk number 5: consensus_auc.Rnw:56-63
###################################################
library(mda)
confusion(predict(fgl_rpart_pruned, newdata = fgl[ind_eval, TRUE],
                  type = "class"),
          fgl[ind_eval, "type"])
confusion(predict(fgl_multinom, newdata = fgl[ind_eval, TRUE],
                  type = "class"),
          fgl[ind_eval, "type"])


###################################################
### code chunk number 6: consensus_auc.Rnw:66-75
###################################################
library(HandTill2001)
auc(multcap(response = fgl[ind_eval, "type"], 
            predicted = predict(fgl_rpart_pruned, 
                                newdata = fgl[ind_eval, TRUE])))

auc(multcap(response = fgl[ind_eval, "type"],
            predicted = predict(fgl_multinom,
                                newdata = fgl[ind_eval, TRUE],
                                type = "probs")))


###################################################
### code chunk number 7: consensus_auc.Rnw:81-86
###################################################
set.seed(100)
ind_inner_train <- sample(ind_train, 
                          size = floor(length(ind_train) * 0.7),
                          replace = FALSE)
ind_inner_eval <- setdiff(ind_train, ind_inner_train)


###################################################
### code chunk number 8: consensus_auc.Rnw:89-100
###################################################
wa_fgl_multinom <- multinom(fgl_multinom, data = fgl[ind_inner_train, ],
                            trace = FALSE)
wa_fgl_rpart <- rpart(type ~ ., data = fgl[ind_inner_train, ],
                      parms = list(split = "information")
                      )
newcp <- max(wa_fgl_rpart$cptable[,"CP"] *
             as.numeric(wa_fgl_rpart$cptable[TRUE ,"xerror"] <
                        sum(wa_fgl_rpart$cptable[dim(wa_fgl_rpart$cptable)[1],
                            c("xerror","xstd")]))
             ) + 1e-13
wa_fgl_rpart_pruned <- prune(wa_fgl_rpart, cp = newcp)


###################################################
### code chunk number 9: consensus_auc.Rnw:103-114
###################################################
li <- list()
li$rpart$auc <- auc(multcap(response = fgl[ind_inner_eval, "type"],
                            predicted = predict(wa_fgl_rpart_pruned,
                                                newdata = fgl[ind_inner_eval, 
                                                              TRUE]
                                                )))
li$mllm$auc <- auc(multcap(response = fgl[ind_inner_eval, "type"],
                           predicted = predict(wa_fgl_multinom,
                                               newdata = fgl[ind_inner_eval,
                                                             TRUE],
                                               type = "probs")))


###################################################
### code chunk number 10: consensus_auc.Rnw:117-122
###################################################
li$rpart$predictions <- predict(fgl_rpart_pruned,
                                newdata = fgl[ind_eval, TRUE])
li$mllm$predictions <- predict(fgl_multinom,
                               newdata = fgl[ind_eval, TRUE],
                               type = "probs")


###################################################
### code chunk number 11: consensus_auc.Rnw:125-128
###################################################
predicted <- Reduce('+', lapply(li, function(x)
				x$auc * x$predictions)
) / Reduce('+', sapply(li,"[", "auc"))


###################################################
### code chunk number 12: consensus_auc.Rnw:131-133
###################################################
auc(multcap(response = fgl[ind_eval, "type"],
            predicted = predicted))


###################################################
### code chunk number 13: consensus_auc.Rnw:136-141
###################################################
classes_predicted <-
    factor(x = apply(predicted, 1, function(x)
                     dimnames(predicted)[[2]][which.max(x)]),
           levels = levels(fgl[ind_eval, "type"])	 
           )


