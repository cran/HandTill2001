data(ht01.twoclass, package = "HandTill2001")
data(ht01.multipleclass, package = "HandTill2001")
message("AUC for a two class response")
HandTill2001::auc(HandTill2001::bincap(
  response = as.factor(ht01.twoclass$observed),
  predicted = ht01.twoclass$predicted,
  true = "1"
))
message("AUC for a multiple class response")
HandTill2001::auc(HandTill2001::multcap(
  response = ht01.multipleclass$observed,
  predicted = as.matrix(ht01.multipleclass[,
                        levels(ht01.multipleclass$observed)])
))
