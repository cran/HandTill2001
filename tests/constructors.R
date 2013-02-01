## from system.file("test-tools-1.R",      package = "Matrix"):
assertError <- function(expr) {
  d.expr <- deparse(substitute(expr))
  t.res <- tryCatch(expr, error = function(e) e)
  print(t.res)
  if(!inherits(t.res, "error"))
    stop(d.expr, "\n\t did not give an error", call. = FALSE)
  invisible(t.res)
}

library(HandTill2001)
data(ht01.twoclass)
new("bincap"
    , response = as.factor(ht01.twoclass$observed)
    , predicted = ht01.twoclass$predicted
    )


assertError(
            new("bincap"
                , response = ht01.twoclass$observed
                , predicted = ht01.twoclass$predicted
                )
            )
assertError(
            new("bincap"
                , response = as.factor(ht01.twoclass$observed)
                , predicted = as.data.frame(ht01.twoclass$predicted)
                )
            )
assertError(
            new("bincap"
                , true = 1
                )
            )
data(ht01.multipleclass)
new("multcap"
    , response = ht01.multipleclass$observed
    , predicted = as.matrix(ht01.multipleclass[, levels(ht01.multipleclass$observed)])
    )

assertError(
            new("multcap"
                , response = as.numeric(ht01.multipleclass$observed)
                , predicted = as.matrix(ht01.multipleclass[, levels(ht01.multipleclass$observed)])
                )
            )
assertError(
            new("multcap"
                , response = ht01.multipleclass$observed
                , predicted = ht01.multipleclass[, levels(ht01.multipleclass$observed)]
                )
            )
assertError(
            new("multcap"
                , true = 1
                )
            )

### Prediction matrix has a column that doesn't correspond to any of the levels of
### the factor of observed values. This can happen when predicting a subset of the
### observed values used for building a model.
df.sub <- (subset(ht01.multipleclass, observed != "Tabl"))
p <- df.sub[, ! names(df.sub)  %in%  c("observed")]
r <- df.sub[, c("observed")]
str(p)
str(r)
new("multcap"
    , response = r
    , predicted = as.matrix(p)
    )
### There is a level in the factor of observed values that does not correspond to
### any of the columns in the prediction matrix. This is an error although it may
### happen, for example when building a model using nnet::multinom with a level
### in the factor of observed values that occures only once.
p <- ht01.multipleclass[, ! names(ht01.multipleclass)  %in%  c("Con","observed")]
set.seed(123)
r <- ht01.multipleclass[sample(nrow(ht01.multipleclass),
                               size = nrow(p)
                               , replace = FALSE)
                        , c("observed")]
str(p)
str(r)
assertError(
            new("multcap"
                , response = r
                , predicted = as.matrix(p)
    )
)




