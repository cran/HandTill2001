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
