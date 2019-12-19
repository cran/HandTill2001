test_exception <- function() {
    RUnit::checkException(HandTill2001:::throw("Hello, error!"))
}
