## OPTIONS ----

# Run all tests
testthat::test_dir("tests/testthat")

# Run all tests with different reporters
testthat::test_dir("tests/testthat", reporter = testthat::LocationReporter)
testthat::test_dir("tests/testthat", reporter = testthat::SummaryReporter)


## RUN TEST SUBSETS ----

# Run single-site example tests only
testthat::test_dir("tests/testthat", filter = "single-site")

# Run multi-site example tests only
testthat::test_dir("tests/testthat", filter = "multi-site")

# Run parallel-vs-sequential consistency tests only
testthat::test_dir("tests/testthat", filter = "parallel")

# Run numeric regression tests against BiomeBGCR reference outputs only
testthat::test_dir("tests/testthat", filter = "validation-reference")

# Run integration tests only
testthat::test_dir("tests/testthat", filter = "integration")
