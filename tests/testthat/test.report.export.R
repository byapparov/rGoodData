library(httptest)

with_mock_API({
  test_that("Report definition is extracted correctly", {

    Sys.setenv(GOODDATA_DOMAIN = "test.com")
    Sys.setenv(GOODDATA_PROJECT = "dummy")
    res <- getLastDefinition(810164)

    # This is the last value in the provided fake file.
    expect_identical(res, 1090631L)
  })

  test_that("Correct report URI is returned", {
    Sys.setenv(GOODDATA_DOMAIN = "test.com")
    Sys.setenv(GOODDATA_PROJECT = "dummy")
    res <- getReportRawUri(111111)
    expect_identical(res, "test.com/test/uri")
  })

})
