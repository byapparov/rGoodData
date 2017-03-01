library(httptest)

test_that("Report definition is extracted correctly", {
  with_mock_API({
    Sys.setenv(GOODDATA_DOMAIN = "test.com")
    Sys.setenv(GOODDATA_PROJECT = "dummy")
    res <- getLastDefinition(810164)

    # This is the last value in the provided fake file.
    expect_identical(res, 1090631L)
  })
})
