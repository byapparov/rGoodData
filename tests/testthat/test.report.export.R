library(httptest)

context("Report data")
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

context("Error Codes")
library(mockery)

with_mock(
  status_code = mock(401, cycle = T),
  buildMockURL = mock("test.com/401.json", cycle = T),
  {
    with_mock_API({
      expect_error(res <- getLastDefinition(810164), regex = "401")
      expect_error(res <- getReportRawUri(810164), regex = "401")
      expect_error(res <- getReportData("test.com/dummy"), regex = "401")
    })
  }
)

with_mock(
  status_code = mock(201, 201, 200),
  buildMockURL = mock("test.com/report-data-201.json",
                      "test.com/report-data-201.json",
                      "test.com/report-data-200.csv"),
  getResponseType = mock("application/json", "application/json", "text/csv"),
  {
    with_mock_API({
       expect_output(res <- getReportData("test.com/dummy", 0.012), regexp = "0.012")
       expect_true(is.data.table(res))
       expect_identical(nrow(res), 2L)
    })
  }
)
