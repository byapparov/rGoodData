library(httptest)

Sys.setenv(GOODDATA_DOMAIN = "test.com")
Sys.setenv(GOODDATA_PROJECT = "dummy")

context("Report data")
with_mock_API({
  test_that("Report definition is extracted correctly", {

    res <- getLastDefinition(810164)
    # This is the last value in the provided fake file.
    expect_identical(res, 1090631L)
  })

  test_that("Correct report URI is returned", {
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

with_mock( # Test that data is extracted correctly from large report
  status_code = mock(201, 201, 200),
  buildMockURL = mock("test.com/report-data-201.json",
                      "test.com/report-data-201.json",
                      "test.com/report-data-200.csv"),
  http_type = mock("application/json", "application/json", "text/csv"),
  {
    with_mock_API({
       expect_output(res <- getReportData("test.com/dummy", 0.012), regexp = "0.012")
       expect_true(is.data.table(res))
       expect_identical(nrow(res), 2L)
    })
  }
)

with_mock( # Test bad response with text message
  status_code = mock(503, cycle = T),
  buildMockURL = mock("test.com/503.txt", cycle = T),
  http_type = mock("text", cycle = T),
  {
    with_mock_API({
      expect_error(res <- getLastDefinition(5232), regexp = "503")
    })
  }
)
