library(httptest)
library(mockery)

context("Login")

test_that("Login procedure works", {
  with_mock_api({
    expect_POST(sst <- superSecuredToken())
    expect_GET(sst <- temporaryToken("dummy"))
  })
})


test_that("Login unauthorized message is correct", {
  with_mock(
    status_code = mock(401L, cycle = T),
    build_mock_url = mock("test.com/login-401.json", cycle = T),
    with_mock_api({
      expect_error(sst <- superSecuredToken(), regexp = "401")
    })
  )

})

Sys.setenv(GOODDATA_DOMAIN = "test.com")
Sys.setenv(GOODDATA_PROJECT = "dummy")

test_that("Correct auth code is captured from response header", {
  with_mock_api({
    Sys.setenv(TZ='UTC')
    sst <- superSecuredToken()
    expect_match(sst, "^GDCAuthTT=")
  })
})
