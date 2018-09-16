library(httptest)
library(mockery)

context("Schedule")

Sys.setenv(GOODDATA_DOMAIN = "test.com")
Sys.setenv(GOODDATA_PROJECT = "dummy")


test_that("Schedule execution sends POST request" , {
  without_internet(expect_POST(schedule.uri <- executeSchedule("abcd")))
})

with_mock(authCookie = mock("", cycle = F), {
  with_mock_API({
    test_that("Schedule execution works" , {
      execution <- executeSchedule("abcd")
      expect_equal(execution, "test.com/gdc/projects/dummy/schedules/abcd/executions/1234")
    })
  })
})



