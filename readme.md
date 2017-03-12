[![Build Status](https://travis-ci.org/byapparov/rGoodData.svg?branch=master)](https://travis-ci.org/byapparov/rGoodData)
![codecov](https://codecov.io/gh/byapparov/rGoodData/branch/master/graph/badge.svg)](https://codecov.io/gh/byapparov/rGoodData)

With rGoodData package you should be able to access data via GoodData API from R.

Here is a short example:

```R
# Setup the required environment variables
Sys.setenv(GOODDATA_DOMAIN = "https://gooddata.com")
Sys.setenv(GOODDATA_PROJECT = "project-id")
Sys.setenv(GOODDATA_USER = "user-name")
Sys.setenv(GOODDATA_PASSWORD = "user-password")

library(rGoodData)
# get data for a given report object-id
def.obj <- getLastDefinition(1213086)
uri <- getReportRawUri(def.obj)
dt <- getReportData(uri)

```
