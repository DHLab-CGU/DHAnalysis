library(doParallel)
library(jsonlite)

context("RxNav")

test_that("Check the MESH return",
          {NationalDrugCodeList<-c("54092047612","00430078327","00406035705","00074433902","00310075190")
          expect_equal(nrow(getMESHInfoBasedonNDC(NationalDrugCodeList)),11)})


