test_that("test cluster input", {
  
  # test that wildrwolf accepts formulas and characters
  # as fixest cluster input
  
  library(fixest)
  library(wildrwolf)
  
  set.seed(123)
  models1 = feols(c(vs, am) ~ mpg | cyl, mtcars, cluster = "carb")
  rwolf1 = rwolf(models = models1, param = "mpg", B = 999)
  
  set.seed(123)
  models2 = feols(c(vs, am) ~ mpg | cyl, mtcars, cluster = ~carb)
  rwolf2 = rwolf(models = models2, param = "mpg", B = 999)
  
  expect_equal(rwolf1, rwolf2)
  
  
})
