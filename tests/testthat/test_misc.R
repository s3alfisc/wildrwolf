test_that("test cluster input", {
  
  # test that wildrwolf accepts formulas and characters
  # as fixest cluster input
  
  library(fixest)
  library(wildrwolf)
  
  dqrng::dqset.seed(123)
  models1 = feols(c(vs, am) ~ mpg | cyl, mtcars, cluster = "carb")
  rwolf1 = rwolf(models = models, param = "mpg", B = 9999)
  
  dqrng::dqset.seed(123)
  models2 = feols(c(vs, am) ~ mpg | cyl, mtcars, cluster = ~carb)
  rwolf2 = rwolf(models = models, param = "mpg", B = 9999)
  
  expect_equal(rwolf1, rwolf2)
  
  
})
