test_that("defaults", {
  data <- sim_design(seed = 8675309)
  
  cb <- codebook(data)
  
  compare <- '{
    "@type": "Dataset",
    "schemaVersion": "Psych-DS 0.1.0",
    "variableMeasured": [
        {
            "type": "PropertyValue",
            "unitText": "id",
            "name": "id",
            "missingValues": 0
        },
        {
            "type": "PropertyValue",
            "unitText": "y",
            "name": "y",
            "missingValues": 0,
            "minValue": -2.5969,
            "maxValue": 2.0294,
            "meanValue": 0.0523,
            "sdValue": 0.9291
        }
    ]
}
'
  class(compare) <- "json"
  
  expect_equal(cb, compare)
  
  
})

test_that("no coldesc", {
  data <- sim_design(2, 2, seed = 8675309)
  cb <- codebook(data, as_json = FALSE)
  
  vm <- cb$variableMeasured
  
  expect_equal(cb[["@type"]], "Dataset")
  expect_equal(cb[["schemaVersion"]], "Psych-DS 0.1.0")
  expect_equal(length(vm), 4)
  
  expect_equal(vm[[1]], list(type = "PropertyValue",
                             unitText = "id",
                             name = "id", 
                             missingValues = 0))
  
  expect_equal(vm[[2]], list(type = "PropertyValue",
                             unitText = "B",
                             name = "B", 
                             missingValues = 0,
                             levels = c(B1 = "B1", B2 = "B2")))
  
  expect_equal(vm[[3]], list(type = "PropertyValue",
                             unitText = "A1",
                             name = "A1", 
                             missingValues = 0,
                             minValue = -1.914629,
                             maxValue = 3.237439,
                             meanValue = 0.08935032,
                             sdValue = 0.9884778),
               tolerance = 1e-6, scale = 1)
  
  expect_equal(vm[[4]], list(type = "PropertyValue",
                             unitText = "A2",
                             name = "A2", 
                             missingValues = 0,
                             minValue = -2.722819,
                             maxValue = 2.869492,
                             meanValue = 0.0236779,
                             sdValue = 0.9744563),
               tolerance = 1e-6, scale = 1)
})

test_that("with coldesc", {
  data <- sim_design(2, 2, seed = 8675309)
  cb <- codebook(data, c(id = "Subject ID",
                         B = "Between-subject factor",
                         A1 = "Condition 1",
                         A2 = "Condition 2"),
                 as_json = FALSE)
  
  vm <- cb$variableMeasured
  
  expect_equal(cb[["@type"]], "Dataset")
  expect_equal(cb[["schemaVersion"]], "Psych-DS 0.1.0")
  expect_equal(length(vm), 4)
  
  expect_equal(vm[[1]], list(type = "PropertyValue",
                             unitText = "id",
                             name = "Subject ID", 
                             missingValues = 0))
  
  expect_equal(vm[[2]], list(type = "PropertyValue",
                             unitText = "B",
                             name = "Between-subject factor", 
                             missingValues = 0,
                             levels = c(B1 = "B1", B2 = "B2")))
  
  expect_equal(vm[[3]], list(type = "PropertyValue",
                             unitText = "A1",
                             name = "Condition 1", 
                             missingValues = 0,
                             minValue = -1.914629,
                             maxValue = 3.237439,
                             meanValue = 0.08935032,
                             sdValue = 0.9884778),
               tolerance = 1e-6, scale = 1)
  
  expect_equal(vm[[4]], list(type = "PropertyValue",
                             unitText = "A2",
                             name = "Condition 2", 
                             missingValues = 0,
                             minValue = -2.722819,
                             maxValue = 2.869492,
                             meanValue = 0.0236779,
                             sdValue = 0.9744563),
               tolerance = 1e-6, scale = 1)
})
