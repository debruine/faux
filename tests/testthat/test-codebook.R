test_that("defaults", {
  data <- sim_design(seed = 8675309)
  
  cb <- codebook(data)
  
  compare <- '{
    "@context": "https://schema.org/",
    "@type": "Dataset",
    "name": "data",
    "schemaVersion": "Psych-DS 0.1.0",
    "variableMeasured": [
        {
            "@type": "PropertyValue",
            "name": "id",
            "description": "id",
            "type": "string"
        },
        {
            "@type": "PropertyValue",
            "name": "y",
            "description": "value",
            "type": "float"
        }
    ]
}
'
  class(compare) <- "json"
  
  expect_equal(cb, compare)
})

test_that("warnings", {
  data <- sim_design(seed = 8675309)
  # all valid properties
  vardesc <- list("description" = c(id = "Subject ID"), 
                  "privacy" = c(T, F), 
                  "type" = c("factor", "float"),
                  "propertyID" = c(id = "ID7"), 
                  "minValue" = c(y = -100), 
                  "maxValue" = c(y = 100),
                  "levels" = list(id = LETTERS), 
                  "ordered" = c(id = TRUE), 
                  "na" = c(id = "NA"), 
                  "naValues" = c(id = "NOPE"),
                  "alternateName" = c(id = "part_id"), 
                  "unitCode" = c(id = "huh?"))
  expect_silent(cb <- codebook(data, "data", vardesc, as_json = FALSE))
  
  expect_equal(cb$variableMeasured[[1]], 
               list(`@type` = "PropertyValue",
                     name = "id",
                     description ="Subject ID",
                     privacy =TRUE,
                     type ="factor",
                     propertyID = "ID7",
                     levels = LETTERS,
                     ordered = TRUE,
                     na = "NA",
                     naValues = "NOPE",
                     alternateName = "part_id",
                     unitCode = "huh?"))
  
  expect_equal(cb$variableMeasured[[2]], 
               list(`@type` = "PropertyValue",
                    name = "y",
                    description= "value",
                    privacy = FALSE,
                    type = "float",
                    minValue = -100,
                    maxValue = 100))
  
  # add an invalid property
  vardesc$invalid <- c(id = "STOP")
  expect_warning(cb <- codebook(data, "data", vardesc, as_json = FALSE),
                 "The following variable properties are not standard: invalid", fixed = TRUE)
})

test_that("no vardesc", {
  data <- sim_design(2, 2, seed = 8675309, plot = FALSE)
  cb <- codebook(data, as_json = FALSE)
  
  vm <- cb$variableMeasured
  
  expect_equal(cb[["@type"]], "Dataset")
  expect_equal(cb[["schemaVersion"]], "Psych-DS 0.1.0")
  expect_equal(length(vm), 4)
  
  expect_equal(vm[[1]], list(`@type` = "PropertyValue",
                             name = "id",
                             description = "id", 
                             type = "string"))
  
  expect_equal(vm[[2]], list(`@type` = "PropertyValue",
                             name = "B",
                             description = "B",
                             levels = list(B1 = "B1", B2 = "B2"),
                             type = "factor",
                             ordered = FALSE))
  
  expect_equal(vm[[3]], list(`@type` = "PropertyValue",
                             name = "A1",
                             description = "A1", 
                             type = "float"))
  
  expect_equal(vm[[4]], list(`@type` = "PropertyValue",
                             name = "A2",
                             description = "A2", 
                             type = "float"))
})

test_that("named factor levels", {
  between <- list(
    pet = c(cat = "Has a cat", dog = "Has a dog")
  )
  data <- sim_design(between = between,
                     dv = list(y = "Happiness Score"),
                     id = list(id = "Subject ID"),
                     plot = FALSE)
  expect_message(cb <- codebook(data), "id set to type string")
  expect_message(cb <- codebook(data), "pet set to type factor")
  expect_message(cb <- codebook(data, as_json = FALSE), "y set to type float")
  
  output <- capture_output(print(cb))
  expect_equal(output, "Codebook for data (Psych-DS 0.1.0)\n* id (string): Subject ID\n* pet (factor)\n  * Levels\n    * cat: Has a cat\n    * dog: Has a dog\n  * Ordered: FALSE\n* y (float): Happiness Score")
})

test_that("with vardesc", {
  data <- sim_design(2, 2, seed = 8675309, plot = FALSE)
  vardesc <- list(description = c(id = "Subject ID",
                                  B = "Between-subject factor",
                                  A1 = "Condition 1",
                                  A2 = "Condition 2"),
                  levels = list(B = c(B1 = "First level",
                                      B2 = "Second level")))
  cb <- codebook(data, "My Data", vardesc, as_json = FALSE)
  
  output <- capture_output(print(cb))
  expect_equal(output, "Codebook for My Data (Psych-DS 0.1.0)\n* id (string): Subject ID\n* B (factor): Between-subject factor\n  * Levels\n    * B1: First level\n    * B2: Second level\n  * Ordered: FALSE\n* A1 (float): Condition 1\n* A2 (float): Condition 2")
  
  # unseen levels
  vardesc <- list(description = c(id = "Subject ID",
                                  B = "Between-subject factor",
                                  A1 = "Condition 1",
                                  A2 = "Condition 2"),
                  levels = list(B = c(B1 = "First level",
                                      B2 = "Second level",
                                      B3 = "Third level")))
  cb <- codebook(data, "My Data", vardesc, as_json = FALSE)
  # should convert levels to a list
  expect_equal(cb$variableMeasured[[2]]$levels,
               list(B1 = "First level",
                    B2 = "Second level",
                    B3 = "Third level"))
})

test_that("ignores extra vardesc", {
  data <- sim_design(2, 2, seed = 8675309, plot = FALSE)
  vardesc <- list(description = c(id = "Subject ID",
                                  B = "Between-subject factor",
                                  C = "Extra Factor",
                                  A1 = "Condition 1",
                                  A2 = "Condition 2"),
                  levels = list(B = c(B1 = "First level",
                                      B2 = "Second level"),
                                C = c("C1", "C2"))
  )
  
  # turn off messages to check there are no warnings
  faux_options(verbose = FALSE)
  expect_silent(
    cb <- codebook(data, "My Data", vardesc, as_json = FALSE)
  )
  faux_options(verbose = TRUE)
})