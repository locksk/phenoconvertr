#' Function to generate a test data set that phenoconverteR can be tested on.
#'
#' @returns a dataframe containing 100 participants with information on age, sex, weight, dose, medication, and activity scores/metabolism phenotypes for 3 CYP enzymes.
#'
#' @examples
#' test_df <- test_data()
#'
#' @import dplyr
#' @import wakefield
#'
#' @export
#'
test_data <- function() {

  # clozapine pharmacogenomics dataset
  test <- data.frame(ID = seq(1, 100, by=1),
                     age = sample(18:75, 100, replace = TRUE),
                     sex = sex(100, x = c("Male", "Female"), prob = c(0.512, 0.488)),
                     weight = NA,
                     daily_dose = NA,
                     other_meds = NA,
                     CYP1A2_MP = NA,
                     CYP1A2_AS = NA,
                     CYP2D6_MP = NA,
                     CYP2D6_AS = NA,
                     CYP3A4_MP = NA,
                     CYP3A4_AS = NA
  )

  test$weight[test$sex == "Male"] <-  sample(60:110, length(test$weight[test$sex == "Male"]), replace = TRUE)
  test$weight[test$sex == "Female"] <-  sample(55:85, length(test$weight[test$sex == "Female"]), replace = TRUE)


  dd <- c(100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400, 425, 450, 475, 500, 525, 550, 575, 600, 625, 650, 675, 700, 725, 750, 775, 800, 825, 850, 875, 900, 925, 950, 975, 1000)
  test$daily_dose <-  sample(dd, 100, replace = TRUE, prob = c(0.0118, 0.0127, 0.0166, 0.0254, 0.05, 0.047, 0.077, 0.043, 0.169, 0.041, 0.089, 0.028, 0.105, 0.024, 0.055, 0.012, 0.057, 0.01, 0.021, 0.006, 0.029, 0.004, 0.016, 0.005, 0.0166, 0.0017, 0.0057, 0.0004, 0.006, 0.0017, 0.004, 0.0004, 0.003, 0, 0, 0.004, 0.0003))

  as <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  test$CYP1A2_AS[test$daily_dose >= 500] <-  sample(as, length(test$CYP1A2_AS[test$daily_dose >= 500]), replace = TRUE, prob = c(0.02, 0.03, 0.05, 0.10, 0.15, 0.25, 0.40))
  test$CYP1A2_AS[test$daily_dose < 500] <-  sample(as, length(test$CYP1A2_AS[test$daily_dose < 500]), replace = TRUE, prob = c(0.05, 0.07, 0.10, 0.13, 0.18, 0.20, 0.27))
  test$CYP1A2_MP[test$CYP1A2_AS >= 2] <- "Rapid Metaboliser"
  test$CYP1A2_MP[test$CYP1A2_AS >= 3] <- "Ultra-rapid Metaboliser"
  test$CYP1A2_MP[test$CYP1A2_AS < 2] <- "Normal Metaboliser"
  test$CYP1A2_MP[test$CYP1A2_AS < 1] <- "Intermediate Metaboliser"
  test$CYP1A2_MP[test$CYP1A2_AS == 0] <- "Poor Metaboliser"


  as_a <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  test$CYP2D6_AS[test$daily_dose >= 650] <-  sample(as, length(test$CYP2D6_AS[test$daily_dose >= 650]), replace = TRUE, prob = c(0.02, 0.03, 0.05, 0.25, 0.15, 0.25, 0.15))
  test$CYP2D6_AS[test$daily_dose < 650] <-  sample(as, length(test$CYP2D6_AS[test$daily_dose < 650]), replace = TRUE, prob = c(0.05, 0.07, 0.15, 0.28, 0.23, 0.15, 0.07))
  test$CYP2D6_MP[test$CYP2D6_AS > 2] <- "Rapid Metaboliser"
  test$CYP2D6_MP[test$CYP2D6_AS >= 3] <- "Ultra-rapid Metaboliser"
  test$CYP2D6_MP[test$CYP2D6_AS <= 2] <- "Normal Metaboliser"
  test$CYP2D6_MP[test$CYP2D6_AS < 1] <- "Intermediate Metaboliser"
  test$CYP2D6_MP[test$CYP2D6_AS == 0] <- "Poor Metaboliser"

  as_b <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  test$CYP3A4_AS[test$daily_dose >= 350] <-  sample(as, length(test$CYP3A4_AS[test$daily_dose >= 350]), replace = TRUE, prob = c(0.01, 0.015, 0.05, 0.90, 0.015, 0.01, 0.01))
  test$CYP3A4_AS[test$daily_dose < 350] <-  sample(as, length(test$CYP3A4_AS[test$daily_dose < 350]), replace = TRUE, prob = c(0.01, 0.015, 0.05, 0.90, 0.015, 0.01, 0.01))
  test$CYP3A4_MP[test$CYP3A4_AS >= 3] <- "Ultra-rapid Metaboliser"
  test$CYP3A4_MP[test$CYP3A4_AS <= 2] <- "Normal Metaboliser"
  test$CYP3A4_MP[test$CYP3A4_AS < 1] <- "Intermediate Metaboliser"
  test$CYP3A4_MP[test$CYP3A4_AS == 0] <- "Poor Metaboliser"

  meds <- c(1:5)
  test$other_meds <- sample(meds, 100, replace = TRUE, prob = c(0.5, 0.17, 0.15, 0.13, 0.05))
  test$other_meds[test$other_meds == 1] <- NA
  test$other_meds[test$other_meds == 3] <- "Fluoxetine"
  test$other_meds[test$other_meds == 4] <- "Fluvoxamine"
  test$other_meds[test$other_meds == 5] <- "Cigarettes"
  test$other_meds[test$other_meds == 2] <- "Quinidine"

  return(test)
}

#' Function to generate a more complex test data set that phenoconverteR can be tested on. This has multiple medication columns, and therefore should be used with the 'transform_and_clean' function.
#'
#' @returns a dataframe containing 100 participants with information on age, sex, weight, dose, multiple medication columns, and activity scores/metabolism phenotypes for 3 CYP enzymes.
#'
#' @examples
#' test_df <- test_data_multimed()
#'
#' @import dplyr
#' @import wakefield
#'
#' @export
test_data_multimed <- function() {

  # clozapine pharmacogenomics dataset
  test <- data.frame(ID = seq(1, 100, by=1),
                     age = sample(18:75, 100, replace = TRUE),
                     sex = sex(100, x = c("Male", "Female"), prob = c(0.512, 0.488)),
                     weight = NA,
                     daily_dose = NA,
                     other_meds1 = NA,
                     other_meds2 = NA,
                     other_meds3 = NA,
                     CYP1A2_MP = NA,
                     CYP1A2_AS = NA,
                     CYP2D6_MP = NA,
                     CYP2D6_AS = NA,
                     CYP3A4_MP = NA,
                     CYP3A4_AS = NA
  )

  test$weight[test$sex == "Male"] <-  sample(60:110, length(test$weight[test$sex == "Male"]), replace = TRUE)
  test$weight[test$sex == "Female"] <-  sample(55:85, length(test$weight[test$sex == "Female"]), replace = TRUE)


  dd <- c(100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400, 425, 450, 475, 500, 525, 550, 575, 600, 625, 650, 675, 700, 725, 750, 775, 800, 825, 850, 875, 900, 925, 950, 975, 1000)
  test$daily_dose <-  sample(dd, 100, replace = TRUE, prob = c(0.0118, 0.0127, 0.0166, 0.0254, 0.05, 0.047, 0.077, 0.043, 0.169, 0.041, 0.089, 0.028, 0.105, 0.024, 0.055, 0.012, 0.057, 0.01, 0.021, 0.006, 0.029, 0.004, 0.016, 0.005, 0.0166, 0.0017, 0.0057, 0.0004, 0.006, 0.0017, 0.004, 0.0004, 0.003, 0, 0, 0.004, 0.0003))

  as <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  test$CYP1A2_AS[test$daily_dose >= 500] <-  sample(as, length(test$CYP1A2_AS[test$daily_dose >= 500]), replace = TRUE, prob = c(0.02, 0.03, 0.05, 0.10, 0.15, 0.25, 0.40))
  test$CYP1A2_AS[test$daily_dose < 500] <-  sample(as, length(test$CYP1A2_AS[test$daily_dose < 500]), replace = TRUE, prob = c(0.05, 0.07, 0.10, 0.13, 0.18, 0.20, 0.27))
  test$CYP1A2_MP[test$CYP1A2_AS >= 2] <- "Rapid Metaboliser"
  test$CYP1A2_MP[test$CYP1A2_AS >= 3] <- "Ultra-rapid Metaboliser"
  test$CYP1A2_MP[test$CYP1A2_AS < 2] <- "Normal Metaboliser"
  test$CYP1A2_MP[test$CYP1A2_AS < 1] <- "Intermediate Metaboliser"
  test$CYP1A2_MP[test$CYP1A2_AS == 0] <- "Poor Metaboliser"


  as_a <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  test$CYP2D6_AS[test$daily_dose >= 650] <-  sample(as, length(test$CYP2D6_AS[test$daily_dose >= 650]), replace = TRUE, prob = c(0.02, 0.03, 0.05, 0.25, 0.15, 0.25, 0.15))
  test$CYP2D6_AS[test$daily_dose < 650] <-  sample(as, length(test$CYP2D6_AS[test$daily_dose < 650]), replace = TRUE, prob = c(0.05, 0.07, 0.15, 0.28, 0.23, 0.15, 0.07))
  test$CYP2D6_MP[test$CYP2D6_AS > 2] <- "Rapid Metaboliser"
  test$CYP2D6_MP[test$CYP2D6_AS >= 3] <- "Ultra-rapid Metaboliser"
  test$CYP2D6_MP[test$CYP2D6_AS <= 2] <- "Normal Metaboliser"
  test$CYP2D6_MP[test$CYP2D6_AS < 1] <- "Intermediate Metaboliser"
  test$CYP2D6_MP[test$CYP2D6_AS == 0] <- "Poor Metaboliser"

  as_b <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  test$CYP3A4_AS[test$daily_dose >= 350] <-  sample(as, length(test$CYP3A4_AS[test$daily_dose >= 350]), replace = TRUE, prob = c(0.01, 0.015, 0.05, 0.90, 0.015, 0.01, 0.01))
  test$CYP3A4_AS[test$daily_dose < 350] <-  sample(as, length(test$CYP3A4_AS[test$daily_dose < 350]), replace = TRUE, prob = c(0.01, 0.015, 0.05, 0.90, 0.015, 0.01, 0.01))
  test$CYP3A4_MP[test$CYP3A4_AS >= 3] <- "Ultra-rapid Metaboliser"
  test$CYP3A4_MP[test$CYP3A4_AS <= 2] <- "Normal Metaboliser"
  test$CYP3A4_MP[test$CYP3A4_AS < 1] <- "Intermediate Metaboliser"
  test$CYP3A4_MP[test$CYP3A4_AS == 0] <- "Poor Metaboliser"

  meds <- c(1:5)
  test$other_meds1 <- sample(meds, 100, replace = TRUE, prob = c(0.5, 0.17, 0.15, 0.13, 0.05))
  test$other_meds1[test$other_meds1 == 1] <- NA
  test$other_meds1[test$other_meds1 == 3] <- "Fluoxetine"
  test$other_meds1[test$other_meds1 == 4] <- "Fluvoxamine"
  test$other_meds1[test$other_meds1 == 5] <- "Cigarettes"
  test$other_meds1[test$other_meds1 == 2] <- "Quinidine"

  test$other_meds2 <- sample(meds, 100, replace = TRUE, prob = c(0.5, 0.17, 0.15, 0.13, 0.05))
  test$other_meds2[test$other_meds2 == 1] <- NA
  test$other_meds2[test$other_meds2 == 3] <- "Carbamazepine"
  test$other_meds2[test$other_meds2 == 4] <- "Oral contraceptives"
  test$other_meds2[test$other_meds2 == 5] <- "Rifampin"
  test$other_meds2[test$other_meds2 == 2] <- "Cigarettes"

  test$other_meds3 <- sample(meds, 100, replace = TRUE, prob = c(0.5, 0.17, 0.15, 0.13, 0.05))
  test$other_meds3[test$other_meds3 == 1] <- NA
  test$other_meds3[test$other_meds3 == 3] <- "Carbamazepine"
  test$other_meds3[test$other_meds3 == 4] <- "Phenytoin"
  test$other_meds3[test$other_meds3 == 5] <- "Rifampin"
  test$other_meds3[test$other_meds3 == 2] <- "Fluvoxamine"


  return(test)
}
