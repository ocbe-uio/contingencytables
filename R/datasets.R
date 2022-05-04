#' A case-control study of GADA exposure on IPEX syndrome
#' @name lampasona_2013
#' @docType data
#' @references Lampasona et al. (2013)
#' @export
lampasona_2013 <- matrix(c(9, 4, 4, 10), nrow = 2, byrow = TRUE)

#' The association between CHRNA4 genotype and XFS
#' @name ritland_2007
#' @docType data
#' @references Ritland et al. (2007)
#' @export
ritland_2007 <- matrix(c(0, 16, 15, 57), nrow = 2, byrow = TRUE)

#' An RCT of high vs standard dose of epinephrine
#' @name perondi_2004
#' @docType data
#' @references Perondi et al. (2004)
#' @export
perondi_2004 <- matrix(c(7, 27, 1, 33), nrow = 2, byrow = TRUE)

#' The number of n-th order male births
#' @name singh_2010_1
#' @docType data
#' @references Singh et al. (2010)
#' @export
singh_2010 <- data.frame(
  "X" = c(250, 204, 103, 33),
  "n" = c(533, 412, 167, 45),
  row.names = c("1st", "2nd", "3rd", "4th")
)

#' Ligarden et al., 2010
#' @name ligarden_2010
#' @docType data
#' @references ligarden_2010
#' @export
ligarden_2010 <- c("X" = 13, "n" = 16)

#' A lady tasting a cup of tea
#' @docType data
#' @name tea
#' @export
tea <- rbind(c(3, 1), c(1, 3))

#' Pretherapy susceptability of pathogens
#' @docType data
#' @name peterson_2007
#' @references Peterson et al. (2007)
#' @export
peterson_2007 <- rbind(
  c(596, 18, 6, 5),
  c(0, 2, 0, 0),
  c(0, 0, 42, 0),
  c(11, 0, 0, 0)
)

#' Airway hyper-responsiveness before and after stem cell transplantation
#' @docType data
#' @name bentur_2009
#' @references Bentur et al. (2009)
#' @export
bentur_2009 <- rbind(c(1, 1), c(7, 12))

#' Complete response before and after consolidation therapy
#' @docType data
#' @name cavo_2012
#' @references Cavo et al. (2012)
#' @export
cavo_2012 <- rbind(c(59, 6), c(16, 80))

#' The Adolescent Placement Study
#' @docType data
#' @name fontanella_2008
#' @references Fontanella et al. (2008)
#' @export
fontanella_2008 <- rbind(c(8, 28, 72, 126), c(46, 73, 69, 86))

#' Postoperative nausea
#' @docType data
#' @name lydersen_2012a
#' @references Lydersen et al. (2012a)
#' @export
lydersen_2012a <- rbind(c(14, 10, 3, 2), c(11, 7, 8, 4))

#' Smoking and lung cancer
#' @docType data
#' @name doll_hill_1950
#' @references Doll and Hill (1950)
#' @export
doll_hill_1950 <- array(dim = c(2, 2, 2))
doll_hill_1950[, , 1] <- matrix(c(647, 622, 2, 27), 2, byrow = TRUE)
doll_hill_1950[, , 2] <- matrix(c(41, 28, 19, 32), 2, byrow = TRUE)

#' Prophylactice use of Lidocaine in myocardial infarction
#' @docType data
#' @name hine_1989
#' @references Hine et al. (1989)
#' @export
hine_1989 <- array(dim = c(2, 2, 6))
hine_1989[, , 1] <- rbind(c(2, 37), c(1, 42))
hine_1989[, , 2] <- rbind(c(4, 40), c(4, 40))
hine_1989[, , 3] <- rbind(c(6, 101), c(4, 106))
hine_1989[, , 4] <- rbind(c(7, 96), c(5, 95))
hine_1989[, , 5] <- rbind(c(7, 103), c(3, 103))
hine_1989[, , 6] <- rbind(c(11, 143), c(4, 142))

#' Hypothetical experiment
#' @docType data
#' @name hypothetical
#' @export
hypothetical <- c(1, 4, 3, 11, 9)

#' Alcohol consumption and malformations
#' @docType data
#' @name mills_graubard_1987
#' @references Mills and Graubard (1987)
#' @export
mills_graubard_1987 <- rbind(
  c(48, 17066), c(38, 14464), c(5, 788), c(1, 126), c(1, 37)
)

#' Elevated troponin T levels in stroke patients
#' @docType data
#' @name indredavik_2008
#' @references Indredavik et al. (2008)
#' @export
indredavik_2008 <- rbind(c(8, 53), c(10, 48), c(11, 100), c(22, 102), c(6, 129))

#' Treatment for ear infection
#' @description Status after 21 days treatment of the ear infection acute otitis
#' externa.
#'
#' Van Balen et al. (2003) report a randomized, double-blind, controlled trial
#' comparing three treatments for an ear infection. The numbers and proportions
#' of patients reported cured and not cured after 21 days of treatment are
#' summarized in Table 7.3. Because there is no ordering between the treatments,
#' we regard Table 7.3 as an unordered 3 × 2 table.
#' @docType data
#' @name table_7.3
#' @references
#' Fagerland MW, Lydersen S, Laake P (2017)
#'
#' Van Balen et al. (2003)
#' @export
table_7.3 <- matrix(
  data = c(40, 25, 54, 7, 63, 10),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    c(
      "Acetic acid",
      "Corticosteroid and acetic acid",
      "Corticosteroid and antibiotic"
    ),
    c("Cured", "Not cured")
  )
)

#' Psychiatric diag. vs BMI with hyperkinetic disorders as reference category
#' @docType data
#' @description Psychiatric diagnoses and weight categories based on age- and
#' sex-adjusted BMI.
#'
#' Table 7.5 shows the number of thin, normal weight, and overweight subjects
#' within each of six psychiatric diagnoses, based on the same study as in
#' Section 7.2.2 (Mangerud et al., 2014). Body mass index (BMI) is calculated
#' as the weight in kg divided by the squared height in meters. In subjects
#' aged 18 years or older, the cut-off points for being categorized as thin,
#' normal weight, and overweight are BMI less than 18.5, BMI between 18.5 and
#' 25, and BMI above 25, respectively. For younger subjects (below 18 years of
#' age), the categorization was done following internationally adopted cut-off
#' points for age and sex (Cole et al., 2000, 2007). For example, the cut-off
#' point for being overweight at age 13 is 21.91 for males and 22.58 for
#' females.
#' @name table_7.5
#' @references
#' Fagerland MW, Lydersen S, Laake P (2017)
#'
#' Mangerud et al. (2014)
#' @export
table_7.5 <- matrix(
  data = c(3, 55, 23, 8, 102, 36, 6, 14, 1, 5, 21, 12, 19, 130, 64, 7, 26, 18),
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c(
      "Mood (affective) dis.",
      "Anxiety disorders",
      "Eating disorders",
      "Autism spectrum dis.",
      "Hyperkinetic dis.",
      "Other disorders"
    ),
    c("Thin", "Normal", "Overweight")
  )
)

#' Low Birth Weight vs psychiatric morbitidy with control as reference category
#' @docType data
#' @description Categories of birth weight and psychiatric problems at age 20
#' years (Lund et al., 2012).
#'
#' Lund et al. (2012) report psychiatric morbidity in young adulthood in two low
#' birth weight groups and a control group. The subjects were born between 1986
#' and 1988. The very low birth weight (VLBW) group consisted of babies born
#' preterm with birth weight ≤ 1500 grams. The small for gestational age at
#' term (SGA) group was born at term with birth weight below the 10th percentile
#' adjusted for gestational age, sex, and parity. The control group was born at
#' term, and was not small for gestational age. Table 7.6 shows the severity
#' level of psychiatric problems at age 20 years. We shall regard the birth
#' groups as unordered; however, the diagnostic groups are naturally ordered.
#' Hence, Table 7.6 is a singly ordered 3 × 3 table with unordered rows and
#' ordered columns.
#' @name table_7.6
#' @references
#' Fagerland MW, Lydersen S, Laake P (2017)
#'
#' Lund et al. (2012)
#' @export
table_7.6 <- matrix(
  data = c(22, 4, 12, 24, 9, 10, 51, 7, 6),
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c("VLBW", "SGA", "Control"),
    c("No diagnosis", "Subthreshold diagnosis", "Definite diagnosis")
  )
)
