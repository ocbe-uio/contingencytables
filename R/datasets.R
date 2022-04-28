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
#' @docType data
#' @name table_7.3
#' @references Fagerland MW, Lydersen S, Laake P (2017)
#' @export
table_7.3 <- rbind(c(40, 25), c(54, 7), c(63, 10))

#' Psychiatric diag. vs BMI with hyperkinetic disorders as reference category
#' @docType data
#' @name table_7.5
#' @references Fagerland MW, Lydersen S, Laake P (2017)
#' @export
table_7.5 <- matrix(
  c(3, 55, 23, 8, 102, 36, 6, 14, 1, 5, 21, 12, 19, 130, 64, 7, 26, 18),
  ncol = 3, byrow = TRUE
)

#' LBW vs psych. morbidity with control as reference category
#' @docType data
#' @name table_7.6
#' @references Fagerland MW, Lydersen S, Laake P (2017)
#' @export
table_7.6 <- matrix(c(22, 4, 12, 24, 9, 10, 51, 7, 6), ncol = 3, byrow = TRUE)
