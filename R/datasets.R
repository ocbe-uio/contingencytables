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
#' externa (Van Balen et al., 2003).
#'
#' Van Balen et al. (2003) report a randomized, double-blind, controlled trial
#' comparing three treatments for an ear infection. The numbers and proportions
#' of patients reported cured and not cured after 21 days of treatment are
#' summarized in Table 7.3. Because there is no ordering between the treatments,
#' we regard Table 7.3 as an unordered 3 × 2 table.
#' @docType data
#' @name table_7.3
#' @aliases vanbalen_2003
#' @usage table_7.3
#' vanbalen_2003
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
vanbalen_2003 <- table_7.3

#' Psychiatric Diagnoses and Physical Activity
#' @docType data
#' @description Psychiatric diagnoses and participation in team
#' sports (Mangerud et al., 2014)
#'
#' Table 7.4 shows the number of subjects participating in team sports within
#' each of six psychiatric diagnoses, based on data from a study of physical
#' activity in adolescents aged 13 to 18 years who were referred to a child and
#' adolescent psychiatric clinic from 2009 to 2001 (Mangerud et al., 2014). The
#' psychiatric diagnoses are unordered, and we shall treat this as an unordered
#' 6 x 2 table
#' @name table_7.4
#' @aliases mangerud_2014_PA
#' @usage table_7.4
#' mangerud_2014_PA
#' @references
#' Fagerland MW, Lydersen S, Laake P (2017)
#'
#'
#' @export
table_7.4 <- matrix(
  data = c(62, 21, 97, 48, 10, 12, 30, 7, 132, 78, 34, 17),
  ncol = 2,
  byrow = TRUE,
  dimnames = list(
    c(
      "Mood (affective) disorders", "Anxiety disorders", "Eating disorders",
      "Autism spectrum disorders", "Hyperkinetic disorders", "Other disorders"
      ),
    c("No", "Yes")
  )
)
mangerud_2014_PA <- table_7.4

#' Psychiatric diag. vs BMI with hyperkinetic disorders as reference category
#' @docType data
#' @description Psychiatric diagnoses and weight categories based on age- and
#' sex-adjusted BMI (Mangerud et al., 2014).
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
#' @aliases mangerud_2014_BMI
#' @usage table_7.5
#' mangerud_2014_BMI
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
mangerud_2014_BMI <- table_7.5

#' Low Birth Weight vs psychiatric morbitidy with control as reference category
#' @docType data
#' @description Categories of birth weight and psychiatric problems at age 20
#' years (Lund et al., 2012).
#'
#' Lund et al. (2012) report psychiatric morbidity in young adulthood in two low
#' birth weight groups and a control group. The subjects were born between 1986
#' and 1988. The very low birth weight (VLBW) group consisted of babies born
#' preterm with birth weight up to 1500 grams. The small for gestational age at
#' term (SGA) group was born at term with birth weight below the 10th percentile
#' adjusted for gestational age, sex, and parity. The control group was born at
#' term, and was not small for gestational age. Table 7.6 shows the severity
#' level of psychiatric problems at age 20 years. We shall regard the birth
#' groups as unordered; however, the diagnostic groups are naturally ordered.
#' Hence, Table 7.6 is a singly ordered 3 × 3 table with unordered rows and
#' ordered columns.
#' @name table_7.6
#' @aliases lund_2012
#' @usage table_7.6
#' lund_2012
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
lund_2012 <- table_7.6

#' Colorectal cancer (Table 7.7)
#' @description Duration of symptoms and tumor stage for patients treated for
#' colorectal cancer (Jullumstroe et al., 2009).
#'
#' Early detection and treatment of colorectal cancer is beneficial, because
#' advanced stages of colorectal cancer have poorer prognosis. Table 7.7
#' displays duration of symptoms (rows) versus tumor stage (columns) in a study
#' of 784 patients treated for colorectal cancer at a regional hospital in
#' Norway from 1980 to 2004 (Jullumstroe et al., 2009). The rows as well as the
#' columns are ordered, and Table 7.7 can be regarded as a doubly ordered 4 × 4
#' table.
#' @docType data
#' @name table_7.7
#' @aliases jullumstroe_2009
#' @usage table_7.7
#' jullumstroe_2009
#' @references
#' Fagerland MW, Lydersen S, Laake P (2017)
#'
#' Jullumstroe et al. (2009)
#' @export
table_7.7 <- matrix(
  data = c(2, 4, 29, 19, 7, 6, 116, 51, 19, 27, 201, 76, 18, 22, 133, 54),
  ncol = 4,
  byrow = TRUE,
  dimnames = list(
    c("< 1 week", "2-8 weeks", "2-6 months", "> 6 months"),
    c("T-1", "T-2", "T-3", "T-4")
  )
)
jullumstroe_2009 <- table_7.7

#' Breast Tumor
#' @description Nuclear pleomorphism from fine needle aspiration smears and
#' breast tumor type (Bofin et al., 2004).
#'
#' Bofin et al. (2004) studied associations between different findings in fine
#' needle aspiration (FNA) smears from breast tumors and the final histological
#' diagnosis of tumor type in 133 patients. The aim of the study was to identify
#' variables developed from FNA smears that could differentiate between
#' the different tumor diagnoses. Table 7.8 presents the cross-classification of
#' the FNA variable nuclear pleomorphism with tumor types. Both variables can be
#' considered as ordered, with tumor type ordered from benign (as in NPBD) to
#' most malign (as in IDC).
#' @docType data
#' @name table_7.8
#' @aliases bofin_2004
#' @usage table_7.8
#' bofin_2004
#' @references
#' Fagerland MW, Lydersen S, Laake P (2017)
#'
#' Bofin et al. (2004)
#' @export
table_7.8 <- matrix(
  data = c(15, 35, 6, 9, 6, 2, 4, 2, 11, 11, 0, 0, 1, 10, 21),
  ncol = 5,
  byrow = TRUE,
  dimnames = list(
    c("None/mild", "Moderate", "Profound"),
    c("NPBD", "PBD", "AIDH", "DCIS", "IDC")
  )
)
bofin_2004 <- table_7.8

#' Self-rated health (Table 7.9)
#' @description Self-rated health for 12 to 17 years old adolescents in
#' Young-HUNT 1 and four years later in Young-HUNT 2 (Breidablik et al., 2008).
#'
#' In the HUNT study (Nord-Trøndelag county health survey), one of the questions
#' is: “How is your overall health at the moment?” The outcome categories
#' are “Very good”, “Good”, “Not very good”, and “Poor”. Table 7.9 shows
#' the counts for the adolescents aged 12 to 17 years in 1995 to 1997
#' (Young-HUNT 1), and for the same individuals four years later (Young-HUNT 2;
#' Breidablik et al. (2008)). Both the rows and the columns are ordered. In
#' this example, it may be appropriate to regard self-rated health as an
#' unobserved (latent) continuous variable, where only a categorized version has
#' been observed. Table 7.9 is actually an example of a paired c × c table with
#' ordinal data.
#' @docType data
#' @name table_7.9
#' @aliases breidablik_2008
#' @usage table_7.9
#' breidablik_2008
#' @references
#' Fagerland MW, Lydersen S, Laake P (2017)
#'
#' Breidablik et al. (2008)
#' @export
table_7.9 <- matrix(
  data = c(2, 3, 3, 3, 2, 58, 98, 14, 8, 162, 949, 252, 4, 48, 373, 369),
  ncol = 4,
  byrow = TRUE,
  dimnames = list(
    c("Poor", "Not very good", "Good", "Very good"),
    c("Poor", "Not very good", "Good", "Very good")
  )
)
breidablik_2008 <- table_7.9

#' Genotype counts for SNP rs 6498169 in RA patients
#' @name snp6498169
#' @docType data
#' @export
snp6498169 <- list(
  complete = data.frame(n = c(276, 380, 118), pi0 = c(0.402, 0.479, 0.119)),
  subset   = data.frame(n = c(6, 1, 3), pi0 = c(0.402, 0.479, 0.119))
)

#' Table 13.6, page 382, of Fleiss et al. (2003)
#' @name fleiss_2003
#' @docType data
#' @references Fleiss et al. (2003)
#' @export
fleiss_2003 <- rbind(c(35, 5, 0), c(15, 20, 5), c(10, 5, 5))

#' Floppy eyelid syndrome vs obstructive sleep apnea
#' @name ezra_2010
#' @docType data
#' @references Ezra et al. (2010)
#' @export
ezra_2010 <- rbind(c(7, 25), c(2, 68))


#' A comparison between serial and retrospective measurements
#' @name fischer_1999
#' @docType data
#' @references Fischer et al. (1999)
#' @export
fischer_1999 <- rbind(
  c(1, 0, 1, 0, 0),
  c(0, 2, 8, 4, 4),
  c(1, 1, 31, 14, 11),
  c(1, 0, 15, 9, 12),
  c(0, 0, 2, 1, 3)
)
