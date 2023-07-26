---
title: "PEBRA Complier Average Causal Effect Analyses"
author: "A.Amstutz & N.Tschumi"
date: "2023-05-07"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    code_folding: hide
  pdf_document:
    toc: yes
  word_document:
    toc: yes
---

# Load packages



# Load Data


# Load functions



# Data management
### Preference assessments (intervention peer-educator data only)





### Baseline preference assessment data only



### Join to main dataset with all peer-educators and participants
### Sociodemographic characteristics of all participants



### TABLE Baseline characteristics by group: socio-demographics


```r
# table for sociodemographic characteristics
vars.list <- c("ARM","GENDER","AGE","CELL_GIVEN","SEX_ORIENT","currently_attending","no_schooling","N_school","emplyoment","occupation","profession","Maritalstatus","pregnant_breastfeeding","Howmany","using_fp","fp_condom","fp_pill","fp_inject","fp_withdraw","fp_calendar","fp_other","fp_no_answer","N_HIV_question","expenses_transport_yn","expenses_transport_cost","expenses_food_yn","expenses_food_cost")

df_sociodemo <- df[,colnames(df)%in%vars.list]
df_sociodemo <- df_sociodemo[,match(vars.list,colnames(df_sociodemo))]
colnames(df_sociodemo) <- vars.list <- c("ARM","Gender","Age at enrolment","Cell phone to receive confidential information","Sexual orientation","Currently attending school","No schooling","Number of completed school years","Employment","Occupation","Profession (if (self-)employed)","Marital status","Pregnant  or breastfeeding","Number of children","Contraception use","Contraception: Condom use (male and female)","Contraception: Contraceptive pill","Contraception: Injectable or Implant (e.g. DEPO)","Contraception: Withdraw","Contraception: Calendar method","Contraception: other","Contraception: no answer","Number of correctly answered HIV knowledge questions (maximum 10)","Expenses: transport","Expenses: transport costs","Expenses: food","Expenses: food costs")

table_sociodemo <- tableone::CreateTableOne(data = df_sociodemo,vars = vars.list[!vars.list == "ARM"],strata = "ARM",includeNA = TRUE,test = FALSE,addOverall = TRUE)

capture.output(table_sociodemo <- print(table_sociodemo, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = FALSE,printToggle = FALSE,missing = TRUE))
```

```
## character(0)
```

```r
#print
knitr::kable(table_sociodemo,caption = "Baseline characteristics: socio-demographic, missing values in categorical variables: NAs")
```



Table: Baseline characteristics: socio-demographic, missing values in categorical variables: NAs

|                                                                                 |level                             |Overall              |control              |interv.              |Missing |
|:--------------------------------------------------------------------------------|:---------------------------------|:--------------------|:--------------------|:--------------------|:-------|
|n                                                                                |                                  |307                  |157                  |150                  |        |
|Gender (%)                                                                       |female                            |218 (71.0)           |119 (75.8)           |99 (66.0)            |0.0     |
|                                                                                 |male                              |89 (29.0)            |38 (24.2)            |51 (34.0)            |        |
|Age at enrolment (median [IQR])                                                  |                                  |19.41 [16.94, 22.44] |20.12 [17.03, 22.94] |18.72 [16.81, 22.07] |0.0     |
|Cell phone to receive confidential information (%)                               |No                                |106 (34.5)           |52 (33.1)            |54 (36.0)            |0.0     |
|                                                                                 |Yes                               |201 (65.5)           |105 (66.9)           |96 (64.0)            |        |
|Sexual orientation (%)                                                           |gay or lesbian                    |1 ( 0.3)             |0 ( 0.0)             |1 ( 0.7)             |0.0     |
|                                                                                 |prefer not to answer              |2 ( 0.7)             |1 ( 0.6)             |1 ( 0.7)             |        |
|                                                                                 |straight or heterosexual          |304 (99.0)           |156 (99.4)           |148 (98.7)           |        |
|Currently attending school (%)                                                   |No                                |212 (69.1)           |121 (77.1)           |91 (60.7)            |0.0     |
|                                                                                 |Yes                               |95 (30.9)            |36 (22.9)            |59 (39.3)            |        |
|No schooling (%)                                                                 |No                                |302 (98.4)           |153 (97.5)           |149 (99.3)           |0.0     |
|                                                                                 |Yes                               |5 ( 1.6)             |4 ( 2.5)             |1 ( 0.7)             |        |
|Number of completed school years (median [IQR])                                  |                                  |9.00 [7.00, 10.50]   |9.00 [7.00, 11.00]   |9.00 [7.25, 10.00]   |0.0     |
|Employment (%)                                                                   |Employed in Lesotho               |15 ( 4.9)            |7 ( 4.5)             |8 ( 5.3)             |0.0     |
|                                                                                 |Employed in RSA                   |2 ( 0.7)             |1 ( 0.6)             |1 ( 0.7)             |        |
|                                                                                 |Housewife                         |43 (14.0)            |25 (15.9)            |18 (12.0)            |        |
|                                                                                 |No regular income / unemployed    |231 (75.2)           |116 (73.9)           |115 (76.7)           |        |
|                                                                                 |Self-employed with regular income |5 ( 1.6)             |1 ( 0.6)             |4 ( 2.7)             |        |
|                                                                                 |Subsistence farming               |11 ( 3.6)            |7 ( 4.5)             |4 ( 2.7)             |        |
|Occupation (%)                                                                   |(self-)employed                   |22 ( 7.2)            |9 ( 5.7)             |13 ( 8.7)            |0.0     |
|                                                                                 |attending school                  |93 (30.3)            |36 (22.9)            |57 (38.0)            |        |
|                                                                                 |nothing                           |192 (62.5)           |112 (71.3)           |80 (53.3)            |        |
|Profession (if (self-)employed) (%)                                              |Business man/woman                |3 ( 1.0)             |2 ( 1.3)             |1 ( 0.7)             |92.8    |
|                                                                                 |Domestic worker                   |4 ( 1.3)             |3 ( 1.9)             |1 ( 0.7)             |        |
|                                                                                 |Herdboy                           |3 ( 1.0)             |0 ( 0.0)             |3 ( 2.0)             |        |
|                                                                                 |Other                             |12 ( 3.9)            |4 ( 2.5)             |8 ( 5.3)             |        |
|                                                                                 |NA                                |285 (92.8)           |148 (94.3)           |137 (91.3)           |        |
|Marital status (%)                                                               |div/sep/wid                       |8 ( 2.6)             |4 ( 2.5)             |4 ( 2.7)             |0.0     |
|                                                                                 |married                           |93 (30.3)            |54 (34.4)            |39 (26.0)            |        |
|                                                                                 |single                            |206 (67.1)           |99 (63.1)            |107 (71.3)           |        |
|Pregnant  or breastfeeding (%)                                                   |No                                |184 (59.9)           |104 (66.2)           |80 (53.3)            |29.0    |
|                                                                                 |Yes                               |34 (11.1)            |15 ( 9.6)            |19 (12.7)            |        |
|                                                                                 |NA                                |89 (29.0)            |38 (24.2)            |51 (34.0)            |        |
|Number of children (%)                                                           |0                                 |200 (65.1)           |91 (58.0)            |109 (72.7)           |0.0     |
|                                                                                 |1                                 |81 (26.4)            |46 (29.3)            |35 (23.3)            |        |
|                                                                                 |2 or 3                            |26 ( 8.5)            |20 (12.7)            |6 ( 4.0)             |        |
|Contraception use (%)                                                            |I prefer not to answer            |23 ( 7.5)            |12 ( 7.6)            |11 ( 7.3)            |2.3     |
|                                                                                 |No                                |86 (28.0)            |18 (11.5)            |68 (45.3)            |        |
|                                                                                 |Not currently sexually active     |57 (18.6)            |38 (24.2)            |19 (12.7)            |        |
|                                                                                 |Yes                               |134 (43.6)           |85 (54.1)            |49 (32.7)            |        |
|                                                                                 |NA                                |7 ( 2.3)             |4 ( 2.5)             |3 ( 2.0)             |        |
|Contraception: Condom use (male and female) (%)                                  |No                                |43 (14.0)            |23 (14.6)            |20 (13.3)            |56.4    |
|                                                                                 |Yes                               |91 (29.6)            |62 (39.5)            |29 (19.3)            |        |
|                                                                                 |NA                                |173 (56.4)           |72 (45.9)            |101 (67.3)           |        |
|Contraception: Contraceptive pill (%)                                            |No                                |116 (37.8)           |74 (47.1)            |42 (28.0)            |56.4    |
|                                                                                 |Yes                               |18 ( 5.9)            |11 ( 7.0)            |7 ( 4.7)             |        |
|                                                                                 |NA                                |173 (56.4)           |72 (45.9)            |101 (67.3)           |        |
|Contraception: Injectable or Implant (e.g. DEPO) (%)                             |No                                |92 (30.0)            |61 (38.9)            |31 (20.7)            |56.4    |
|                                                                                 |Yes                               |42 (13.7)            |24 (15.3)            |18 (12.0)            |        |
|                                                                                 |NA                                |173 (56.4)           |72 (45.9)            |101 (67.3)           |        |
|Contraception: Withdraw (%)                                                      |No                                |130 (42.3)           |82 (52.2)            |48 (32.0)            |56.4    |
|                                                                                 |Yes                               |4 ( 1.3)             |3 ( 1.9)             |1 ( 0.7)             |        |
|                                                                                 |NA                                |173 (56.4)           |72 (45.9)            |101 (67.3)           |        |
|Contraception: Calendar method (%)                                               |No                                |131 (42.7)           |82 (52.2)            |49 (32.7)            |56.4    |
|                                                                                 |Yes                               |3 ( 1.0)             |3 ( 1.9)             |0 ( 0.0)             |        |
|                                                                                 |NA                                |173 (56.4)           |72 (45.9)            |101 (67.3)           |        |
|Contraception: other (%)                                                         |No                                |134 (43.6)           |85 (54.1)            |49 (32.7)            |56.4    |
|                                                                                 |NA                                |173 (56.4)           |72 (45.9)            |101 (67.3)           |        |
|Contraception: no answer (%)                                                     |No                                |133 (43.3)           |84 (53.5)            |49 (32.7)            |56.4    |
|                                                                                 |Yes                               |1 ( 0.3)             |1 ( 0.6)             |0 ( 0.0)             |        |
|                                                                                 |NA                                |173 (56.4)           |72 (45.9)            |101 (67.3)           |        |
|Number of correctly answered HIV knowledge questions (maximum 10) (median [IQR]) |                                  |9.00 [9.00, 10.00]   |9.00 [9.00, 10.00]   |9.00 [9.00, 10.00]   |0.0     |
|Expenses: transport (%)                                                          |no                                |204 (66.4)           |104 (66.2)           |100 (66.7)           |0.0     |
|                                                                                 |yes                               |103 (33.6)           |53 (33.8)            |50 (33.3)            |        |
|Expenses: transport costs (median [IQR])                                         |                                  |17.00 [10.00, 30.00] |16.00 [8.00, 35.00]  |20.00 [10.00, 30.00] |66.4    |
|Expenses: food (%)                                                               |no                                |260 (84.7)           |130 (82.8)           |130 (86.7)           |0.0     |
|                                                                                 |yes                               |47 (15.3)            |27 (17.2)            |20 (13.3)            |        |
|Expenses: food costs (median [IQR])                                              |                                  |10.00 [6.00, 20.00]  |10.00 [8.00, 20.00]  |10.00 [5.00, 21.25]  |84.7    |

### Clinical characteristics of all participants



### TABLE Clinical characteristics by group: socio-demographics


```r
# table for clinical characteristics
vars.list <- c("diagnosis_year_cat","time_diag_enrol","artstart_year_cat",
                   "time_artstart_enrol","infection_year_cat","time_infection_enrol",
                   "CurrentARTregimen","Currently_TBx","cd4_start_cat",
                   "baseline_Vl_cat","infection_way","ARM")

df_clinical <- df[,colnames(df)%in%vars.list]
df_clinical <- df_clinical[,match(vars.list,colnames(df_clinical))]

colnames(df_clinical) <- vars.list <- c("Year of HIV diagnosis","Years since HIV diagnosis","Year of starting ART","Years since starting ART","Year of HIV infection","Years since HIV infection","Current ART regimen","Currently on TB treatment","CD4 count at ART start", "Baseline viral load","How do you believe you were infected with HIV?","ARM")

table_clinical <- tableone::CreateTableOne(data = df_clinical,vars = vars.list[!vars.list == "ARM"],strata = "ARM",includeNA = TRUE,test = FALSE,addOverall = TRUE)

capture.output(table_clinical <- print(table_clinical, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = FALSE,printToggle = FALSE,missing = TRUE))
```

```
## character(0)
```

```r
#print
knitr::kable(table_clinical,caption = "Baseline characteristics: clinical, missing values in categorical variables: NAs")
```



Table: Baseline characteristics: clinical, missing values in categorical variables: NAs

|                                                   |level                  |Overall            |control            |interv.            |Missing |
|:--------------------------------------------------|:----------------------|:------------------|:------------------|:------------------|:-------|
|n                                                  |                       |307                |157                |150                |        |
|Year of HIV diagnosis (%)                          |2005-2009              |74 (24.1)          |29 (18.5)          |45 (30.0)          |0.0     |
|                                                   |2010-2014              |69 (22.5)          |30 (19.1)          |39 (26.0)          |        |
|                                                   |2015-2020              |164 (53.4)         |98 (62.4)          |66 (44.0)          |        |
|Years since HIV diagnosis (median [IQR])           |                       |4.52 [1.86, 9.74]  |3.63 [1.52, 7.87]  |5.45 [2.91, 10.99] |0.0     |
|Year of starting ART (%)                           |2005-2009              |56 (18.2)          |21 (13.4)          |35 (23.3)          |0.0     |
|                                                   |2010-2014              |66 (21.5)          |27 (17.2)          |39 (26.0)          |        |
|                                                   |2015-2020              |185 (60.3)         |109 (69.4)         |76 (50.7)          |        |
|Years since starting ART (median [IQR])            |                       |3.65 [1.65, 8.39]  |3.14 [1.21, 5.82]  |4.90 [2.67, 9.35]  |0.0     |
|Year of HIV infection (%)                          |1995-1999              |10 ( 3.3)          |6 ( 3.8)           |4 ( 2.7)           |0.0     |
|                                                   |2000-2004              |49 (16.0)          |26 (16.6)          |23 (15.3)          |        |
|                                                   |2005-2009              |57 (18.6)          |18 (11.5)          |39 (26.0)          |        |
|                                                   |2010-2014              |48 (15.6)          |19 (12.1)          |29 (19.3)          |        |
|                                                   |2015-2020              |143 (46.6)         |88 (56.1)          |55 (36.7)          |        |
|Years since HIV infection (median [IQR])           |                       |5.93 [2.92, 12.90] |4.93 [2.89, 12.87] |8.43 [3.88, 12.93] |0.0     |
|Current ART regimen (%)                            |DTG-based              |40 (13.0)          |26 (16.6)          |14 ( 9.3)          |0.0     |
|                                                   |EFV-based              |212 (69.1)         |104 (66.2)         |108 (72.0)         |        |
|                                                   |LPV/r-based            |8 ( 2.6)           |5 ( 3.2)           |3 ( 2.0)           |        |
|                                                   |NVP-based              |47 (15.3)          |22 (14.0)          |25 (16.7)          |        |
|Currently on TB treatment (%)                      |No                     |299 (97.4)         |151 (96.2)         |148 (98.7)         |0.0     |
|                                                   |Yes                    |8 ( 2.6)           |6 ( 3.8)           |2 ( 1.3)           |        |
|CD4 count at ART start (%)                         |<200                   |36 (11.7)          |13 ( 8.3)          |23 (15.3)          |58.3    |
|                                                   |200-499                |53 (17.3)          |32 (20.4)          |21 (14.0)          |        |
|                                                   |>499                   |39 (12.7)          |24 (15.3)          |15 (10.0)          |        |
|                                                   |NA                     |179 (58.3)         |88 (56.1)          |91 (60.7)          |        |
|Baseline viral load (%)                            |<20                    |166 (54.1)         |84 (53.5)          |82 (54.7)          |11.1    |
|                                                   |20-999                 |63 (20.5)          |30 (19.1)          |33 (22.0)          |        |
|                                                   |>999                   |44 (14.3)          |26 (16.6)          |18 (12.0)          |        |
|                                                   |NA                     |34 (11.1)          |17 (10.8)          |17 (11.3)          |        |
|How do you believe you were infected with HIV? (%) |Blood products         |17 ( 5.5)          |11 ( 7.0)          |6 ( 4.0)           |0.0     |
|                                                   |I don't know           |103 (33.6)         |50 (31.8)          |53 (35.3)          |        |
|                                                   |I prefer not to answer |6 ( 2.0)           |5 ( 3.2)           |1 ( 0.7)           |        |
|                                                   |Other                  |7 ( 2.3)           |7 ( 4.5)           |0 ( 0.0)           |        |
|                                                   |Sex with a man         |63 (20.5)          |36 (22.9)          |27 (18.0)          |        |
|                                                   |Sex with a woman       |4 ( 1.3)           |2 ( 1.3)           |2 ( 1.3)           |        |
|                                                   |Through my mother      |107 (34.9)         |46 (29.3)          |61 (40.7)          |        |

# Primary endpoint

The primary outcome of the trial is:

In care with documented viral suppression at 12 months, defined as the proportion of participants in care with a documented VL \<20 copies/mL 12 months (range: 9 -- 15 months) after enrolment out of all participants enrolled

i.  If several viral loads are available in the primary endpoint window, then the result closest to the 12-months endpoint (date of enrolment + 365 days) will be considered.

ii. Rational for VL suppression level at 20 copies/mL: VL determination will be done on COBAS TaqMan HIV-1 Test, v2.0 (Roche Diagnostics) using plasma, and has a reliable lower limit of detection of 20 copies/mL

iii. Definition of "in care": at least one ART visit in the defined window

<!-- -->

I.  Participants who transferred out to any other health facility with known outcome are considered in care (documented proof VL within the primary endpoint window)

II. Participants who died (all-cause), were lost to follow-up (LTFU), or were alive, not taking ART anymore are considered out of care. We define participants lost to follow-up if they or their treatment buddies were more than 2 months late for a scheduled consultation or medication pick-up and no information was found about the participant. We define participants alive, not taking ART anymore if they were more than 2 months late for ART refill with a reason available (e.g. currently no money for clinic-visit, busy working in South Africa, known refusers, known defaulters, etc.)

<!-- -->

iv. The study will use VL results from routine VL monitoring. To synchronize routine VL monitoring and study VL monitoring, study staff will help ensure each site has the capacity to collect these samples and will support existing systems that help to provide results back to sites. VLs will only be performed on individuals who return for visits and no tracking besides standard of care will be performed by the study staff to obtain VLs.


```r
# endpoint definition
df$VL_DATE <- as.Date(df$VL_DATE)
df$VL_RESULT <- as.numeric(df$VL_RESULT)

# endpoint window (9-15)
df$endpoint_l <- as.Date(df$DATE_ENROL) + 252
df$endpoint_u <- as.Date(df$DATE_ENROL) + 450

# definition endpoint
fun_endpoint <- function(VL,VL_date,endpoint_l,endpoint_u,status){
  if (is.na(VL)){return(0)}
  else if (VL >= 20){return(0)}
  else if (status == "out of care"){return(0)}
  else if (xor(VL_date < endpoint_l,VL_date > endpoint_u)){return(0)}
  else {return(1)}}

df$endpoint_reached <- with(df,mapply(fun_endpoint,
  VL_RESULT,VL_DATE,endpoint_l,endpoint_u,STATUS))

# table(df$endpoint_reached, useNA = "always")
tab <- table(df$endpoint_reached)
rownames(tab) <- c(">=20","<20")

# print
knitr::kable(tab,caption = "reached primary endpoint")
```



Table: reached primary endpoint

|Var1 | Freq|
|:----|----:|
|>=20 |  113|
|<20  |  194|

### VL endpoint overview


```r
tab <- percentage_table(df$endpoint_reached,df$ARM)
rownames(tab) <- c(">=20","<20")
# print
knitr::kable(tab,caption = "reached primary endpoint, by arm")
```



Table: reached primary endpoint, by arm

|     |control     |interv.  |
|:----|:-----------|:--------|
|>=20 |62 (39.49%) |51 (34%) |
|<20  |95 (60.51%) |99 (66%) |

```r
fun_endpoint_detail <- function(VL,VL_date,endpoint_l,endpoint_u,status){
  if (status == "out of care"){return("out of care")}
  else if (is.na(VL)){return("in care, VL missing")}
  else if (xor(VL_date < endpoint_l,VL_date > endpoint_u)){return("VL out of window")}
  else if (VL < 20){return("VL < 20")}
  else if (VL >= 20 && VL < 1000){return("VL 20-999")}
  else if (VL >= 1000){return("VL > 999")}}
  
df$endpoint_detail <- with(df,mapply(fun_endpoint_detail,
  VL_RESULT,VL_DATE,endpoint_l,endpoint_u,STATUS))

tab <- percentage_table(df$endpoint_detail,df$ARM)
tab <- tab[c(2,1,3,5,4,6),]

# print
knitr::kable(tab,caption = "primary endpoint detail, by arm")
```



Table: primary endpoint detail, by arm

|                    |control     |interv.     |
|:-------------------|:-----------|:-----------|
|out of care         |26 (16.56%) |23 (15.33%) |
|in care, VL missing |9 (5.73%)   |8 (5.33%)   |
|VL < 20             |95 (60.51%) |99 (66%)    |
|VL 20-999           |19 (12.1%)  |14 (9.33%)  |
|VL > 999            |6 (3.82%)   |3 (2%)      |
|VL out of window    |2 (1.27%)   |3 (2%)      |

```r
out_of_window <- df[df$endpoint_detail == "VL out of window",]
tab <- percentage_table(out_of_window$VL_RESULT<20,out_of_window$ARM)
rownames(tab) <- c(">=20","<20")

# print
knitr::kable(tab,caption = "VLs of those out of window, by arm")
```



Table: VLs of those out of window, by arm

|     |control |interv.    |
|:----|:-------|:----------|
|>=20 |1 (50%) |1 (33.33%) |
|<20  |1 (50%) |2 (66.67%) |

# Primary ITT analysis

For the analysis of the primary outcome, we will use a multi-level logistic regression model including clinic as a random effect, arm allocation and the randomisation stratification factor (district) as a fixed effect. **Gender was included in the analysis as a relevant baseline factors found to be randomly unbalanced between intervention and control arm**. Unavailability of a viral load or invalid VL result in the predefined primary endpoint window (for any reason) will be considered as failures. Results will be reported as odds ratios with 95% confidence intervals **and p-values**.


```r
# use logit link to obtain ORs
fit <- glmer(endpoint_reached ~ DISTRICT + ARM + GENDER + (1|USER), 
              data = df, 
              family = binomial(link = "logit"))

# class(fit)
primary <- df %>% 
  glmer(endpoint_reached ~ DISTRICT + ARM + GENDER + (1|USER), 
              family = binomial(link = "logit"), data=.)
tab_model(primary)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">endpoint reached</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Odds Ratios</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.93</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.26&nbsp;&ndash;&nbsp;2.97</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.003</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">DISTRICT [Leribe]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.71</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.34&nbsp;&ndash;&nbsp;1.49</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.370</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">DISTRICT [MKG]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.59</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.36&nbsp;&ndash;&nbsp;0.98</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.040</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ARM [interv.]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.27</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.79&nbsp;&ndash;&nbsp;2.03</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.327</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">GENDER [male]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.11</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.66&nbsp;&ndash;&nbsp;1.88</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.688</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">Random Effects</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&sigma;<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">3.29</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>00</sub> <sub>USER</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.00</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">N <sub>USER</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">20</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">307</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">Marginal R<sup>2</sup> / Conditional R<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.023 / NA</td>
</tr>

</table>

```r
summ(primary, exp = T, confint = T, model.info = F, model.fit = F, digits = 3)
```

```
## Warning in !is.null(rmarkdown::metadata$output) && rmarkdown::metadata$output
## %in% : 'length(x) = 3 > 1' in coercion to 'logical(1)'
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr><th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Fixed Effects</div></th></tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 1.932 </td>
   <td style="text-align:right;"> 1.258 </td>
   <td style="text-align:right;"> 2.967 </td>
   <td style="text-align:right;"> 3.008 </td>
   <td style="text-align:right;"> 0.003 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> DISTRICTLeribe </td>
   <td style="text-align:right;"> 0.714 </td>
   <td style="text-align:right;"> 0.342 </td>
   <td style="text-align:right;"> 1.492 </td>
   <td style="text-align:right;"> -0.896 </td>
   <td style="text-align:right;"> 0.370 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> DISTRICTMKG </td>
   <td style="text-align:right;"> 0.590 </td>
   <td style="text-align:right;"> 0.357 </td>
   <td style="text-align:right;"> 0.975 </td>
   <td style="text-align:right;"> -2.058 </td>
   <td style="text-align:right;"> 0.040 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> ARMinterv. </td>
   <td style="text-align:right;"> 1.266 </td>
   <td style="text-align:right;"> 0.790 </td>
   <td style="text-align:right;"> 2.028 </td>
   <td style="text-align:right;"> 0.981 </td>
   <td style="text-align:right;"> 0.327 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> GENDERmale </td>
   <td style="text-align:right;"> 1.113 </td>
   <td style="text-align:right;"> 0.660 </td>
   <td style="text-align:right;"> 1.877 </td>
   <td style="text-align:right;"> 0.401 </td>
   <td style="text-align:right;"> 0.688 </td>
  </tr>
</tbody>
</table> <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr><th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Random Effects</div></th></tr>
  <tr>
   <th> Group </th>
   <th> Parameter </th>
   <th> Std. Dev. </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td> USER </td>
   <td> (Intercept) </td>
   <td> 0.000 </td>
  </tr>
</tbody>
</table> <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr><th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Grouping Variables</div></th></tr>
  <tr>
   <th> Group </th>
   <th> # groups </th>
   <th> ICC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td> USER </td>
   <td> 20 </td>
   <td> 0.000 </td>
  </tr>
</tbody>
</table>

```r
summ(primary, exp = F, confint = F, model.info = F, model.fit = F, digits = 3)
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr><th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="5"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Fixed Effects</div></th></tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Est. </th>
   <th style="text-align:right;"> S.E. </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.658 </td>
   <td style="text-align:right;"> 0.219 </td>
   <td style="text-align:right;"> 3.008 </td>
   <td style="text-align:right;"> 0.003 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> DISTRICTLeribe </td>
   <td style="text-align:right;"> -0.337 </td>
   <td style="text-align:right;"> 0.376 </td>
   <td style="text-align:right;"> -0.896 </td>
   <td style="text-align:right;"> 0.370 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> DISTRICTMKG </td>
   <td style="text-align:right;"> -0.527 </td>
   <td style="text-align:right;"> 0.256 </td>
   <td style="text-align:right;"> -2.058 </td>
   <td style="text-align:right;"> 0.040 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> ARMinterv. </td>
   <td style="text-align:right;"> 0.236 </td>
   <td style="text-align:right;"> 0.240 </td>
   <td style="text-align:right;"> 0.981 </td>
   <td style="text-align:right;"> 0.327 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> GENDERmale </td>
   <td style="text-align:right;"> 0.107 </td>
   <td style="text-align:right;"> 0.267 </td>
   <td style="text-align:right;"> 0.401 </td>
   <td style="text-align:right;"> 0.688 </td>
  </tr>
</tbody>
</table> <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr><th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Random Effects</div></th></tr>
  <tr>
   <th> Group </th>
   <th> Parameter </th>
   <th> Std. Dev. </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td> USER </td>
   <td> (Intercept) </td>
   <td> 0.000 </td>
  </tr>
</tbody>
</table> <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
<tr><th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Grouping Variables</div></th></tr>
  <tr>
   <th> Group </th>
   <th> # groups </th>
   <th> ICC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td> USER </td>
   <td> 20 </td>
   <td> 0.000 </td>
  </tr>
</tbody>
</table>

```r
# primary <- df %>% 
#   glmer(endpoint_reached ~ DISTRICT + ARM + GENDER + (1|USER), 
#               family = binomial(link = "logit"), data=.) %>% 
#   confint.merMod(method="Wald") %>% 
#   exp()

# class(primary)

# exp(fixef(fit))
# summary(fit)$coefficients

#confidence intervals
confint <- exp(confint.merMod(fit,method="Wald"))
confint <- t(confint[!rownames(confint)%in%c("(Intercept)",".sig01"),])

data <- data.frame(
  "odds ratio" = round(exp(fixef(fit)[names(fixef(fit))!="(Intercept)"]),2),
  "confidence interval" = paste_confint(confint),
  "p-value" = round(summary(fit)$coefficients[,4][names(summary(fit)$coefficients[,4])!="(Intercept)"],3))

# ordering A on top
data <- data[order(rownames(data)),]           

knitr::kable(data,caption = "individual-level analysis adjusted for district and gender")
```



Table: individual-level analysis adjusted for district and gender

|               | odds.ratio|confidence.interval | p.value|
|:--------------|----------:|:-------------------|-------:|
|ARMinterv.     |       1.27|(0.79 to 2.03)      |   0.327|
|DISTRICTLeribe |       0.71|(0.34 to 1.49)      |   0.370|
|DISTRICTMKG    |       0.59|(0.36 to 0.98)      |   0.040|
|GENDERmale     |       1.11|(0.66 to 1.88)      |   0.688|

# Explore small sample correction


```r
# small sample correction should be taken into account (https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-022-01699-2) -> use p-values from a t-distribution with degrees of freedom as clusters minus cluster-level parameters / and use restricted pseudolikelihood (RPL) estimation instead of REML if convergence issues

# Extract estimated coefficients and standard errors
coefs <- coef(summary(fit))[, "Estimate"]
std_errors <- coef(summary(fit))[, "Std. Error"]

odds_ratio <- exp(coefs)

# Calculate the t-values
t_values <- coefs / std_errors

# number of clusters
n_clusters <- length(unique(df$USER))
# Correct the degrees of freedom (Clusters minus cluster-level parameters: Leribe, MKG, Male, Arm, Intercept)
df_correction <- n_clusters - 5

# Calculate the p-values based on the t-values and degrees of freedom
p_values <- 2 * pt(abs(t_values), df = df_correction, lower.tail = FALSE)

# Calculate the critical value from the t-distribution for the desired confidence level
t_critical <- qt(0.975, df = df_correction)

# Calculate the confidence intervals using the modified degrees of freedom
conf_intervals <- t_critical * std_errors

# Calculate the lower and upper limits of the confidence intervals
lower_limit <- exp(coefs - conf_intervals)
upper_limit <- exp(coefs + conf_intervals)

# Combine all // very similar point estimate but slightly increases uncertainty -> keep original model
results <- cbind(coefs, odds_ratio, std_errors, t_values, p_values, lower_limit, upper_limit)
print(results)
```

```
##                     coefs odds_ratio std_errors   t_values    p_values
## (Intercept)     0.6584987  1.9318898  0.2189169  3.0079852 0.008828004
## DISTRICTLeribe -0.3369242  0.7139630  0.3760493 -0.8959575 0.384428682
## DISTRICTMKG    -0.5274376  0.5901151  0.2563479 -2.0575073 0.057456319
## ARMinterv.      0.2358977  1.2660448  0.2404740  0.9809697 0.342165901
## GENDERmale      0.1070197  1.1129561  0.2666882  0.4012913 0.693864326
##                lower_limit upper_limit
## (Intercept)      1.2115353    3.080552
## DISTRICTLeribe   0.3203137    1.591387
## DISTRICTMKG      0.3416973    1.019136
## ARMinterv.       0.7583119    2.113734
## GENDERmale       0.6303924    1.964921
```


# Sensitivity analysis on ITT set

### Wider Endpoint window

We use a wider window for the primary endpoint, namely 9-18 months.


```r
# endpoint window
df$endpoint_l <- as.Date(df$DATE_ENROL) + 252
df$endpoint_u_alternative <- as.Date(df$DATE_ENROL) + (30*18)

# defintion endpoint alternative
fun_endpoint_alternative <- function(VL,VL_date,endpoint_l,endpoint_u_alternative,status){
  if (is.na(VL)){return(0)}
  else if (VL >= 20){return(0)}
  else if (status == "out of care"){return(0)}
  else if (xor(VL_date < endpoint_l,VL_date > endpoint_u_alternative)){return(0)}
  else {return(1)}}

df$endpoint_reached_alt <- with(df,mapply(fun_endpoint_alternative,
  VL_RESULT,VL_DATE,endpoint_l,endpoint_u_alternative,STATUS))

tab <- percentage_table(df$endpoint_reached_alt,df$ARM)
# print
knitr::kable(tab,caption = "primary endpoint reached with wider endpoint window, by arm")
```



Table: primary endpoint reached with wider endpoint window, by arm

|   |control     |interv.      |
|:--|:-----------|:------------|
|0  |61 (38.85%) |49 (32.67%)  |
|1  |96 (61.15%) |101 (67.33%) |

```r
# use logit link to obtain ORs
fit <- glmer(endpoint_reached_alt ~ DISTRICT + ARM + GENDER + (1|USER), 
              data = df, 
              family = binomial(link = "logit"))
tab_model(fit)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">endpoint reached alt</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Odds Ratios</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.90</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.24&nbsp;&ndash;&nbsp;2.92</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.003</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">DISTRICT [Leribe]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.68</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.33&nbsp;&ndash;&nbsp;1.43</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.315</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">DISTRICT [MKG]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.61</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.37&nbsp;&ndash;&nbsp;1.01</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.054</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ARM [interv.]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.29</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.80&nbsp;&ndash;&nbsp;2.07</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.292</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">GENDER [male]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.30</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.76&nbsp;&ndash;&nbsp;2.21</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.337</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">Random Effects</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&sigma;<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">3.29</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>00</sub> <sub>USER</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.00</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">N <sub>USER</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">20</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">307</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">Marginal R<sup>2</sup> / Conditional R<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.026 / NA</td>
</tr>

</table>

```r
#confidence intervals
confint <- exp(confint.merMod(fit,method="Wald"))
confint <- t(confint[!rownames(confint)%in%c("(Intercept)",".sig01"),])

data <- data.frame("odds ratio" = round(exp(fixef(fit)[names(fixef(fit))!="(Intercept)"]),2),
           "confidence interval" = paste_confint(confint),
           "p-value" = round(summary(fit)$coefficients[,4][names(summary(fit)$coefficients[,4])!="(Intercept)"],3))

# order by A
data <- data [order(rownames(data)),]           

# print
knitr::kable(data,caption = "primary endpoint with 9-18 window: individual-level analysis adjusted for district and gender")
```



Table: primary endpoint with 9-18 window: individual-level analysis adjusted for district and gender

|               | odds.ratio|confidence.interval | p.value|
|:--------------|----------:|:-------------------|-------:|
|ARMinterv.     |       1.29|(0.8 to 2.07)       |   0.292|
|DISTRICTLeribe |       0.68|(0.33 to 1.43)      |   0.315|
|DISTRICTMKG    |       0.61|(0.37 to 1.01)      |   0.054|
|GENDERmale     |       1.30|(0.76 to 2.21)      |   0.337|


# "Per-protocol set/analysis" (according to protocol/publication)

We analyse the individual-based per protocol set, considering participants who have 1. a documented 6 month visit (range: 5 -- 8 months); and **(this was the only difference compared to primary analysis)** 2. a documented 12 months visit with viral load measurement (range: 9 -- 15 months).


```r
df$status_6 <- status_6$STATUS[match(df$STICKER_ID,status_6$STICKER_ID)]
df$REFILL_NO_6 <- status_6$REFILL_NO[match(df$STICKER_ID,status_6$STICKER_ID)]
df$endpoint_reached_pp <- df$endpoint_reached 
df$endpoint_reached_pp[df$status_6 == "out of care"] <- 0

tab <- percentage_table(df$endpoint_reached_pp,df$ARM)
# print
knitr::kable(tab,caption = "reached primary endpoint per protocol, by arm")
```



Table: reached primary endpoint per protocol, by arm

|   |control     |interv.     |
|:--|:-----------|:-----------|
|0  |69 (43.95%) |55 (36.67%) |
|1  |88 (56.05%) |95 (63.33%) |

```r
fit <- glmer(endpoint_reached_pp ~ DISTRICT + ARM + GENDER + (1|USER), 
              data = df, 
              family = binomial(link = "logit"))
tab_model(fit)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">endpoint reached pp</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Odds Ratios</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.42</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.94&nbsp;&ndash;&nbsp;2.16</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.097</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">DISTRICT [Leribe]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.91</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.44&nbsp;&ndash;&nbsp;1.89</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.796</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">DISTRICT [MKG]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.68</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.42&nbsp;&ndash;&nbsp;1.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.127</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ARM [interv.]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.33</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.84&nbsp;&ndash;&nbsp;2.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.222</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">GENDER [male]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.27</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.76&nbsp;&ndash;&nbsp;2.12</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.368</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">Random Effects</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&sigma;<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">3.29</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>00</sub> <sub>USER</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.00</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">N <sub>USER</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">20</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">307</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">Marginal R<sup>2</sup> / Conditional R<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.020 / NA</td>
</tr>

</table>

```r
confint <- exp(confint.merMod(fit,method="Wald"))
confint <- t(confint[!rownames(confint)%in%c("(Intercept)",".sig01"),])
data <- data.frame("odds ratio" = round(exp(fixef(fit)[names(fixef(fit))!="(Intercept)"]),2),
           "confidence interval" = paste_confint(confint),
           "p-value" = round(summary(fit)$coefficients[,4][names(summary(fit)$coefficients[,4])!="(Intercept)"],3))

# order by A
data <- data [order(rownames(data)),]           
      
# print
knitr::kable(data,caption = "per-protocol individual-level analysis adjusted for district and gender")
```



Table: per-protocol individual-level analysis adjusted for district and gender

|               | odds.ratio|confidence.interval | p.value|
|:--------------|----------:|:-------------------|-------:|
|ARMinterv.     |       1.33|(0.84 to 2.12)      |   0.222|
|DISTRICTLeribe |       0.91|(0.44 to 1.89)      |   0.796|
|DISTRICTMKG    |       0.68|(0.42 to 1.12)      |   0.127|
|GENDERmale     |       1.27|(0.76 to 2.12)      |   0.368|

# Complier Average Causal Effect (CACE) analysis (i.e., optimized per-protocol analysis)

The ITT analysis estimates a "policy-level" estimate, meaning the effect if PEBRA model is being implemented at a health facility, irrespective of uptake and adherence to the care model by individual participants.
The per-protocol analysis according to protocol/publication excludes participants with missing VL measurements at 6 and 12 months. This is based on a strong assumption that participants with missing VL measurements are not different to those participants with available VL measurements with respect to all predictors of outcome since it compromises randomization. Moreover, it is still a "policy-level" estimate; it does not tell a patient or a clinician what the effect of the PEBRA model is if a patient adheres to the model as intended. Besides, the non-uptake in the intervention dilutes both these trial estimates.
A better option is to conduct a *Complier Average Causal Effect (CACE)* analysis. We plan to use two methods for such a CACE analysis: 1) instrumental variable (IV) analysis and 2) propensity score (PS) matched or weighted analysis. These methods are based on different underlying assumptions, we will discuss them in each chapter. Both analyses ideally deal with a situation whereby the intervention is administered at one time point only since the definition of non-uptake/-compliance/-adherence to the intervention is based on one overall value, i.e. the "point compliance". Therefore, we first define the point compliance in PEBRA. This is a reasonable assumption. However, we may extend the analysis to a situation whereby the intervention is administered over time and thus, time-varying non-uptake/-compliance/-adherence is taken into account ("sustained compliance"), using g-estimation methods.

## First, we define a reasonable "point compliance" in PEBRA intervention.

This analysis shall answers the question: "What would be the treatment effect if participants would have chosen the PEBRA model?" So, this includes all participants that chose any peer-educator support vs those that chose only nurse support (i.e. not the PEBRA model) => variable pe_support (see above)

### TABLE characteristics by group and by complier within intervention group


```r
# pe_support
table(df$pe_support, useNA = "always") # 11 wanted ONLY nurse-support, i.e., did not choose PEBRA model.
```

```
## 
##   No  Yes <NA> 
##   11  139  157
```

```r
df <- df %>% 
  mutate(endpoint_reached_f = case_when(endpoint_reached == 1 ~ "Below 20 c/mL",
                                endpoint_reached == 0 ~ "Above 20 c/mL or missing"))
df <- df %>% 
  mutate(pe_support = case_when(pe_support == "Yes" ~ "Accepted PEBRA",
                                pe_support == "No" ~ "Refused PEBRA",
                                TRUE ~ "Refused PEBRA"))

# table for sociodemographic characteristics
vars.list <- c("ARM","pe_support","GENDER","AGE","CELL_GIVEN","SEX_ORIENT","currently_attending","no_schooling","N_school","emplyoment","occupation","profession","Maritalstatus","pregnant_breastfeeding","Howmany","using_fp","N_HIV_question","expenses_transport_yn","expenses_transport_cost","expenses_food_yn","expenses_food_cost","diagnosis_year_cat","time_diag_enrol","artstart_year_cat","time_artstart_enrol","infection_year_cat","time_infection_enrol","CurrentARTregimen","Currently_TBx","cd4_start_cat","baseline_Vl_cat","infection_mode","endpoint_reached_f")

df_cace <- df[,colnames(df)%in%vars.list]
df_cace <- df_cace[,match(vars.list,colnames(df_cace))]

colnames(df_cace) <- vars.list <- c("ARM","complier","Gender","Age at enrolment","Cell phone to receive confidential information","Sexual orientation","Currently attending school","No schooling","Number of completed school years","Employment","Occupation","Profession (if (self-)employed)","Marital status","Pregnant  or breastfeeding","Number of children","Contraception use","Number of correctly answered HIV knowledge questions (maximum 10)","Expenses: transport","Expenses: transport costs","Expenses: food","Expenses: food costs","Year of HIV diagnosis","Years since HIV diagnosis","Year of starting ART","Years since starting ART","Year of HIV infection","Years since HIV infection","Current ART regimen","Currently on TB treatment","CD4 count at ART start","Baseline viral load","How do you believe you were infected with HIV?","Primary endpoint reached as per primary analysis")

char_vars <- c("Gender", "Cell phone to receive confidential information","Sexual orientation","Currently attending school","No schooling","Employment","Occupation","Profession (if (self-)employed)","Marital status","Pregnant  or breastfeeding","Number of children", "Contraception use","Expenses: transport","Expenses: food","Year of HIV diagnosis","Year of starting ART","Year of HIV infection","Current ART regimen","Currently on TB treatment","CD4 count at ART start","Baseline viral load","How do you believe you were infected with HIV?","Primary endpoint reached as per primary analysis")

# Convert character variables to factors
df_cace <- df_cace %>%
  mutate(across(all_of(char_vars), factor))

# take special care of ARM and complier and primary endpoint
df_cace <- df_cace %>% 
  mutate(ARM = case_when(ARM == "control" ~ 0,
                         ARM == "interv." ~ 1))
df_cace$ARM <- factor(df_cace$ARM, levels = c(0, 1))

df_cace <- df_cace %>% 
  mutate(complier = case_when(complier == "Refused PEBRA" ~ 0,
                         complier == "Accepted PEBRA" ~ 1))
df_cace$complier <- factor(df_cace$complier, levels = c(0, 1))

df_cace <- df_cace %>% 
  mutate(`Primary endpoint reached as per primary analysis` = case_when(`Primary endpoint reached as per primary analysis` == "Above 20 c/mL or missing" ~ 0,
                         `Primary endpoint reached as per primary analysis` == "Below 20 c/mL" ~ 1))
df_cace$`Primary endpoint reached as per primary analysis` <- factor(df_cace$`Primary endpoint reached as per primary analysis`, levels = c(0, 1))

# Correct numeric variables
df_cace$`Number of correctly answered HIV knowledge questions (maximum 10)` <- as.numeric(df_cace$`Number of correctly answered HIV knowledge questions (maximum 10)`)

# Replace empty levels
df_cace <- df_cace %>% 
  mutate(`Pregnant  or breastfeeding` = as.factor(case_when(`Pregnant  or breastfeeding` == "No" ~ "No",
                         `Pregnant  or breastfeeding` == "Yes" ~ "Yes",
                         TRUE ~ "not applicable")))


# intervention arm only (for CreateTableOne)
df_cace_int <- df_cace %>%
  filter(ARM == "1")

table_cace_int <- tableone::CreateTableOne(data = df_cace_int, vars = vars.list[!vars.list %in% c("complier", "ARM")], strata = "complier", includeNA = TRUE, test = TRUE, addOverall = TRUE)

capture.output(table_cace_int <- print(table_cace_int, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = TRUE,printToggle = FALSE,missing = TRUE))
```

```
## character(0)
```

```r
#print
knitr::kable(table_cace_int,caption = "Baseline characteristics of intervention participants, by compliance to PEBRA model")
```



Table: Baseline characteristics of intervention participants, by compliance to PEBRA model

|                                                                                 |level                             |Overall              |0                    |1                    |p     |test    |Missing |
|:--------------------------------------------------------------------------------|:---------------------------------|:--------------------|:--------------------|:--------------------|:-----|:-------|:-------|
|n                                                                                |                                  |150                  |11                   |139                  |      |        |        |
|Gender (%)                                                                       |female                            |99 (66.0)            |8 ( 72.7)            |91 (65.5)            |0.874 |        |0.0     |
|                                                                                 |male                              |51 (34.0)            |3 ( 27.3)            |48 (34.5)            |      |        |        |
|Age at enrolment (median [IQR])                                                  |                                  |18.72 [16.81, 22.07] |22.01 [18.92, 22.71] |18.37 [16.75, 21.87] |0.055 |nonnorm |0.0     |
|Cell phone to receive confidential information (%)                               |No                                |54 (36.0)            |3 ( 27.3)            |51 (36.7)            |0.764 |        |0.0     |
|                                                                                 |Yes                               |96 (64.0)            |8 ( 72.7)            |88 (63.3)            |      |        |        |
|Sexual orientation (%)                                                           |gay or lesbian                    |1 ( 0.7)             |0 (  0.0)            |1 ( 0.7)             |0.923 |        |0.0     |
|                                                                                 |prefer not to answer              |1 ( 0.7)             |0 (  0.0)            |1 ( 0.7)             |      |        |        |
|                                                                                 |straight or heterosexual          |148 (98.7)           |11 (100.0)           |137 (98.6)           |      |        |        |
|Currently attending school (%)                                                   |No                                |91 (60.7)            |9 ( 81.8)            |82 (59.0)            |0.242 |        |0.0     |
|                                                                                 |Yes                               |59 (39.3)            |2 ( 18.2)            |57 (41.0)            |      |        |        |
|No schooling (%)                                                                 |No                                |149 (99.3)           |11 (100.0)           |138 (99.3)           |1.000 |        |0.0     |
|                                                                                 |Yes                               |1 ( 0.7)             |0 (  0.0)            |1 ( 0.7)             |      |        |        |
|Number of completed school years (median [IQR])                                  |                                  |9.00 [7.25, 10.00]   |9.00 [7.50, 11.00]   |9.00 [7.50, 10.00]   |0.910 |nonnorm |0.0     |
|Employment (%)                                                                   |Employed in Lesotho               |8 ( 5.3)             |1 (  9.1)            |7 ( 5.0)             |0.949 |        |0.0     |
|                                                                                 |Employed in RSA                   |1 ( 0.7)             |0 (  0.0)            |1 ( 0.7)             |      |        |        |
|                                                                                 |Housewife                         |18 (12.0)            |1 (  9.1)            |17 (12.2)            |      |        |        |
|                                                                                 |No regular income / unemployed    |115 (76.7)           |9 ( 81.8)            |106 (76.3)           |      |        |        |
|                                                                                 |Self-employed with regular income |4 ( 2.7)             |0 (  0.0)            |4 ( 2.9)             |      |        |        |
|                                                                                 |Subsistence farming               |4 ( 2.7)             |0 (  0.0)            |4 ( 2.9)             |      |        |        |
|Occupation (%)                                                                   |(self-)employed                   |13 ( 8.7)            |1 (  9.1)            |12 ( 8.6)            |0.356 |        |0.0     |
|                                                                                 |attending school                  |57 (38.0)            |2 ( 18.2)            |55 (39.6)            |      |        |        |
|                                                                                 |nothing                           |80 (53.3)            |8 ( 72.7)            |72 (51.8)            |      |        |        |
|Profession (if (self-)employed) (%)                                              |Business man/woman                |1 ( 0.7)             |0 (  0.0)            |1 ( 0.7)             |0.950 |        |91.3    |
|                                                                                 |Domestic worker                   |1 ( 0.7)             |0 (  0.0)            |1 ( 0.7)             |      |        |        |
|                                                                                 |Herdboy                           |3 ( 2.0)             |0 (  0.0)            |3 ( 2.2)             |      |        |        |
|                                                                                 |Other                             |8 ( 5.3)             |1 (  9.1)            |7 ( 5.0)             |      |        |        |
|                                                                                 |NA                                |137 (91.3)           |10 ( 90.9)           |127 (91.4)           |      |        |        |
|Marital status (%)                                                               |div/sep/wid                       |4 ( 2.7)             |0 (  0.0)            |4 ( 2.9)             |0.076 |        |0.0     |
|                                                                                 |married                           |39 (26.0)            |6 ( 54.5)            |33 (23.7)            |      |        |        |
|                                                                                 |single                            |107 (71.3)           |5 ( 45.5)            |102 (73.4)           |      |        |        |
|Pregnant  or breastfeeding (%)                                                   |No                                |80 (53.3)            |5 ( 45.5)            |75 (54.0)            |0.317 |        |0.0     |
|                                                                                 |not applicable                    |51 (34.0)            |3 ( 27.3)            |48 (34.5)            |      |        |        |
|                                                                                 |Yes                               |19 (12.7)            |3 ( 27.3)            |16 (11.5)            |      |        |        |
|Number of children (%)                                                           |0                                 |109 (72.7)           |6 ( 54.5)            |103 (74.1)           |0.037 |        |0.0     |
|                                                                                 |1                                 |35 (23.3)            |3 ( 27.3)            |32 (23.0)            |      |        |        |
|                                                                                 |2 or 3                            |6 ( 4.0)             |2 ( 18.2)            |4 ( 2.9)             |      |        |        |
|Contraception use (%)                                                            |I prefer not to answer            |11 ( 7.3)            |0 (  0.0)            |11 ( 7.9)            |0.361 |        |2.0     |
|                                                                                 |No                                |68 (45.3)            |5 ( 45.5)            |63 (45.3)            |      |        |        |
|                                                                                 |Not currently sexually active     |19 (12.7)            |0 (  0.0)            |19 (13.7)            |      |        |        |
|                                                                                 |Yes                               |49 (32.7)            |6 ( 54.5)            |43 (30.9)            |      |        |        |
|                                                                                 |NA                                |3 ( 2.0)             |0 (  0.0)            |3 ( 2.2)             |      |        |        |
|Number of correctly answered HIV knowledge questions (maximum 10) (median [IQR]) |                                  |9.00 [9.00, 10.00]   |9.00 [9.00, 9.00]    |9.00 [9.00, 10.00]   |0.280 |nonnorm |0.0     |
|Expenses: transport (%)                                                          |no                                |100 (66.7)           |7 ( 63.6)            |93 (66.9)            |1.000 |        |0.0     |
|                                                                                 |yes                               |50 (33.3)            |4 ( 36.4)            |46 (33.1)            |      |        |        |
|Expenses: transport costs (median [IQR])                                         |                                  |20.00 [10.00, 30.00] |12.50 [9.75, 18.75]  |20.00 [10.00, 30.00] |0.472 |nonnorm |66.7    |
|Expenses: food (%)                                                               |no                                |130 (86.7)           |8 ( 72.7)            |122 (87.8)           |0.341 |        |0.0     |
|                                                                                 |yes                               |20 (13.3)            |3 ( 27.3)            |17 (12.2)            |      |        |        |
|Expenses: food costs (median [IQR])                                              |                                  |10.00 [5.00, 21.25]  |20.00 [12.50, 25.00] |10.00 [5.00, 20.00]  |0.593 |nonnorm |86.7    |
|Year of HIV diagnosis (%)                                                        |2005-2009                         |45 (30.0)            |1 (  9.1)            |44 (31.7)            |0.284 |        |0.0     |
|                                                                                 |2010-2014                         |39 (26.0)            |4 ( 36.4)            |35 (25.2)            |      |        |        |
|                                                                                 |2015-2020                         |66 (44.0)            |6 ( 54.5)            |60 (43.2)            |      |        |        |
|Years since HIV diagnosis (median [IQR])                                         |                                  |5.45 [2.91, 10.99]   |3.28 [1.21, 6.20]    |5.76 [2.97, 11.13]   |0.057 |nonnorm |0.0     |
|Year of starting ART (%)                                                         |2005-2009                         |35 (23.3)            |0 (  0.0)            |35 (25.2)            |0.104 |        |0.0     |
|                                                                                 |2010-2014                         |39 (26.0)            |5 ( 45.5)            |34 (24.5)            |      |        |        |
|                                                                                 |2015-2020                         |76 (50.7)            |6 ( 54.5)            |70 (50.4)            |      |        |        |
|Years since starting ART (median [IQR])                                          |                                  |4.90 [2.67, 9.35]    |3.28 [1.21, 5.09]    |4.98 [2.84, 9.74]    |0.063 |nonnorm |0.0     |
|Year of HIV infection (%)                                                        |1995-1999                         |4 ( 2.7)             |0 (  0.0)            |4 ( 2.9)             |0.096 |        |0.0     |
|                                                                                 |2000-2004                         |23 (15.3)            |0 (  0.0)            |23 (16.5)            |      |        |        |
|                                                                                 |2005-2009                         |39 (26.0)            |1 (  9.1)            |38 (27.3)            |      |        |        |
|                                                                                 |2010-2014                         |29 (19.3)            |5 ( 45.5)            |24 (17.3)            |      |        |        |
|                                                                                 |2015-2020                         |55 (36.7)            |5 ( 45.5)            |50 (36.0)            |      |        |        |
|Years since HIV infection (median [IQR])                                         |                                  |8.43 [3.88, 12.93]   |5.91 [2.90, 7.92]    |9.90 [3.89, 12.98]   |0.040 |nonnorm |0.0     |
|Current ART regimen (%)                                                          |DTG-based                         |14 ( 9.3)            |2 ( 18.2)            |12 ( 8.6)            |0.650 |        |0.0     |
|                                                                                 |EFV-based                         |108 (72.0)           |8 ( 72.7)            |100 (71.9)           |      |        |        |
|                                                                                 |LPV/r-based                       |3 ( 2.0)             |0 (  0.0)            |3 ( 2.2)             |      |        |        |
|                                                                                 |NVP-based                         |25 (16.7)            |1 (  9.1)            |24 (17.3)            |      |        |        |
|Currently on TB treatment (%)                                                    |No                                |148 (98.7)           |11 (100.0)           |137 (98.6)           |1.000 |        |0.0     |
|                                                                                 |Yes                               |2 ( 1.3)             |0 (  0.0)            |2 ( 1.4)             |      |        |        |
|CD4 count at ART start (%)                                                       |<200                              |23 (15.3)            |0 (  0.0)            |23 (16.5)            |0.376 |        |60.7    |
|                                                                                 |200-499                           |21 (14.0)            |1 (  9.1)            |20 (14.4)            |      |        |        |
|                                                                                 |>499                              |15 (10.0)            |2 ( 18.2)            |13 ( 9.4)            |      |        |        |
|                                                                                 |NA                                |91 (60.7)            |8 ( 72.7)            |83 (59.7)            |      |        |        |
|Baseline viral load (%)                                                          |<20                               |82 (54.7)            |5 ( 45.5)            |77 (55.4)            |0.858 |        |11.3    |
|                                                                                 |20-999                            |33 (22.0)            |3 ( 27.3)            |30 (21.6)            |      |        |        |
|                                                                                 |>999                              |18 (12.0)            |2 ( 18.2)            |16 (11.5)            |      |        |        |
|                                                                                 |NA                                |17 (11.3)            |1 (  9.1)            |16 (11.5)            |      |        |        |
|How do you believe you were infected with HIV? (%)                               |horizontal                        |35 (23.3)            |4 ( 36.4)            |31 (22.3)            |0.749 |        |0.0     |
|                                                                                 |I don't know                      |53 (35.3)            |3 ( 27.3)            |50 (36.0)            |      |        |        |
|                                                                                 |I prefer not to answer            |1 ( 0.7)             |0 (  0.0)            |1 ( 0.7)             |      |        |        |
|                                                                                 |vertical                          |61 (40.7)            |4 ( 36.4)            |57 (41.0)            |      |        |        |
|Primary endpoint reached as per primary analysis (%)                             |0                                 |51 (34.0)            |4 ( 36.4)            |47 (33.8)            |1.000 |        |0.0     |
|                                                                                 |1                                 |99 (66.0)            |7 ( 63.6)            |92 (66.2)            |      |        |        |

```r
# add the overall table
# take out missing and total
table_cace_int <- tableone::CreateTableOne(data = df_cace_int, vars = vars.list[!vars.list %in% c("complier", "ARM")], strata = "complier", includeNA = TRUE, test = FALSE, addOverall = FALSE)
capture.output(table_cace_int <- print(table_cace_int, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = FALSE,printToggle = FALSE,missing = FALSE))
```

```
## character(0)
```

```r
table_cace <- tableone::CreateTableOne(data = df_cace, vars = vars.list[!vars.list %in% c("complier", "ARM")], strata = "ARM", includeNA = TRUE, test = FALSE, addOverall = FALSE)
capture.output(table_cace <- print(table_cace, nonnormal = vars.list,catDigits = 1,SMD = TRUE,showAllLevels = TRUE,test = FALSE,printToggle = FALSE,missing = FALSE))
```

```
## character(0)
```

```r
#print both alongside
table_cace_overall <- cbind(table_cace, table_cace_int)
knitr::kable(table_cace_overall,caption = "Baseline characteristics by randomization group and of participants that refused or accepted the PEBRA model")
```



Table: Baseline characteristics by randomization group and of participants that refused or accepted the PEBRA model

|                                                                                 |level                             |0                    |1                    |level                             |0                    |1                    |
|:--------------------------------------------------------------------------------|:---------------------------------|:--------------------|:--------------------|:---------------------------------|:--------------------|:--------------------|
|n                                                                                |                                  |157                  |150                  |                                  |11                   |139                  |
|Gender (%)                                                                       |female                            |119 (75.8)           |99 (66.0)            |female                            |8 ( 72.7)            |91 (65.5)            |
|                                                                                 |male                              |38 (24.2)            |51 (34.0)            |male                              |3 ( 27.3)            |48 (34.5)            |
|Age at enrolment (median [IQR])                                                  |                                  |20.12 [17.03, 22.94] |18.72 [16.81, 22.07] |                                  |22.01 [18.92, 22.71] |18.37 [16.75, 21.87] |
|Cell phone to receive confidential information (%)                               |No                                |52 (33.1)            |54 (36.0)            |No                                |3 ( 27.3)            |51 (36.7)            |
|                                                                                 |Yes                               |105 (66.9)           |96 (64.0)            |Yes                               |8 ( 72.7)            |88 (63.3)            |
|Sexual orientation (%)                                                           |gay or lesbian                    |0 ( 0.0)             |1 ( 0.7)             |gay or lesbian                    |0 (  0.0)            |1 ( 0.7)             |
|                                                                                 |prefer not to answer              |1 ( 0.6)             |1 ( 0.7)             |prefer not to answer              |0 (  0.0)            |1 ( 0.7)             |
|                                                                                 |straight or heterosexual          |156 (99.4)           |148 (98.7)           |straight or heterosexual          |11 (100.0)           |137 (98.6)           |
|Currently attending school (%)                                                   |No                                |121 (77.1)           |91 (60.7)            |No                                |9 ( 81.8)            |82 (59.0)            |
|                                                                                 |Yes                               |36 (22.9)            |59 (39.3)            |Yes                               |2 ( 18.2)            |57 (41.0)            |
|No schooling (%)                                                                 |No                                |153 (97.5)           |149 (99.3)           |No                                |11 (100.0)           |138 (99.3)           |
|                                                                                 |Yes                               |4 ( 2.5)             |1 ( 0.7)             |Yes                               |0 (  0.0)            |1 ( 0.7)             |
|Number of completed school years (median [IQR])                                  |                                  |9.00 [7.00, 11.00]   |9.00 [7.25, 10.00]   |                                  |9.00 [7.50, 11.00]   |9.00 [7.50, 10.00]   |
|Employment (%)                                                                   |Employed in Lesotho               |7 ( 4.5)             |8 ( 5.3)             |Employed in Lesotho               |1 (  9.1)            |7 ( 5.0)             |
|                                                                                 |Employed in RSA                   |1 ( 0.6)             |1 ( 0.7)             |Employed in RSA                   |0 (  0.0)            |1 ( 0.7)             |
|                                                                                 |Housewife                         |25 (15.9)            |18 (12.0)            |Housewife                         |1 (  9.1)            |17 (12.2)            |
|                                                                                 |No regular income / unemployed    |116 (73.9)           |115 (76.7)           |No regular income / unemployed    |9 ( 81.8)            |106 (76.3)           |
|                                                                                 |Self-employed with regular income |1 ( 0.6)             |4 ( 2.7)             |Self-employed with regular income |0 (  0.0)            |4 ( 2.9)             |
|                                                                                 |Subsistence farming               |7 ( 4.5)             |4 ( 2.7)             |Subsistence farming               |0 (  0.0)            |4 ( 2.9)             |
|Occupation (%)                                                                   |(self-)employed                   |9 ( 5.7)             |13 ( 8.7)            |(self-)employed                   |1 (  9.1)            |12 ( 8.6)            |
|                                                                                 |attending school                  |36 (22.9)            |57 (38.0)            |attending school                  |2 ( 18.2)            |55 (39.6)            |
|                                                                                 |nothing                           |112 (71.3)           |80 (53.3)            |nothing                           |8 ( 72.7)            |72 (51.8)            |
|Profession (if (self-)employed) (%)                                              |Business man/woman                |2 ( 1.3)             |1 ( 0.7)             |Business man/woman                |0 (  0.0)            |1 ( 0.7)             |
|                                                                                 |Domestic worker                   |3 ( 1.9)             |1 ( 0.7)             |Domestic worker                   |0 (  0.0)            |1 ( 0.7)             |
|                                                                                 |Herdboy                           |0 ( 0.0)             |3 ( 2.0)             |Herdboy                           |0 (  0.0)            |3 ( 2.2)             |
|                                                                                 |Other                             |4 ( 2.5)             |8 ( 5.3)             |Other                             |1 (  9.1)            |7 ( 5.0)             |
|                                                                                 |NA                                |148 (94.3)           |137 (91.3)           |NA                                |10 ( 90.9)           |127 (91.4)           |
|Marital status (%)                                                               |div/sep/wid                       |4 ( 2.5)             |4 ( 2.7)             |div/sep/wid                       |0 (  0.0)            |4 ( 2.9)             |
|                                                                                 |married                           |54 (34.4)            |39 (26.0)            |married                           |6 ( 54.5)            |33 (23.7)            |
|                                                                                 |single                            |99 (63.1)            |107 (71.3)           |single                            |5 ( 45.5)            |102 (73.4)           |
|Pregnant  or breastfeeding (%)                                                   |No                                |104 (66.2)           |80 (53.3)            |No                                |5 ( 45.5)            |75 (54.0)            |
|                                                                                 |not applicable                    |38 (24.2)            |51 (34.0)            |not applicable                    |3 ( 27.3)            |48 (34.5)            |
|                                                                                 |Yes                               |15 ( 9.6)            |19 (12.7)            |Yes                               |3 ( 27.3)            |16 (11.5)            |
|Number of children (%)                                                           |0                                 |91 (58.0)            |109 (72.7)           |0                                 |6 ( 54.5)            |103 (74.1)           |
|                                                                                 |1                                 |46 (29.3)            |35 (23.3)            |1                                 |3 ( 27.3)            |32 (23.0)            |
|                                                                                 |2 or 3                            |20 (12.7)            |6 ( 4.0)             |2 or 3                            |2 ( 18.2)            |4 ( 2.9)             |
|Contraception use (%)                                                            |I prefer not to answer            |12 ( 7.6)            |11 ( 7.3)            |I prefer not to answer            |0 (  0.0)            |11 ( 7.9)            |
|                                                                                 |No                                |18 (11.5)            |68 (45.3)            |No                                |5 ( 45.5)            |63 (45.3)            |
|                                                                                 |Not currently sexually active     |38 (24.2)            |19 (12.7)            |Not currently sexually active     |0 (  0.0)            |19 (13.7)            |
|                                                                                 |Yes                               |85 (54.1)            |49 (32.7)            |Yes                               |6 ( 54.5)            |43 (30.9)            |
|                                                                                 |NA                                |4 ( 2.5)             |3 ( 2.0)             |NA                                |0 (  0.0)            |3 ( 2.2)             |
|Number of correctly answered HIV knowledge questions (maximum 10) (median [IQR]) |                                  |9.00 [9.00, 10.00]   |9.00 [9.00, 10.00]   |                                  |9.00 [9.00, 9.00]    |9.00 [9.00, 10.00]   |
|Expenses: transport (%)                                                          |no                                |104 (66.2)           |100 (66.7)           |no                                |7 ( 63.6)            |93 (66.9)            |
|                                                                                 |yes                               |53 (33.8)            |50 (33.3)            |yes                               |4 ( 36.4)            |46 (33.1)            |
|Expenses: transport costs (median [IQR])                                         |                                  |16.00 [8.00, 35.00]  |20.00 [10.00, 30.00] |                                  |12.50 [9.75, 18.75]  |20.00 [10.00, 30.00] |
|Expenses: food (%)                                                               |no                                |130 (82.8)           |130 (86.7)           |no                                |8 ( 72.7)            |122 (87.8)           |
|                                                                                 |yes                               |27 (17.2)            |20 (13.3)            |yes                               |3 ( 27.3)            |17 (12.2)            |
|Expenses: food costs (median [IQR])                                              |                                  |10.00 [8.00, 20.00]  |10.00 [5.00, 21.25]  |                                  |20.00 [12.50, 25.00] |10.00 [5.00, 20.00]  |
|Year of HIV diagnosis (%)                                                        |2005-2009                         |29 (18.5)            |45 (30.0)            |2005-2009                         |1 (  9.1)            |44 (31.7)            |
|                                                                                 |2010-2014                         |30 (19.1)            |39 (26.0)            |2010-2014                         |4 ( 36.4)            |35 (25.2)            |
|                                                                                 |2015-2020                         |98 (62.4)            |66 (44.0)            |2015-2020                         |6 ( 54.5)            |60 (43.2)            |
|Years since HIV diagnosis (median [IQR])                                         |                                  |3.63 [1.52, 7.87]    |5.45 [2.91, 10.99]   |                                  |3.28 [1.21, 6.20]    |5.76 [2.97, 11.13]   |
|Year of starting ART (%)                                                         |2005-2009                         |21 (13.4)            |35 (23.3)            |2005-2009                         |0 (  0.0)            |35 (25.2)            |
|                                                                                 |2010-2014                         |27 (17.2)            |39 (26.0)            |2010-2014                         |5 ( 45.5)            |34 (24.5)            |
|                                                                                 |2015-2020                         |109 (69.4)           |76 (50.7)            |2015-2020                         |6 ( 54.5)            |70 (50.4)            |
|Years since starting ART (median [IQR])                                          |                                  |3.14 [1.21, 5.82]    |4.90 [2.67, 9.35]    |                                  |3.28 [1.21, 5.09]    |4.98 [2.84, 9.74]    |
|Year of HIV infection (%)                                                        |1995-1999                         |6 ( 3.8)             |4 ( 2.7)             |1995-1999                         |0 (  0.0)            |4 ( 2.9)             |
|                                                                                 |2000-2004                         |26 (16.6)            |23 (15.3)            |2000-2004                         |0 (  0.0)            |23 (16.5)            |
|                                                                                 |2005-2009                         |18 (11.5)            |39 (26.0)            |2005-2009                         |1 (  9.1)            |38 (27.3)            |
|                                                                                 |2010-2014                         |19 (12.1)            |29 (19.3)            |2010-2014                         |5 ( 45.5)            |24 (17.3)            |
|                                                                                 |2015-2020                         |88 (56.1)            |55 (36.7)            |2015-2020                         |5 ( 45.5)            |50 (36.0)            |
|Years since HIV infection (median [IQR])                                         |                                  |4.93 [2.89, 12.87]   |8.43 [3.88, 12.93]   |                                  |5.91 [2.90, 7.92]    |9.90 [3.89, 12.98]   |
|Current ART regimen (%)                                                          |DTG-based                         |26 (16.6)            |14 ( 9.3)            |DTG-based                         |2 ( 18.2)            |12 ( 8.6)            |
|                                                                                 |EFV-based                         |104 (66.2)           |108 (72.0)           |EFV-based                         |8 ( 72.7)            |100 (71.9)           |
|                                                                                 |LPV/r-based                       |5 ( 3.2)             |3 ( 2.0)             |LPV/r-based                       |0 (  0.0)            |3 ( 2.2)             |
|                                                                                 |NVP-based                         |22 (14.0)            |25 (16.7)            |NVP-based                         |1 (  9.1)            |24 (17.3)            |
|Currently on TB treatment (%)                                                    |No                                |151 (96.2)           |148 (98.7)           |No                                |11 (100.0)           |137 (98.6)           |
|                                                                                 |Yes                               |6 ( 3.8)             |2 ( 1.3)             |Yes                               |0 (  0.0)            |2 ( 1.4)             |
|CD4 count at ART start (%)                                                       |<200                              |13 ( 8.3)            |23 (15.3)            |<200                              |0 (  0.0)            |23 (16.5)            |
|                                                                                 |200-499                           |32 (20.4)            |21 (14.0)            |200-499                           |1 (  9.1)            |20 (14.4)            |
|                                                                                 |>499                              |24 (15.3)            |15 (10.0)            |>499                              |2 ( 18.2)            |13 ( 9.4)            |
|                                                                                 |NA                                |88 (56.1)            |91 (60.7)            |NA                                |8 ( 72.7)            |83 (59.7)            |
|Baseline viral load (%)                                                          |<20                               |84 (53.5)            |82 (54.7)            |<20                               |5 ( 45.5)            |77 (55.4)            |
|                                                                                 |20-999                            |30 (19.1)            |33 (22.0)            |20-999                            |3 ( 27.3)            |30 (21.6)            |
|                                                                                 |>999                              |26 (16.6)            |18 (12.0)            |>999                              |2 ( 18.2)            |16 (11.5)            |
|                                                                                 |NA                                |17 (10.8)            |17 (11.3)            |NA                                |1 (  9.1)            |16 (11.5)            |
|How do you believe you were infected with HIV? (%)                               |horizontal                        |56 (35.7)            |35 (23.3)            |horizontal                        |4 ( 36.4)            |31 (22.3)            |
|                                                                                 |I don't know                      |50 (31.8)            |53 (35.3)            |I don't know                      |3 ( 27.3)            |50 (36.0)            |
|                                                                                 |I prefer not to answer            |5 ( 3.2)             |1 ( 0.7)             |I prefer not to answer            |0 (  0.0)            |1 ( 0.7)             |
|                                                                                 |vertical                          |46 (29.3)            |61 (40.7)            |vertical                          |4 ( 36.4)            |57 (41.0)            |
|Primary endpoint reached as per primary analysis (%)                             |0                                 |62 (39.5)            |51 (34.0)            |0                                 |4 ( 36.4)            |47 (33.8)            |
|                                                                                 |1                                 |95 (60.5)            |99 (66.0)            |1                                 |7 ( 63.6)            |92 (66.2)            |

## Exploiting the exclusion restriction

To test whether compliance occurs at random (with respect to the outcomes of interest) in the intervention group: were compliance random, then the outcome rates among the non-compliers in the intervention group would be equal to those in the control group. 
We fit a log reg model restricted to non-compliers in the intervention group and all individuals in the control group.

```r
df_er <- df %>%
  filter(pe_support == "Refused PEBRA")

table(df_er$ARM, useNA = "always")
```

```
## 
## control interv.    <NA> 
##     157      11       0
```

```r
er <- df_er %>% 
  glmer(endpoint_reached ~ ARM + DISTRICT + GENDER + (1|USER), 
              family = binomial(link = "logit"), data=.)
```

```
## boundary (singular) fit: see help('isSingular')
```

```r
tab_model(er)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">&nbsp;</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">endpoint reached</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">Predictors</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">Odds Ratios</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">CI</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">p</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">(Intercept)</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.73</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.06&nbsp;&ndash;&nbsp;2.82</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  "><strong>0.029</strong></td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">ARM [interv.]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.30</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.36&nbsp;&ndash;&nbsp;4.74</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.692</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">DISTRICT [Leribe]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.85</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.31&nbsp;&ndash;&nbsp;2.32</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.754</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">DISTRICT [MKG]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.65</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.33&nbsp;&ndash;&nbsp;1.28</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.212</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">GENDER [male]</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">1.38</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.66&nbsp;&ndash;&nbsp;2.90</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">0.395</td>
</tr>
<tr>
<td colspan="4" style="font-weight:bold; text-align:left; padding-top:.8em;">Random Effects</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&sigma;<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">3.29</td>
</tr>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">&tau;<sub>00</sub> <sub>USER</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.00</td>

<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">N <sub>USER</sub></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">17</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">Observations</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">168</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">Marginal R<sup>2</sup> / Conditional R<sup>2</sup></td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">0.017 / NA</td>
</tr>

</table>
The OR (95% CI) of viral suppression for non-adherents versus controls was 1.30 (0.36-4.74) after adjustment for the variables of the primary model. These estimates indicate that non-compliers and controls have a different risk of viral suppression, and that direct adjustment for the measured baseline characteristics may be insufficient to eliminate the differences. The results may indicate that per protocol effect estimates will be biased when using any method that requires measurement and adjustment for all joint predictors of compliance and outcome (e.g. conventional regression, standardization, inverse probability weighting, propensity score matching, etc.) To unbiasedly estimate the per protocol effect of PEBRA model on viral suppression, we try first a method that does not require adjustment for joint predictors of compliance and outcome: instrumental variable (IV) estimation. (alternative explanation: the exclusion restriction is not true)


## Investigate further, esp. re predictors for compliance / add OR and 95% to table



The table shows that some baseline characteristics were different between the accepters and refusers (transmission mode, baseline VL, baseline regimen, Years since HIV infection, Years since starting ART, etc.) -> to be explored more. Valid estimation of the traditional per protocol effect would require adjustment for these and any other prognostic factors that predict compliance. As a result, the per protocol effect cannot be generally estimated via a traditional per protocol analysis that directly compares the outcomes of the adherents and the controls. The conducted per protocol analysis above (according to protocol/publication) was different, and did estimate a different question.

## Now, we conduct the first CACE analysis, using IV.

IV analyses are based on several assumptions:
1. The instrumental variable (randomization) is associated with the treatment -> true by design
2. There are no unmeasured common causes between randomization and the outcome -> true by design
3. The effect of the randomization on the outcome is fully mediated through the intervention ("exclusion restrictions") -> we cannot fully rule out that intervention refusers were not influenced by the offer of the intervention. Therefore, we will perform the PS analysis as a sensitivity analysis.
4. Monotonicity (i.e. no “defiers”) -> true by design (consent was different by group; groups were blinded to allocation; only participants randomized to the PEBRA model could have received the PEBRA model. No one was prevented from accessing the PEBRA model because they were randomized to the PEBRA model)
5. Stable unit treatment value assumption: one participants’s randomization to the PEBRA model does not affect another participants’s outcome -> true by design (participants were unaware of their randomization status. Randomization was only linked to the offer of the PEBRA model).

Estimation of the CACE is most often done using a “two-stage least squares” (TSLS) approach (Stuart, 2008; 10.1007/s11121-008-0104-y), which jointly models the two processes of compliance and outcome (Angrist and Imbens, 1995). TSLS involves two models: a regression model of compliance, and a regression model predicting the outcome, given compliance. These models are estimated jointly, to calculate accurate standard errors that account for the uncertainty in the “first-stage” (compliance) model. Using TSLS also allows the inclusion of covariates that predict participation and/or the outcome, which can increase the precision of the estimates.

Accordingly, the primary outcome was analysed using a TSLS model with randomization as the instrumental variable and "accepting the PEBRA model" as the treatment variable. In the first stage, the relation between treatment assignment and treatment acceptance (compliance) was estimated.
In the second stage, the effect of the PEBRA model on the outcome was estimated, using the predicted values from the first stage as an independent variable in a logistic regression model.


```r
# In the 2SLS, we will estimate the predicted treatment in the first stage. We can do it by fitting logistic regression for PEBRA acceptance on randomization, important predictors, and then using the predict command to estimate the probabilities.

df_cace %>% 
  select("ARM", "complier","Gender","Age at enrolment","Cell phone to receive confidential information","Sexual orientation", "Number of completed school years", "Occupation", "Marital status", "Pregnant  or breastfeeding","Number of children","Contraception use","Number of correctly answered HIV knowledge questions (maximum 10)","Expenses: transport","Expenses: food","Years since HIV diagnosis","Years since starting ART","Years since HIV infection","Current ART regimen","Currently on TB treatment","Baseline viral load","How do you believe you were infected with HIV?")
```

```
## # A tibble: 307 × 22
##    ARM   complier Gender `Age at enrolment` Cell phone to receive confidential…¹
##    <fct> <fct>    <fct>               <dbl> <fct>                               
##  1 1     1        male                 15.4 No                                  
##  2 1     1        male                 20.0 Yes                                 
##  3 1     1        female               19.9 Yes                                 
##  4 1     1        female               22.8 Yes                                 
##  5 1     1        female               19.2 Yes                                 
##  6 1     1        female               21.4 Yes                                 
##  7 1     1        female               23.4 Yes                                 
##  8 1     1        female               19.0 Yes                                 
##  9 1     0        male                 18.5 Yes                                 
## 10 1     1        male                 18.0 No                                  
## # ℹ 297 more rows
## # ℹ abbreviated name: ¹​`Cell phone to receive confidential information`
## # ℹ 17 more variables: `Sexual orientation` <fct>,
## #   `Number of completed school years` <dbl>, Occupation <fct>,
## #   `Marital status` <fct>, `Pregnant  or breastfeeding` <fct>,
## #   `Number of children` <fct>, `Contraception use` <fct>,
## #   `Number of correctly answered HIV knowledge questions (maximum 10)` <dbl>, …
```

```r
# Stage 1 of 2SLS
s1 <- glm(complier ~ ARM + 
            `Years since HIV diagnosis` 
          + Gender
          + `Number of completed school years`
          + Occupation
          + `Current ART regimen`
          , data = df_cace, family = binomial("logit"))
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
df_cace$complier.new <- NA
df_cace$complier.new <- predict(s1, type = "response")

# In the second stage, we will fit the outcome model (the logistic regression) with the predicted treatment from the first stage and the confounders. Remember, we should use the robust sandwich SE to estimate the SE of the treatment effect correctly to account for the clustering effect of multiple observations per subject (Cameron AC, Miller DL. A practitioner's guide to cluster-robust inference. J Hum Resour. 2015;50(2):317-372.)

# Stage 2 of 2SLS
fit.2sls <- glm(`Primary endpoint reached as per primary analysis` ~ complier.new + `Years since HIV diagnosis` + Gender, data = df_cace, family = binomial("logit"))
summ(fit.2sls, exp = T, confint = T, model.info = F, model.fit = F, robust = "HC0")
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> 2.5% </th>
   <th style="text-align:right;"> 97.5% </th>
   <th style="text-align:right;"> z val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 1.41 </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 2.16 </td>
   <td style="text-align:right;"> 1.60 </td>
   <td style="text-align:right;"> 0.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> complier.new </td>
   <td style="text-align:right;"> 1.26 </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 2.10 </td>
   <td style="text-align:right;"> 0.90 </td>
   <td style="text-align:right;"> 0.37 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> `Years since HIV diagnosis` </td>
   <td style="text-align:right;"> 1.01 </td>
   <td style="text-align:right;"> 0.96 </td>
   <td style="text-align:right;"> 1.07 </td>
   <td style="text-align:right;"> 0.44 </td>
   <td style="text-align:right;"> 0.66 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Gendermale </td>
   <td style="text-align:right;"> 1.07 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 1.82 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.81 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> Standard errors: Robust, type = HC0</td></tr></tfoot>
</table>

```r
# as expected, result similar but uncertainty increased
```

## Now, conduct the second CACE analysis (sensitivity analysis), using PS (check UMBRELLA/Zelen_SSc).

## Re-consider primary endpoint definition re missing outcomes. And consider MICE for missing covariates (check SPIN/UMBRELLA)
