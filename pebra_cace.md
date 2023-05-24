---
title: "PEBRA Complier Average Causal Effect Analyses"
author: "A.Amstutz"
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













# Baseline characteristics by group: socio-demographics


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
|                                                                                 |2                                 |24 ( 7.8)            |18 (11.5)            |6 ( 4.0)             |        |
|                                                                                 |3                                 |2 ( 0.7)             |2 ( 1.3)             |0 ( 0.0)             |        |
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

# Baseline characteristics by group: clinical




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
