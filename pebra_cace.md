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

