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
vars.list <- c("ARM","GENDER","AGE","CELL_GIVEN","SEX_ORIENT","currently_attending","no_schooling","N_school")

df_sociodemo <- df[,colnames(df)%in%vars.list]
df_sociodemo <- df_sociodemo[,match(vars.list,colnames(df_sociodemo))]
colnames(df_sociodemo) <- vars.list <- c("ARM","Gender","Age at enrolment","Cell phone to receive confidential information",
                               "Sexual orientation","Currently attending school","No schooling","Number of completed school years")

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

|                                                   |level                    |Overall              |control              |interv.              |Missing |
|:--------------------------------------------------|:------------------------|:--------------------|:--------------------|:--------------------|:-------|
|n                                                  |                         |307                  |157                  |150                  |        |
|Gender (%)                                         |female                   |218 (71.0)           |119 (75.8)           |99 (66.0)            |0.0     |
|                                                   |male                     |89 (29.0)            |38 (24.2)            |51 (34.0)            |        |
|Age at enrolment (median [IQR])                    |                         |19.41 [16.94, 22.44] |20.12 [17.03, 22.94] |18.72 [16.81, 22.07] |0.0     |
|Cell phone to receive confidential information (%) |No                       |106 (34.5)           |52 (33.1)            |54 (36.0)            |0.0     |
|                                                   |Yes                      |201 (65.5)           |105 (66.9)           |96 (64.0)            |        |
|Sexual orientation (%)                             |gay or lesbian           |1 ( 0.3)             |0 ( 0.0)             |1 ( 0.7)             |0.0     |
|                                                   |prefer not to answer     |2 ( 0.7)             |1 ( 0.6)             |1 ( 0.7)             |        |
|                                                   |straight or heterosexual |304 (99.0)           |156 (99.4)           |148 (98.7)           |        |
|Currently attending school (%)                     |No                       |212 (69.1)           |121 (77.1)           |91 (60.7)            |0.0     |
|                                                   |Yes                      |95 (30.9)            |36 (22.9)            |59 (39.3)            |        |
|No schooling (%)                                   |No                       |302 (98.4)           |153 (97.5)           |149 (99.3)           |0.0     |
|                                                   |Yes                      |5 ( 1.6)             |4 ( 2.5)             |1 ( 0.7)             |        |
|Number of completed school years (median [IQR])    |                         |9.00 [7.00, 10.50]   |9.00 [7.00, 11.00]   |9.00 [7.25, 10.00]   |0.0     |


