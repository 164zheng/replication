library(estimatr)
library(haven)
library(tidyverse)
library(tibble)
library(modelsummary)

setwd("~/Desktop/Replication/Angrist&Krueger(1991)")
raw_data <- read_dta("NEW7080.dta")

AK91 <-
  raw_data %>%
    rename(age = v1, ageq = v2, educ = v4, enocent = v5, esocent = v6,
           lwklywge = v9, married = v10, midatl = v11, mt = v12, neweng = v13,
           census = v16, qob = v18, race = v19, smsa = v20, soatl = v21,
           wnocent = v24, wsocent = v25, yob = v27) %>%
    filter(census == 80) %>%
    filter(yob >= 30, yob <= 39) %>%
    mutate(ageq = ageq - 1900,
           ageq_squared = ageq ^ 2) %>%
    mutate(yob_1930 = if_else(yob == 30, 1, 0)
          ,yob_1931 = if_else(yob == 31, 1, 0)
          ,yob_1932 = if_else(yob == 32, 1, 0)
          ,yob_1933 = if_else(yob == 33, 1, 0)
          ,yob_1934 = if_else(yob == 34, 1, 0)
          ,yob_1935 = if_else(yob == 35, 1, 0)
          ,yob_1936 = if_else(yob == 36, 1, 0)
          ,yob_1937 = if_else(yob == 37, 1, 0)
          ,yob_1938 = if_else(yob == 38, 1, 0)
          ,yob_1939 = if_else(yob == 39, 1, 0)
          ,qob_1 = if_else(qob == 1, 1, 0)
          ,qob_2 = if_else(qob == 2, 1, 0)
          ,qob_3 = if_else(qob == 3, 1, 0)) %>%
    mutate(yq_30_1 = yob_1930 * qob_1
          ,yq_30_2 = yob_1930 * qob_2
          ,yq_30_3 = yob_1930 * qob_3
          ,yq_31_1 = yob_1931 * qob_1
          ,yq_31_2 = yob_1931 * qob_2
          ,yq_31_3 = yob_1931 * qob_3
          ,yq_32_1 = yob_1932 * qob_1
          ,yq_32_2 = yob_1932 * qob_2
          ,yq_32_3 = yob_1932 * qob_3
          ,yq_33_1 = yob_1933 * qob_1
          ,yq_33_2 = yob_1933 * qob_2
          ,yq_33_3 = yob_1933 * qob_3
          ,yq_34_1 = yob_1934 * qob_1
          ,yq_34_2 = yob_1934 * qob_2
          ,yq_34_3 = yob_1934 * qob_3
          ,yq_35_1 = yob_1935 * qob_1
          ,yq_35_2 = yob_1935 * qob_2
          ,yq_35_3 = yob_1935 * qob_3
          ,yq_36_1 = yob_1936 * qob_1
          ,yq_36_2 = yob_1936 * qob_2
          ,yq_36_3 = yob_1936 * qob_3
          ,yq_37_1 = yob_1937 * qob_1
          ,yq_37_2 = yob_1937 * qob_2
          ,yq_37_3 = yob_1937 * qob_3
          ,yq_38_1 = yob_1938 * qob_1
          ,yq_38_2 = yob_1938 * qob_2
          ,yq_38_3 = yob_1938 * qob_3
          ,yq_39_1 = yob_1939 * qob_1
          ,yq_39_2 = yob_1939 * qob_2
          ,yq_39_3 = yob_1939 * qob_3
   )
summary(comp_AK91)

se_type <- 'classical'

fit_1 <- lm_robust(lwklywge ~ educ + yob_1930 + yob_1931 + yob_1932
                                   + yob_1933 + yob_1934 + yob_1935
                                   + yob_1936 + yob_1937 + yob_1938, data = AK91, se_type = se_type)

fit_2 <- iv_robust(lwklywge ~ educ + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938 |
                     yq_30_1 + yq_30_2 + yq_30_3
                   + yq_31_1 + yq_31_2 + yq_31_3
                   + yq_32_1 + yq_32_2 + yq_32_3
                   + yq_33_1 + yq_33_2 + yq_33_3
                   + yq_34_1 + yq_34_2 + yq_34_3
                   + yq_35_1 + yq_35_2 + yq_35_3
                   + yq_36_1 + yq_36_2 + yq_36_3
                   + yq_37_1 + yq_37_2 + yq_37_3
                   + yq_38_1 + yq_38_2 + yq_38_3
                   + yq_39_1 + yq_39_2 + yq_39_3
                   + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938
                    ,data = AK91, se_type = se_type, diagnostics = TRUE)

fit_3 <- lm_robust(lwklywge ~ educ + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938 + ageq + ageq_squared, data = AK91, se_type = se_type)

fit_4 <- iv_robust(lwklywge ~ educ + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938 + ageq + ageq_squared |
                     yq_30_1 + yq_30_2 + yq_30_3
                   + yq_31_1 + yq_31_2 + yq_31_3
                   + yq_32_1 + yq_32_2 + yq_32_3
                   + yq_33_1 + yq_33_2 + yq_33_3
                   + yq_34_1 + yq_34_2 + yq_34_3
                   + yq_35_1 + yq_35_2 + yq_35_3
                   + yq_36_1 + yq_36_2 + yq_36_3
                   + yq_37_1 + yq_37_2 + yq_37_3
                   + yq_38_1 + yq_38_2 + yq_38_3
                   + yq_39_1 + yq_39_2 + yq_39_3
                   + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938 + ageq + ageq_squared
                   ,data = AK91, se_type = se_type, diagnostics = TRUE)

fit_5 <- lm_robust(lwklywge ~ educ + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938
                   + race + married + smsa + neweng
                   + midatl + enocent + wnocent + soatl + esocent + wsocent + mt
                   , data = AK91, se_type = se_type)


fit_6 <- iv_robust(lwklywge ~ educ + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935 + race + married + smsa + neweng
                   + midatl + enocent + wnocent + soatl + esocent + wsocent + mt
                   + yob_1936 + yob_1937 + yob_1938 |
                     yq_30_1 + yq_30_2 + yq_30_3
                   + yq_31_1 + yq_31_2 + yq_31_3
                   + yq_32_1 + yq_32_2 + yq_32_3
                   + yq_33_1 + yq_33_2 + yq_33_3
                   + yq_34_1 + yq_34_2 + yq_34_3
                   + yq_35_1 + yq_35_2 + yq_35_3
                   + yq_36_1 + yq_36_2 + yq_36_3
                   + yq_37_1 + yq_37_2 + yq_37_3
                   + yq_38_1 + yq_38_2 + yq_38_3
                   + yq_39_1 + yq_39_2 + yq_39_3
                   + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938
                   + race + married + smsa + neweng
                   + midatl + enocent + wnocent + soatl + esocent + wsocent + mt
                   ,data = AK91, se_type = se_type, diagnostics = TRUE)

fit_7 <- lm_robust(lwklywge ~ educ + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938
                   + race + married + smsa + neweng
                   + midatl + enocent + wnocent + soatl + esocent + wsocent + mt + ageq + ageq_squared
                   , data = AK91, se_type = se_type)

fit_8 <- iv_robust(lwklywge ~ educ + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938
                   + race + married + smsa + neweng
                   + midatl + enocent + wnocent + soatl + esocent + wsocent + mt
                   + ageq + ageq_squared  |
                     yq_30_1 + yq_30_2 + yq_30_3
                   + yq_31_1 + yq_31_2 + yq_31_3
                   + yq_32_1 + yq_32_2 + yq_32_3
                   + yq_33_1 + yq_33_2 + yq_33_3
                   + yq_34_1 + yq_34_2 + yq_34_3
                   + yq_35_1 + yq_35_2 + yq_35_3
                   + yq_36_1 + yq_36_2 + yq_36_3
                   + yq_37_1 + yq_37_2 + yq_37_3
                   + yq_38_1 + yq_38_2 + yq_38_3
                   + yq_39_1 + yq_39_2 + yq_39_3
                   + yob_1930 + yob_1931 + yob_1932
                   + yob_1933 + yob_1934 + yob_1935
                   + yob_1936 + yob_1937 + yob_1938
                   + race + married + smsa + neweng
                   + midatl + enocent + wnocent + soatl + esocent + wsocent + mt + ageq + ageq_squared
                   ,data = AK91, se_type = se_type, diagnostics = TRUE)

rows <- tribble(~term, ~"Model 1", ~"Model 2", ~"Model 3", ~"Model 4", ~"Model 5", ~"Model 6", ~"Model 7", ~"Model 8",
                '9 Year of birth dummies', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes',
                '8 Region of residence dummies', 'No', 'No', 'No', 'No', 'Yes', 'Yes', 'Yes', 'Yes')
attr(rows, 'position') <- c(9, 10)
cm <- c('educ' = 'Years of education',
        'race' = 'Race(1 = black)',
        'smsa' = 'SMSA(1 = center city)',
        'married' = 'Married(1 = married)',
        'ageq' = 'Age',
        'ageq_squared' = 'Age-squared')
regs <- list(fit_1, fit_2, fit_3, fit_4, fit_5, fit_6, fit_7, fit_8)
msummary(regs, fmt = '%.4f', coef_map = cm, add_rows = rows, title = 'OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1930-1939: 1980 CENSUS',
         gof_omit = 'Num.Obs.|R2|R2 Adj.|Std.Errors|p.value.endogeneity|p.value.overid|p.value.weakinst|statistic.endogeneity|statistic.overid|statistic.weakinst')
