My Project
================
Varun
2024-10-10

``` r
library(psych)
library(haven)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following objects are masked from 'package:psych':
    ## 
    ##     %+%, alpha

``` r
library(bruceR)
```

    ## 
    ## bruceR (v2024.6)
    ## Broadly Useful Convenient and Efficient R functions
    ## 
    ## Packages also loaded:
    ## ✔ data.table ✔ emmeans
    ## ✔ dplyr      ✔ lmerTest
    ## ✔ tidyr      ✔ effectsize
    ## ✔ stringr    ✔ performance
    ## ✔ ggplot2    ✔ interactions
    ## 
    ## Main functions of `bruceR`:
    ## cc()             Describe()  TTEST()
    ## add()            Freq()      MANOVA()
    ## .mean()          Corr()      EMMEANS()
    ## set.wd()         Alpha()     PROCESS()
    ## import()         EFA()       model_summary()
    ## print_table()    CFA()       lavaan_summary()
    ## 
    ## For full functionality, please install all dependencies:
    ## install.packages("bruceR", dep=TRUE)
    ## 
    ## Online documentation:
    ## https://psychbruce.github.io/bruceR
    ## 
    ## To use this package in publications, please cite:
    ## Bao, H.-W.-S. (2024). bruceR: Broadly useful convenient and efficient R functions (Version 2024.6) [Computer software]. https://CRAN.R-project.org/package=bruceR

    ## 
    ## These packages are dependencies of `bruceR` but not installed:
    ## - pacman, openxlsx, ggtext, lmtest, vars, phia, MuMIn, GGally
    ## 
    ## ***** Install all dependencies *****
    ## install.packages("bruceR", dep=TRUE)

``` r
library(performance)
library(sjPlot)
library(ggstatsplot)
```

    ## You can cite this package as:
    ##      Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
    ##      Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

``` r
library(Rmisc)
```

    ## Loading required package: lattice

    ## Loading required package: plyr

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

\#dataset

``` r
load("C:/Users/vtrip/OneDrive/Desktop/ICPSR_38964/DS0001/38964-0001-Data.rda")
```

``` r
new_dataset <- da38964.0001 %>%
  select(RES1, RES2, RES3, RES4, RES5, RES5, RES6, SAT1, SAT2, SAT3, SAT4, SAT5, SEX, LAD_NOW, LAD_FUT)
```

``` r
#new_dataset<- new_dataset %>%
#  mutate(gender_recode = case_when(
#    SEX == "(1) Male" ~  '1',
#    SEX == "(2) Female" ~ "2"
#    ))
```

``` r
new_dataset$SEX <- as.numeric(new_dataset$SEX)

new_dataset <- new_dataset %>%
  filter(SEX < 3)

new_dataset$RES1 <- as.numeric(new_dataset$RES1)
  


#describe(new_dataset$RES1)
#summary(new_dataset$RES1)
  
new_dataset <- new_dataset %>%
  filter(RES1 < 6)

new_dataset$RES2 <- as.numeric(new_dataset$RES2)
  

new_dataset <- new_dataset %>%
  filter(RES2 < 6)

#describe(new_dataset$RES2)  
#summary(new_dataset$RES2)

new_dataset$RES3 <- as.numeric(new_dataset$RES3)
  
new_dataset <- new_dataset %>%
  filter(RES3 < 6)

#describe(new_dataset$RES3)  
#summary(new_dataset$RES3)

new_dataset$RES4 <- as.numeric(new_dataset$RES4)
  
new_dataset <- new_dataset %>%
  filter(RES4 < 6)

#describe(new_dataset$RES4)  
#summary(new_dataset$RES4)

new_dataset$RES5 <- as.numeric(new_dataset$RES5)
  
new_dataset <- new_dataset %>%
  filter(RES5 < 6)

#describe(new_dataset$RES5)  
#summary(new_dataset$RES5)

new_dataset$RES6 <- as.numeric(new_dataset$RES6)
  
new_dataset <- new_dataset %>%
  filter(RES6 < 6)

#describe(new_dataset$RES6)  
#summary(new_dataset$RES6)

new_dataset$SAT1 <- as.numeric(new_dataset$SAT1)
  
new_dataset <- new_dataset %>%
  filter(SAT1 < 6)

#describe(new_dataset$SAT1)  
#summary(new_dataset$SAT1)

new_dataset$SAT2 <- as.numeric(new_dataset$SAT2)
  
new_dataset <- new_dataset %>%
  filter(SAT2 < 6)

#describe(new_dataset$SAT2)  
#summary(new_dataset$SAT2)

new_dataset$SAT3 <- as.numeric(new_dataset$SAT3)
  
new_dataset <- new_dataset %>%
  filter(SAT3 < 6)

#describe(new_dataset$SAT3)  
#summary(new_dataset$SAT3)

new_dataset$SAT4 <- as.numeric(new_dataset$SAT4)
  
new_dataset <- new_dataset %>%
  filter(SAT4 < 6)

#describe(new_dataset$SAT4)  
#summary(new_dataset$SAT4)

new_dataset$SAT5 <- as.numeric(new_dataset$SAT5)
  
new_dataset <- new_dataset %>%
  filter(SAT5 < 6)

#describe(new_dataset$SAT5)  
#summary(new_dataset$SAT5)

new_dataset$LAD_NOW <- as.numeric(new_dataset$LAD_NOW)
  
new_dataset <- new_dataset %>%
  filter(LAD_NOW < 11)

#describe(new_dataset$LAD_NOW)  
#summary(new_dataset$LAD_NOW)

new_dataset$LAD_FUT <- as.numeric(new_dataset$LAD_FUT)
  
new_dataset <- new_dataset %>%
  filter(LAD_FUT < 11)

#describe(new_dataset$LAD_FUT)  
#summary(new_dataset$LAD_FUT)

#code to calculate social ladder variable
new_dataset$MOB_EXPECT<- new_dataset$LAD_FUT - new_dataset$LAD_NOW
```

``` r
#composite variables

#new_dataset <- new_dataset %>%
  #mutate(RES = rbind(rowMeans(cbind(RES1, RES2, RES3, RES4, RES5, RES6))))

#new_dataset <- new_dataset %>%
  #mutate(SAT = rbind(rowMeans(cbind(SAT1, SAT2, SAT3, SAT4, SAT5))))

new_dataset <- new_dataset %>%
  mutate(RES_composite = rowMeans(cbind(RES1, RES2, RES3, RES4, RES5, RES6)))

new_dataset <- new_dataset %>%
  mutate(SAT_composite = rowMeans(cbind(SAT1, SAT2, SAT3, SAT4, SAT5)))
```

``` r
#summary descriptive statistics

new_dataset %>% 
  group_by(SEX) %>%
  dplyr::summarize(
     mean_RES   = mean(RES_composite),
     mean_SAT    = mean(SAT_composite),
     std_dev_RES = sd(RES_composite),
     std_dev_SAT = sd(SAT_composite),
     corr_RES_SAT  = cor(RES_composite, SAT_composite)
)
```

    ## # A tibble: 2 × 6
    ##     SEX mean_RES mean_SAT std_dev_RES std_dev_SAT corr_RES_SAT
    ##   <dbl>    <dbl>    <dbl>       <dbl>       <dbl>        <dbl>
    ## 1     1     3.06     2.94       0.388       0.964       0.0789
    ## 2     2     3.09     2.91       0.376       0.939       0.0921

``` r
#Weak positive correlation between resilience and satisfaction when grouped by sex, where the correlation is slightly stronger in females. 
```

``` r
#data distribution visualization


#ggplot(new_dataset, aes(x = SEX, y = RES)) + geom_violin(aes(fill=SEX)) + scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) + theme_classic()

#ggplot(new_dataset, aes(x = SEX, y = SAT)) + geom_violin(aes(fill=SEX)) + scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) + theme_classic()
```

``` r
#violinBy(SAT ~ SEX, data = new_dataset, rain= TRUE, vertical = FALSE)
```

``` r
#str(new_dataset)
#mo<-lm(data = new_dataset, MOB_EXPECT ~ RES_composite + SAT_composite)

#check_model(mo)


#ggplot(new_dataset, aes(x = RES_composite)) + geom_histogram(binwidth = 0.1) + theme_classic()

#ggplot(new_dataset, aes(x = SAT_composite)) + geom_histogram(binwidth = 0.1) + theme_classic()
```

``` r
#Normality test for SAT, Shapiro

#shapiro.test(new_dataset$SAT)

#shapiro.test(new_dataset$RES)
```

``` r
#new_dataset$SAT_log <- log10(new_dataset$SAT)

#ggplot(new_dataset, aes(x = SAT_log)) + geom_histogram(binwidth = 0.05) + theme_classic()

#new_dataset$RES_log <- log10(new_dataset$RES)

#ggplot(new_dataset, aes(x = RES_log)) + geom_histogram(binwidth = 0.05) + theme_classic()
```

``` r
#shapiro.test(new_dataset$SAT_log)
#shapiro.test(new_dataset$RES_log)

#Based on the original histograms, RES seemed normal & SAT did not. Based on the S-W normality test, both were non-normal. Remained so depite log transformation
```

``` r
#ggplot(new_dataset, aes(x = SEX, y = SAT)) +
 # geom_boxplot(aes(fill = SEX)) +
  #theme_minimal() +
  #labs(title = "Boxplot of SAT Scores by Sex", x = "Sex", y = "SAT Scores")
```

``` r
#variance by group for variables RES & SAT

#var(new_dataset$RES)

#new_dataset %>%
 # group_by(SEX) %>%
  #summarize(Variance = var(RES))

#var(new_dataset$SAT)

#new_dataset %>%
 # group_by(SEX) %>%
  #summarize(Variance = var(SAT))
```

``` r
#ggplot(new_dataset, aes(x = SEX, y = MOB_EXPECT)) +
 # geom_boxplot(aes(fill = SEX)) +
  #theme_minimal() +
  #labs(title = "MOB_EXPECT by SEX", x = "Sex", y = "MOB_EXPECT")
```

``` r
#correlation <- cor(new_dataset$MOB_EXPECT, new_dataset$SAT, use = "complete.obs")

#print(correlation)
```

``` r
#simple correlation using ALL variables 

new_dataset <- new_dataset %>%
  select(SAT_composite, MOB_EXPECT, RES_composite, SEX)


Corr(new_dataset)
```

    ## Pearson's r and 95% confidence intervals:
    ## ────────────────────────────────────────────────────────────────
    ##                                  r       [95% CI]     p        N
    ## ────────────────────────────────────────────────────────────────
    ## SAT_composite-MOB_EXPECT     -0.22 [-0.24, -0.19] <.001 *** 5635
    ## SAT_composite-RES_composite   0.08 [ 0.06,  0.11] <.001 *** 5635
    ## SAT_composite-SEX            -0.02 [-0.04,  0.01]  .180     5635
    ## MOB_EXPECT-RES_composite      0.04 [ 0.01,  0.07]  .003 **  5635
    ## MOB_EXPECT-SEX                0.10 [ 0.07,  0.12] <.001 *** 5635
    ## RES_composite-SEX             0.04 [ 0.02,  0.07]  .001 **  5635
    ## ────────────────────────────────────────────────────────────────

![](MyProject_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

    ## Correlation matrix is displayed in the RStudio `Plots` Pane.

``` r
ggcorrmat(new_dataset)
```

![](MyProject_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
#Multiple regression

model<-lm(MOB_EXPECT ~ SEX + SAT_composite + RES_composite, data = new_dataset)

check_model(model)
```

![](MyProject_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
model_summary(model)
```

    ## 
    ## Model Summary
    ## 
    ## ─────────────────────────────
    ##                (1) MOB_EXPECT
    ## ─────────────────────────────
    ## (Intercept)       1.536 ***  
    ##                  (0.205)     
    ## SEX               0.319 ***  
    ##                  (0.046)     
    ## SAT_composite    -0.415 ***  
    ##                  (0.024)     
    ## RES_composite     0.257 ***  
    ##                  (0.061)     
    ## ─────────────────────────────
    ## R^2               0.059      
    ## Adj. R^2          0.058      
    ## Num. obs.      5635          
    ## ─────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##           Term  VIF     VIF 95% CI Increased SE Tolerance Tolerance 95% CI
    ##            SEX 1.00 [1.00, 168.05]         1.00      1.00     [0.01, 1.00]
    ##  SAT_composite 1.01 [1.00,   1.24]         1.00      0.99     [0.81, 1.00]
    ##  RES_composite 1.01 [1.00,   1.16]         1.00      0.99     [0.86, 1.00]

``` r
tab_model(model)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
MOB_EXPECT
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.13 – 1.94
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
SEX
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.32
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.23 – 0.41
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
SAT composite
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.41
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.46 – -0.37
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
RES composite
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.14 – 0.38
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
5635
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.059 / 0.058
</td>
</tr>
</table>

``` r
plot_model(model,  type ="est",  show.values = TRUE, vline.color = "#1B191999", line.size = 1.5, dot.size = 2.5, colors = "blue") + theme_bruce()
```

![](MyProject_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
#moderation effect of GENDER on correlation between SAT_composite and MOB_expect


PROCESS(new_dataset, y = "MOB_EXPECT", x = "SAT_composite", mods = c("SEX"))
```

    ## 
    ## ****************** PART 1. Regression Model Summary ******************
    ## 
    ## PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
    ## PROCESS Model Type : Simple Moderation
    ## -    Outcome (Y) : MOB_EXPECT
    ## -  Predictor (X) : SAT_composite
    ## -  Mediators (M) : -
    ## - Moderators (W) : SEX
    ## - Covariates (C) : -
    ## -   HLM Clusters : -
    ## 
    ## All numeric predictors have been grand-mean centered.
    ## (For details, please see the help page of PROCESS.)
    ## 
    ## Formula of Outcome:
    ## -    MOB_EXPECT ~ SAT_composite*SEX
    ## 
    ## CAUTION:
    ##   Fixed effect (coef.) of a predictor involved in an interaction
    ##   denotes its "simple effect/slope" at the other predictor = 0.
    ##   Only when all predictors in an interaction are mean-centered
    ##   can the fixed effect denote the "main effect"!
    ##   
    ## Model Summary
    ## 
    ## ─────────────────────────────────────────────────
    ##                    (1) MOB_EXPECT  (2) MOB_EXPECT
    ## ─────────────────────────────────────────────────
    ## (Intercept)           1.593 ***       1.591 ***  
    ##                      (0.023)         (0.023)     
    ## SAT_composite        -0.409 ***      -0.408 ***  
    ##                      (0.024)         (0.024)     
    ## SEX                                   0.328 ***  
    ##                                      (0.046)     
    ## SAT_composite:SEX                    -0.126 **   
    ##                                      (0.049)     
    ## ─────────────────────────────────────────────────
    ## R^2                   0.047           0.057      
    ## Adj. R^2              0.047           0.056      
    ## Num. obs.          5635            5635          
    ## ─────────────────────────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## ************ PART 2. Mediation/Moderation Effect Estimate ************
    ## 
    ## Package Use : ‘interactions’ (v1.2.0)
    ## Effect Type : Simple Moderation (Model 1)
    ## Sample Size : 5635
    ## Random Seed : -
    ## Simulations : -
    ## 
    ## Interaction Effect on "MOB_EXPECT" (Y)
    ## ────────────────────────────────────────────
    ##                         F df1  df2     p    
    ## ────────────────────────────────────────────
    ## SAT_composite * SEX  6.75   1 5631  .009 ** 
    ## ────────────────────────────────────────────
    ## 
    ## Simple Slopes: "SAT_composite" (X) ==> "MOB_EXPECT" (Y)
    ## ────────────────────────────────────────────────────────
    ##  "SEX" Effect    S.E.       t     p             [95% CI]
    ## ────────────────────────────────────────────────────────
    ##  1.000 -0.343 (0.034) -10.015 <.001 *** [-0.411, -0.276]
    ##  2.000 -0.470 (0.035) -13.609 <.001 *** [-0.537, -0.402]
    ## ────────────────────────────────────────────────────────

``` r
PROCESS(new_dataset, y = "MOB_EXPECT", x = "RES_composite", mods = c("SEX"))
```

    ## 
    ## ****************** PART 1. Regression Model Summary ******************
    ## 
    ## PROCESS Model Code : 1 (Hayes, 2018; www.guilford.com/p/hayes3)
    ## PROCESS Model Type : Simple Moderation
    ## -    Outcome (Y) : MOB_EXPECT
    ## -  Predictor (X) : RES_composite
    ## -  Mediators (M) : -
    ## - Moderators (W) : SEX
    ## - Covariates (C) : -
    ## -   HLM Clusters : -
    ## 
    ## All numeric predictors have been grand-mean centered.
    ## (For details, please see the help page of PROCESS.)
    ## 
    ## Formula of Outcome:
    ## -    MOB_EXPECT ~ RES_composite*SEX
    ## 
    ## CAUTION:
    ##   Fixed effect (coef.) of a predictor involved in an interaction
    ##   denotes its "simple effect/slope" at the other predictor = 0.
    ##   Only when all predictors in an interaction are mean-centered
    ##   can the fixed effect denote the "main effect"!
    ##   
    ## Model Summary
    ## 
    ## ─────────────────────────────────────────────────
    ##                    (1) MOB_EXPECT  (2) MOB_EXPECT
    ## ─────────────────────────────────────────────────
    ## (Intercept)           1.593 ***       1.592 ***  
    ##                      (0.024)         (0.024)     
    ## RES_composite         0.187 **        0.170 **   
    ##                      (0.062)         (0.062)     
    ## SEX                                   0.336 ***  
    ##                                      (0.047)     
    ## RES_composite:SEX                     0.100      
    ##                                      (0.124)     
    ## ─────────────────────────────────────────────────
    ## R^2                   0.002           0.011      
    ## Adj. R^2              0.001           0.010      
    ## Num. obs.          5635            5635          
    ## ─────────────────────────────────────────────────
    ## Note. * p < .05, ** p < .01, *** p < .001.
    ## 
    ## ************ PART 2. Mediation/Moderation Effect Estimate ************
    ## 
    ## Package Use : ‘interactions’ (v1.2.0)
    ## Effect Type : Simple Moderation (Model 1)
    ## Sample Size : 5635
    ## Random Seed : -
    ## Simulations : -
    ## 
    ## Interaction Effect on "MOB_EXPECT" (Y)
    ## ────────────────────────────────────────────
    ##                         F df1  df2     p    
    ## ────────────────────────────────────────────
    ## RES_composite * SEX  0.65   1 5631  .419    
    ## ────────────────────────────────────────────
    ## 
    ## Simple Slopes: "RES_composite" (X) ==> "MOB_EXPECT" (Y)
    ## ─────────────────────────────────────────────────────
    ##  "SEX" Effect    S.E.     t     p            [95% CI]
    ## ─────────────────────────────────────────────────────
    ##  1.000  0.119 (0.087) 1.360  .174     [-0.052, 0.290]
    ##  2.000  0.219 (0.088) 2.482  .013 *   [ 0.046, 0.392]
    ## ─────────────────────────────────────────────────────

``` r
plot<-summarySE(new_dataset, measurevar="MOB_EXPECT", groupvars=c("SEX", "SAT_composite"))


ggplot(plot, aes(x = SAT_composite, y = MOB_EXPECT)) +
  geom_smooth(method = lm) + geom_point() + facet_wrap(~ SEX) + theme_bruce()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](MyProject_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
plot<-summarySE(new_dataset, measurevar="MOB_EXPECT", groupvars=c("SEX", "RES_composite"))
```

    ## Warning in qt(conf.interval/2 + 0.5, datac$N - 1): NaNs produced

``` r
ggplot(plot, aes(x = RES_composite, y = MOB_EXPECT)) +
  geom_smooth(method = lm) + geom_point() + facet_wrap(~ SEX) + theme_bruce()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](MyProject_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->
