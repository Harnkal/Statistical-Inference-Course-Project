# Statistical Inference - Inferential Data Analysis
Silva, RAFAEL  
December 4, 2017  

## Setup



This section presents all the libraries used during the project.


```r
library(datasets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
```

## Introduction

The objective of this project is to make some inferences about the length variation on each supplement and dose on the ToothGrowth data from the R datasets package. The analysis is going to follow the four steps below:

1. Exploratory data analysis and summary;
1. Confidence intervals and hypothesis testing;
1. Conclusions and assumptions.

The chunk bellow is used to read the data set into the workspace.


```r
data("ToothGrowth")
```

For more information on the data set (description, dimensions, variables and source) enter ?ToothGrowth on the console to check its documentation.

## Exploratory analysis

To start the analysis, a brief verification on how the dataset looks like is necessary.


```r
dim(ToothGrowth)
```

```
## [1] 60  3
```


```r
head(ToothGrowth)
```

```
##    len supp dose
## 1  4.2   VC  0.5
## 2 11.5   VC  0.5
## 3  7.3   VC  0.5
## 4  5.8   VC  0.5
## 5  6.4   VC  0.5
## 6 10.0   VC  0.5
```

As the outputs above show, the data set has 60 observations on 3 variables. As the introduction chapter states, the objective is to make some inferences about the length variation on each supplement and dose. The box plot below gives a feeling about how each combination affects the length variable.

The code used to generate the plot is available in the appendix.

<img src="inferDataAnalysis_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

In the table below, the main statistical attributes of each group are summarized.

The code used to generate the table is available in the appendix.


Table: Data Summary by Supplement and Dose

Supp/Dose     Mean   Median   Variance    n
----------  ------  -------  ---------  ---
OJ/0.5       13.23    12.25     19.889   10
VC/0.5        7.98     7.15      7.544   10
OJ/1         22.70    23.45     15.296   10
VC/1         16.77    16.50      6.327   10
OJ/2         26.06    25.95      7.049   10
VC/2         26.14    25.95     23.018   10

With the summary presented above, it is possible to manually calculate the confidence interval (Z or T) for all the groups. Besides that, it is also possible to perform an equivalence test on each combination.

## Confidence intervals and hypothesis testing

In this section, each group will have it's confidence intervals calculated and plotted along with its mean. The confidence intervals are represented by the lines and the means by the dots.

The code used to generate the plot is available in the appendix.

<img src="inferDataAnalysis_files/figure-html/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

Before taking conclusions about what the plot shows, there are 2 questions that the plot create, and the output of the next chunks can clarify them. The tests assume equal variance on the samples.

1. H~0~:$\mu$~OJ/0.5~=$\mu$~VC/1~ | H~a~:$\mu$~OJ/0.5~<$\mu$~VC/1~;


```r
oj05 <- with(ToothGrowth, len[interaction(supp, dose)=="OJ.0.5"])
vc1 <- with(ToothGrowth, len[interaction(supp, dose)=="VC.1"])
t.test(oj05, vc1, alternative = "less", var.equal = TRUE)$p.value
```

```
## [1] 0.02111996
```

2. H~0~:$\mu$~OJ/1~=$\mu$~VC/2~ | H~a~:$\mu$~OJ/1~<$\mu$~VC/2~.


```r
oj1 <- with(ToothGrowth, len[interaction(supp, dose)=="OJ.1"])
vc2 <- with(ToothGrowth, len[interaction(supp, dose)=="VC.2"])
t.test(oj1, vc2, alternative = "less", var.equal = TRUE)$p.value
```

```
## [1] 0.04791856
```

## Conclusion

By looking at the plot on the last section, it is possible to make some assumptions about the data:

1. Reject H~0~ for H~0~:$\mu$~OJ/0.5~=$\mu$~VC/0.5~ | H~a~:$\mu$~OJ/0.5~$\neq\mu$~VC/0.5~ ;
1. Reject H~0~ for H~0~:$\mu$~OJ/1~=$\mu$~VC/1~ | H~a~:$\mu$~OJ/1~$\neq\mu$~VC/1~;
1. Fail to reject H~0~ for H~0~:$\mu$~OJ/2~=$\mu$~VC/2~ | H~a~:$\mu$~OJ/2~$\neq\mu$~VC/2~.

Furthermore, by the results of the t tests it is possible to infer:

1. Reject H~0~ for H~0~:$\mu$~OJ/0.5~=$\mu$~VC/1~ | H~a~:$\mu$~OJ/0.5~<$\mu$~VC/1~;
1. reject H~0~ for H~0~:$\mu$~OJ/1~=$\mu$~VC/2~ | H~a~:$\mu$~OJ/1~<$\mu$~VC/2~.

\pagebreak

## Appendix

The code below was used to generate the plot titled "Length by supplement and dose".


```r
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + 
      geom_boxplot() + 
      labs(title = "Length by supplement and dose", 
           x = "Supplement.Dose", y = "Length")
```

The code below was used to generate the table 1.


```r
summ <- ToothGrowth %>%
      mutate(suppDose = interaction(supp, dose))%>%
      group_by(suppDose) %>%
      summarise(mean = mean(len), 
                median = median(len),
                variance = round(var(len),3), 
                n = length(len))

kable(summ, caption = "Data Summary by Supplement and Dose", 
      col.names = c("Supp/Dose", "Mean", "Median", "Variance", "n"))
```

The code below was used to generate the plot titled "Supplement and dose confidence intervals".


```r
results <- summ %>%
      mutate(lowerCI = mean - qt(.975, n-1) * sqrt(variance/n),
             upperCI = mean + qt(.975, n-1) * sqrt(variance/n)) %>%
      select(c("suppDose", "mean", "lowerCI", "upperCI")) %>%
      gather(lowUP, confInt, c("lowerCI", "upperCI"))

ggplot(results) +
      geom_line(aes(y = suppDose, x = confInt, group = suppDose)) +
      geom_point(aes(y = suppDose, x = mean)) +
      labs(x = "Confidence Interval", y = "Supplement.Dose", 
           title = "Supplement and dose confidence intervals") +
      theme_bw()
```
