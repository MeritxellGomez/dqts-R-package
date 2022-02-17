# Introduction 

Data Quality metrics are functions to associate a given time series with numerical scores to quantify the quality of that data. These values range from 0 to 1 where 0 is the worst quality and 1 is the higher quality. The metrics are: 

- Formats: Proportion of variables with the correct format (integer, date, categorical, ...)
- Names: Proportion of variables well labeled.
- Time Uniqueness: Proportion of unique values in the time or date variable.
- Timeliness: Proportion of values captured in the appropriate time interval, that is, without exceeding the allowed waiting time. 
- Range: Proportion of values within the lower and upper bands that can be provided or simulated. 
- Consistency: Proportion of values within the 80% confidence interval
- Typicality: Proportion of values within the 95% confidence interval
- Moderation: Proportion of values within the 99% confidence interval
- Completeness: Proportion of non-missing values.
- Completeness by variables: Proportion of non-missing variables.
- Completeness by observations: Proportion of non-missing observations.

The global value of quality is a combination of all of those 11 metric values. 

# Getting Started

1.	Installation process: to install the dqts package the following code must be executed using the devtools package

```{r}
#devtools::install_github('blabla')
library(dqts)
```

2.	Dependencies: 
- You must ensure that the following R libraries are installed on your computer: here, ggplot2, magrittr, dplyr, reshape2
- This R package was built using a 3.6.3 R version.

3. Main functions:

- DQ() is used to calculate the data quality metric values.
- dqplot() to show graphically the quality returned by DQ function.
- deepDQ() provides specific information about quality problems regarding a certain metric.
- handleDQ() returns a data set with the specified metric corrected by the request method. 

# Build and Test

In data folder you can find three simulated data files to test the available functions (normalData.Rdata, salests.RData and weatherdf.RData). Two of them have also their corresponding range data files (salesRange.RData and weatherRange.RData). Also, you can test them with your own data frames or time series data. 

You can execute the following code tih different purproses:  

- to know the overall DQ of weatherdf.RData
```{r warning=FALSE}
overall_dq <- DQ(data = weatherdf, var_time_name = 'date', maxdif = 1, units = 'days', ranges = weatherRange)
overall_dq
```

- to know the DQ of weatherdf.RData by windows. You should change initialWindow, skip and fixedWindow to execute different sizes of windows. 
```{r warning=FALSE}
window_dq <- DQ(data = weatherdf, var_time_name = 'date', maxdif = 1, units = 'days', ranges = weatherRange, windows = TRUE, initialWindow = 50, skip = 10, fixedWindow = FALSE)
window_dq
```

- to plot the results of DQ function. You can use the logical arguments normal to decide if normality metrics (Consistency, Typicality and Moderation) are shown in the graph and totalquality to show the overall DQ metric.
```{r warning=FALSE}
dqplot(overall_dq, totalquality = TRUE, normal = TRUE)
dqplot(window_dq, totalquality = FALSE, normal = FALSE)
```

- to in-depth inspection of quality issues related to those metrics that not achieve the perfect score. The following example shows the problem with Time Uniqueness metric. You should change the argument metric to inspect different metrics. 
```{r}
deepDQ(data = weatherdf, metric = 'TimeUniqueness', var_time_name = 'date', position = TRUE)
```

- to solve issues with a certain metric. Then, you can check that the quality is higher. 
```{r warning=FALSE}
new_weatherdf <- handleDQ(data = weatherdf, metric = 'TimeUniqueness', var_time_name = 'date', method = 'deletion')
DQ(data =new_weatherdf, var_time_name = 'date', maxdif = 1, units = 'days', ranges = weatherRange)
```

