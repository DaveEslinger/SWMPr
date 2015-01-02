# SWMPr: An R package for the National Estuarine Research Reserve System
Marcus W. Beck, beck.marcus@epa.gov  

#Overview 

SWMPr is an R package that contains functions for retrieving, organizing, and analyzing estuary monitoring data from the System Wide Monitoring Program ([SWMP](http://nerrs.noaa.gov/RCDefault.aspx?ID=18)).  SWMP was implemented by the National Estuarine Research Reserve System ([NERRS](http://nerrs.noaa.gov/)) in 1995 to provide continuous monitoring data at over 300 stations in 28 estuaries across the United States.  SWMP data are maintained online by the Centralized Data Management Office (CDMO). This R package provides several functions to retrieve, organize, and analyze SWMP data from the CDMO.  Information on the CDMO web services are available [here](http://cdmo.baruch.sc.edu/webservices.cfm).  Your computer's IP address must be registered with the CDMO website to use most of the data retrieval functions, see contact info in the link.  All other functions can be used after obtaining data from the CDMO, as described below. 

The package has many dependencies, the most important being the SSOAP package for retrieving data from the CDMO using a SOAP client interface. All dependencies are handled automatically when SWMPr is installed and loaded, excluding SSOAP.  The SSOAP package is not required to use SWMPr but is necessary for using most of the data retrieval functions.  The SSOAP package is currently removed from CRAN but accessible at [http://www.omegahat.org/SSOAP/](http://www.omegahat.org/SSOAP/).  Functions that require SSOAP should install the package automatically or it can be installed as follows:


```r
install.packages("SSOAP", repos="http://www.omegahat.org/R", dependencies = T,
  type =  "source")
```

All data obtained from the CDMO should be [cited](http://cdmo.baruch.sc.edu/data/citation.cfm) using the format:

National Estuarine Research Reserve System (NERRS). 2012. System-wide Monitoring Program. Data accessed from the NOAA NERRS Centralized Data Management Office website: http://cdmo.baruch.sc.edu/; accessed 12 October 2012.

To cite this package:

Beck MW. 2014. SWMPr: An R package for the National Estuarine Research Reserve System.  Version 1.5.0. https://github.com/fawda123/SWMPr

#Installing the package

This package is currently under development and has not been extensively tested.  The development version can be installed from Github:


```r
install.packages('devtools')
require(devtools)
install_github('fawda123/SWMPr')
require(SWMPr)
```


Note that the current version of devtools (v1.6.1) was built under R version 3.1.1.  The SWMPr package may not install correctly with older versions of R.

#Data retrieval

SWMP data can be used in R after they are obtained directly from the CDMO through an online query or by using the retrieval functions provided in this package.  In the latter case, the IP address for the computer making the request must be registered with CDMO.  This can be done by following instructions [here](http://cdmo.baruch.sc.edu/webservices.cfm).  The [metadata](http://cdmo.baruch.sc.edu/data/metadata.cfm) should also be consulted for available data, including the parameters and date ranges for each monitoring station.  Metadata are included as a .csv file with data requested from the CDMO and can also be obtained using the `site_codes` (all sites) or `site_codes_ind` (individual site) functions.  


```r
# retrieve metadata for all sites
site_codes()

# retrieve metadata for a single site
site_codes_ind('apa')
```

Due to rate limitations on the server, the retrieval functions in this package return a limited number of records.  The functions are more useful for evaluating short time periods, although these functions could be used iteratively (i.e., with `for` or `while` loops) to obtain longer time series.  Data retrieval functions to access the CDMO include `all_params`, `all_params_dtrng`, and `single_param`.  These are functions that call the available methods on the CDMO SOAP interface.  `all_params` returns the most recent 100 records of all parameters at a station, `all_params_dtrng` returns all records within a date range for all parameters or a single parameter, and `single_param` is identical to `all_params` except that a single parameter is requested.    



```r
# all parameters for a station, most recent
all_params('hudscwq')

# get all parameters within a date range
all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013'))

# get single parameter within a date range
all_params_dtrng('hudscwq', c('09/10/2012', '02/8/2013'), param = 'do_mgl')

# single parameter for a station, most recent
single_param('hudscwq', 'do_mgl')
```

For larger requests, it's easier to obtain data outside of R using the CDMO query system and then importing within R using the `import_local` function.  Data can be retrieved from the CDMO several ways.  The `import_local` function is designed for data from the [zip downloads](http://cdmo.baruch.sc.edu/aqs/zips.cfm) feature in the advanced query section of the CDMO. The function may also work using data from the [data export system](http://cdmo.baruch.sc.edu/get/export.cfm), but this feature has not been extensively tested (expect bugs).  The [zip downloads](http://cdmo.baruch.sc.edu/aqs/zips.cfm) feature is an easy way to obtain data from multiple stations in one request.  The downloaded data will be in a compressed folder that includes multiple .csv files by year for a given data type (e.g., apacpwq2002.csv, apacpwq2003.csv, apacpnut2002.csv, etc.).  It is recommended that all stations at a site and the complete date ranges are requested to avoid repeated requests to CDMO.  The `import_local` function can be used after the folder is decompressed.

Occasionally, duplicate time stamps are present in the raw data.  The function handles duplicate entries differently depending on the data type (water quality,  weather, or nutrients).  For water quality and nutrient data, duplicate time stamps are simply removed.  Note that nutrient data often contain replicate samples with similar but not duplicated time stamps within a few minutes of each other.  Replicates with unique time stamps are not removed but can be further processed using `rem_reps`.  Weather data prior to 2007 may contain duplicate time stamps at frequencies for 60 (hourly) and 144 (daily) averages, in addition to 15 minute frequencies.  Duplicate values that correspond to the smallest value in the frequency column (15 minutes) are retained.  


```r
# import data for apaebmet that you downloaded

# this is an example path with the csv files, change as needed
path <- 'C:/my_path/'

# import, do not include file extension
import_local(path, 'apaebmet') 
```

For the following example, the `import_local` function is used to load data included with the SWMPr distribution.  To use it, set the path variable using the `system.file` command shown below (you can see this full path by executing `path` at the command line).  Execute both lines to import the data.


```r
# import data for apaebmet that comes with SWMPr

# this is the path for csv example files
path <- system.file('zip_ex', package = 'SWMPr')

# import, do not include file extension
import_local(path, 'apaebmet') 
```

A character string for the station code that also includes a date can be used as input if a specific year is desired, e.g., 'apaebmet2011'.  In all cases, the imported data need to assigned to an object in the workspace for use with other functions:


```r
# import data and assign to dat
dat <- import_local(path, 'apaebmet', trace = F) 

# view first six rows
head(dat)
```

```
##         datetimestamp atemp f_atemp rh f_rh   bp f_bp wspd f_wspd maxwspd
## 1 2011-01-01 00:00:00  15.4    <0>  94 <0>  1019 <0>   2.6   <0>      3.4
## 2 2011-01-01 00:15:00  15.2    <0>  95 <0>  1019 <0>   2.7   <0>      4.0
## 3 2011-01-01 00:30:00  15.2    <0>  95 <0>  1019 <0>   2.8   <0>      3.5
## 4 2011-01-01 00:45:00  15.3    <0>  95 <0>  1019 <0>   3.1   <0>      4.2
## 5 2011-01-01 01:00:00  15.3    <0>  95 <0>  1018 <0>   3.2   <0>      4.4
## 6 2011-01-01 01:15:00  15.3    <0>  95 <0>  1018 <0>   3.6   <0>      4.9
##   f_maxwspd wdir f_wdir sdwdir f_sdwdir totpar  f_totpar totprcp f_totprcp
## 1      <0>   145   <0>       8     <0>     0.8 <1> (CSM)       0      <0> 
## 2      <0>   146   <0>       7     <0>     0.8 <1> (CSM)       0      <0> 
## 3      <0>   139   <0>       7     <0>     0.8 <1> (CSM)       0      <0> 
## 4      <0>   140   <0>       7     <0>     0.8 <1> (CSM)       0      <0> 
## 5      <0>   144   <0>       6     <0>     0.8 <1> (CSM)       0      <0> 
## 6      <0>   141   <0>       7     <0>     0.8 <1> (CSM)       0      <0> 
##   cumprcp f_cumprcp totsorad f_totsorad
## 1       0      <0>        NA      <-1> 
## 2       0      <0>        NA      <-1> 
## 3       0      <0>        NA      <-1> 
## 4       0      <0>        NA      <-1> 
## 5       0      <0>        NA      <-1> 
## 6       0      <0>        NA      <-1>
```

#The swmpr object class

All data retrieval functions return a swmpr object that includes relevant data and several attributes describing the dataset.  The data include a datetimestamp column in the appropriate timezone for a station.  Note that the datetimestamp is standard time for each timezone and does not include daylight savings. Additional columns include parameters for a given data type (weather, nutrients, or water quality) and correspondingg QAQC columns if returned from the initial data request.  The attributes for a swmpr object include `names` of the dataset, `class` (swmpr) `station name` (7 or 8 characters), `qaqc_cols` (logical), `date_rng` (POSIX vector), `timezone` (text string in country/city format), `stamp_class` (class of datetimestamp vector, POSIX or Date), and `parameters` (character vector).  Attributes of a swmpr object can be viewed as follows:


```r
# verify that dat is swmpr class
class(dat)
```

```
## [1] "swmpr"      "data.frame"
```

```r
# all attributes of dat
names(attributes(dat))
```

```
## [1] "names"       "row.names"   "class"       "station"     "parameters" 
## [6] "qaqc_cols"   "date_rng"    "timezone"    "stamp_class"
```

```r
# a single attribute of dat
attr(dat, 'station')
```

```
## [1] "apaebmet"
```

The swmpr object class was created for use with specific methods and it is suggested that these methods be used for data organization and analysis.  A swmpr object also secondarily inherits methods from the data.frame class, such that common data.frame methods also apply to swmpr objects.  Available methods for the swmpr class are described below and can also be viewed:


```r
# available methods for swmpr class
methods(class = 'swmpr')
```

```
##  [1] aggregate.swmpr    comb.swmpr         decomp.swmpr      
##  [4] decomp_cj.swmpr    hist.swmpr         lines.swmpr       
##  [7] na.approx.swmpr    plot.swmpr         plot_summary.swmpr
## [10] qaqc.swmpr         qaqcchk.swmpr      rem_reps.swmpr    
## [13] setstep.swmpr      smoother.swmpr     subset.swmpr
```

#An overview of methods for swmpr objects

Three categories of functions are available: retrieve, organize, and analyze.  The retrieval functions import the data into R as a swmpr object for use with the organize and analyze functions.  Methods defined for swmpr objects can be applied with the organize and analyze functions.  These methods are available for generic functions specific to this package, in addition to methods for existing generic functions available from other packages.  S3 methods are implemented in all cases.  

The organize functions are used to clean or prepare the data for analysis, including removal of QAQC flags, subsetting, creating a standardized time series vector, and combining data of different types.  The `qaqc` function is a simple screen to retain values from the data with specified QAQC flags, described [here](http://cdmo.baruch.sc.edu/data/qaqc.cfm).  Each parameter in the swmpr data typically has a corresponding QAQC column of the same name with the added prefix 'f_'.  Values in the QAQC column specify a flag from -5 to 5.  Generally, only data with the '0' QAQC flag should be used, which is the default option for the `qaqc` function.  Data that do not satisfy QAQC criteria are converted to NA values.   Additionally, simple filters are used to remove obviously bad values, e.g., wind speed values less than zero or pH values greater than 12. Erroneous data entered as -99 are also removed. Processed data will have QAQC columns removed, in addition to removal of values in the actual parameter columns that do not meet the criteria. 


```r
# qaqc screen for a swmpr object, retain only '0'
qaqc(dat)

# retain all data regardless of flag
qaqc(dat, qaqc_keep = NULL)

# retain only '0' and '-1' flags
qaqc(dat, qaqc_keep = c(0, -1))
```

Viewing the number of observations for each parameter that are assigned to a QAQC flag may be useful for deciding how to process the data with `qaqc`.  The `qaqcchk` function can be used to view this information.  Consult the [online documentation](http://cdmo.baruch.sc.edu/data/qaqc.cfm) for a description of each QAQC flag. 


```r
# view the number observations in each QAQC flag
qaqcchk(dat)
```

Raw nutrient data obtained from the CDMO will usually include replicate samples that were taken within a few minutes of each other.  The `rem_reps.swmpr` function combines nutrient data that occur on the same day to preserve an approximate monthly time step.  The datetimestamp column will always be averaged for replicates, but the actual observations will be combined based on the user-supplied function which defauls to the mean.  Other suggested functions include the `median`, `min`, or `max`.  The entire function call including treatment of NA values should be passed to the `FUN` argument (see the examples).  The function is meant to be used after `qaqc` processing, although it works with a warning if QAQC columns are present.


```r
# get nutrient data
path <- system.file('zip_ex', package = 'SWMPr')
swmp1 <- import_local(path, 'apacpnut')
swmp1 <- qaqc(swmp1)

# remove replicate nutrient data
rem_reps(swmp1)

# use different function to aggregate replicates
func <- function(x) max(x, na.rm = T)
rem_reps(swmp1, FUN = func)
```

A subset method added to the existing `subset` function is available for swmpr objects.  This function is used to subset the data by date and/or a selected parameter.  The date can be a single value or as two dates to select records within the range. The former case requires a binary operator input as a character string passed to the argument, such as `>` or `<`.  The subset argument for the date(s) must also be a character string of the format YYYY-mm-dd HH:MM for each element (i.e., %Y-%m%-%d %H:%M in POSIX standards).  Be aware that an error may be returned using this function if the subset argument is in the correct format but the calendar date does not exist, e.g. `2012-11-31 12:00`.  Finally, the function can be used to remove rows and columns that do not contain data. 


```r
# select two parameters from dat
subset(dat, select = c('rh', 'bp'))

# subset records greater than or equal to a date
subset(dat, subset = '2013-01-01 0:00', operator = '>=')

# subset records within a date range
subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'))

# subset records within a date range, select two parameters
subset(dat, subset = c('2012-07-01 6:00', '2012-08-01 18:15'),
  select = c('atemp', 'totsorad'))

# remove rows/columns that do not contain data
subset(dat, rem_rows = T, rem_cols = T)
```

The `setstep` function formats a swmpr object to a continuous time series at a given time step.  This function is not necessary for most stations but can be useful for combining data or converting an existing time series to a set interval.  The first argument of the function, `timestep`, specifies the desired time step in minutes starting from the nearest hour of the first observation.  The second argument, `differ`, specifies the allowable tolerance in minutes for matching existing observations to user-defined time steps in cases where the two are dissimilar.  Values for `differ` that are greater than one half the value of `timestep` are not allowed to prevent duplication of existing data.  Likewise, the default value for `differ` is one half the time step.  Rows that do not match any existing data within the limits of the `differ` argument are not discarded.  Output from the `setstep` function can be used with `subset` and to create a time series at a set interval with empty data removed.


```r
# convert time series to two hour invervals
# tolerance of +/- 30 minutes for matching existing data
setstep(dat, timestep = 120, differ = 30)

# convert a nutrient time series to a continuous time series
# then remove empty rows and columns
dat_nut <- import_local(path, 'apacpnut')
dat_nut <- setstep(dat_nut, timestep = 60)
subset(dat_nut, rem_rows = T, rem_cols = T)
```

The `comb` function is used to combine multiple swmpr objects into a single object with a continuous time series at a given step.  The `timestep` function is used internally such that `timestep` and `differ` are accepted arguments for `comb`.  The function requires one or more swmpr objects as input as separate, undefined arguments.  The remaining arguments must be called explicitly since an arbitrary number of objects can be used as input.  In general, the function combines data by creating a master time series that is used to iteratively merge all swmpr objects.  The time series for merging depends on the value passed to the `method` argument.  Passing `union` to `method` will create a time series that is continuous starting from the earliest date and the latest date for all input objects.  Passing `intersect` to `method` will create a time series that is continuous from the set of dates that are shared between all input objects.  Finally, a seven or eight character station name passed to `method` will merge all input objects based on a continuous time series for the given station.  The specified station must be present in the input data.  Currently, combining data types from different stations is not possible, excluding weather data which are typically at a single, dedicated station.  


```r
# get nuts, wq, and met data as separate objects for the same station
# note that most sites usually have one weather station
swmp1 <- import_local(path, 'apacpnut')
swmp2 <- import_local(path, 'apacpwq')
swmp3 <- import_local(path, 'apaebmet')

# combine nuts and wq data by union
comb(swmp1, swmp2, method = 'union')

# combine nuts and wq data by intersect
comb(swmp1, swmp3, method = 'intersect')

# combine nuts, wq, and met data by nuts time series, two hour time step
comb(swmp1, swmp2, swmp3, timestep = 120, method = 'apacpnut')
```

The analysis functions range from general purpose tools for time series analysis to more specific functions for working with continuous monitoring data in estuaries.  The latter category includes a limited number of functions that were developed by myself or others.  The general purpose tools are swmpr methods that were developed for existing generic functions in the R base installation or relevant packages.  These functions include swmpr methods for `aggregate`, `filter`, and `approx` to deal with missing or noisy data and more general functions for exploratory data analaysis, such as `plot`, `summary`, and `hist` methods.  Decomposition functions (`decomp` and `decomp_cj`) are provided as relatively simple approaches for decomposing time series into additive or multiplicative components. The analysis functions may or may not return a swmpr object depending on whether further processing with swmpr methods is possible from the output.    

The `aggregate` function aggregates parameter data for a swmpr object by set periods of observation.  This function is most useful for aggregating noisy data to evaluate trends on longer time scales, or to simply reduce the size of a dataset.  Data can be aggregated by years, quarters, months, weeks, days, or hours for a user-defined function, which defaults to the mean.  A swmpr object is returned for the aggregated data, although the datetimestamp vector will be converted to a date object if the aggregation period is a day or longer.  Days are assigned to the date vector if the aggregation period is a week or longer based on the `round` method for IDate objects ([data.table](http://cran.r-project.org/web/packages/data.table/index.html) package).  This approach was used to facilitate plotting using predefined methods for Date and POSIX objects.  Additionally, the method of treating NA values for the aggregation function should be noted since this may greatly affect the quantity of data that are returned (see the example below).  Finally, the default argument for `na.action` is set to `na.pass` for swmpr objects to preserve the time series of the input data.


```r
# combine, qaqc, remove empty columns
dat <- comb(swmp1, swmp2, method = 'union')
dat <- qaqc(dat)
swmpr_in <- subset(dat, rem_cols = T)

# get mean DO by quarters
aggregate(swmpr_in, 'quarters', params = c('do_mgl'))

# get mean DO by quarters, remove NA when calculating means
fun_in <- function(x) mean(x, na.rm = T)
aggregate(swmpr_in, FUN = fun_in, 'quarters', params = c('do_mgl'))
```

Time series can be smoothed to better characterize a signal independent of noise.  Although there are many approaches to smoothing, a moving window average is intuitive and commonly used.  The `smoother` function can be used to smooth parameters in a swmpr object using a specified window size.  This method is a simple wrapper to `filter`.  The `window` argument specifies the number of observations included in the moving average.  The `sides` argument specifies how the average is calculated for each observation (see the documentation for `filter`).  A value of 1 will filter observations within the window that are previous to the current observation, whereas a value of 2 will filter all observations within the window centered at zero lag from the current observation. As before, the `params` argument specifies which parameters to smooth.


```r
# import data
swmp1 <- import_local(path, 'apadbwq')

# qaqc and subset imported data
dat <- qaqc(swmp1)
dat <- subset(dat, subset = c('2012-07-09 00:00', '2012-07-24 00:00'))

# filter
test <- smoother(dat, window = 50, params = 'do_mgl')

# plot to see the difference
plot(do_mgl ~ datetimestamp, data = dat, type = 'l')
lines(test, select = 'do_mgl', col = 'red', lwd = 2)
```

![plot of chunk unnamed-chunk-18](./README_files/figure-html/unnamed-chunk-18.png) 

A common issue with any statistical analysis is the treatment of missing values.  Missing data can be excluded from the analysis, included but treated as true zeroes, or interpolated based on similar values.  In either case, an analyst should have a strong rationale for the chosen method.  A common approach used to handle missing data in time series analysis is linear interpolation.  A simple curve fitting method is used to create a continuous set of records between observations separated by missing data.  A challenge with linear interpolation is an appropriate gap size for fitting missing observations.  The ability of the interpolated data to approximate actual trends is a function of the gap size.  Interpolation between larger gaps are less likely to resemble patterns of an actual parameter, whereas interpolation between smaller gaps are more likely to resemble actual patterns.  An appropriate gap size limit depends on the unique characteristics of specific datasets or parameters.  The `na.approx` function can be used to interpolate gaps in a swmpr object.  A required argument for the function is `maxgap` which defines the maximum gap size  for interpolation.


```r
# get data
swmp1 <- import_local(path, 'apadbwq')

# qaqc and subset imported data
dat <- qaqc(swmp1)
dat <- subset(dat, subset = c('2013-01-22 00:00', '2013-01-26 00:00'))

# interpolate, maxgap of 10 records
test <- na.approx(dat, params = 'do_mgl', maxgap = 10)

# interpolate maxgap of 30 records
test2 <- na.approx(dat, params = 'do_mgl', maxgap = 30)

# plot for comparison
par(mfrow = c(3, 1))
plot(do_mgl ~ datetimestamp, dat, main = 'Raw', type = 'l')
plot(do_mgl ~ datetimestamp, test, col = 'red', 
  main = 'Interpolation - maximum gap of 10 records', type = 'l')
lines(dat, select = 'do_mgl')
plot(do_mgl ~ datetimestamp, test2, col = 'red', 
  main = 'Interpolation - maximum gap of 30 records', type = 'l')
lines(dat, select = 'do_mgl')
```

![plot of chunk unnamed-chunk-19](./README_files/figure-html/unnamed-chunk-19.png) 

The `decomp` function is a simple wrapper to `decompose` that separates a time series into additive or multiplicative components describing a trend, cyclical variation (e.g., daily or seasonal), and the remainder.  The additive decomposition assumes that the cyclical component of the time series is stationary (i.e., the variance is constant), whereas a multiplicative decomposition accounts for non-stationarity.  By default, a moving average with a symmetric window is used to filter the seasonal component.  Alternatively, a vector of filter coefficients in reverse time order can be supplied (see the help documentation for `decompose`).  

The `decompose` function requires a ts object with a specified frequency as input.  The `decomp` function converts the input swmpr vector to a ts object prior to `decompose`.  This requires an explicit input defining the frequency of the parameter in the time series.  For example, the frequency of a parameter with diurnal periodicity would be 96 if the time step is 15 minutes (4 * 24).  The frequency of a parameter with seasonal periodicity would be 35040 (4 * 24 * 365).  For simplicity, character strings of `'daily'` or `'seasonal'` can be supplied in place of numeric values.  A starting value of the time series must be supplied in the latter case.  Use of the `setstep` function is also required to standardize the time step prior to decomposition.  

Note that the `decompose` function is a relatively simple approach and alternative methods should be investigated if a more sophisticated decomposition is desired.


```r
# get data
swmp1 <- import_local(path, 'apadbwq')

# subset for daily decomposition
dat <- subset(swmp1, subset = c('2013-07-01 00:00', '2013-07-31 00:00'))

# decomposition and plot
test <- decomp(dat, param = 'do_mgl', frequency = 'daily')
plot(test)
```

![plot of chunk unnamed-chunk-20](./README_files/figure-html/unnamed-chunk-20.png) 

The next example illustrates how to handle missing values using the `decomp` function. The `decompose` function used internally within `decomp` currently cannot process time series with missing values.  A recommended approach is to use `na.approx` to interpolate the missing values prior to `decompose`.


```r
# get data
dat <- subset(swmp1, subset = c('2013-06-01 00:00', '2013-07-31 00:00'))

# this returns an error
# test <- decomp(dat, param = 'do_mgl', frequency = 'daily')

# how many missing values?
sum(is.na(dat$do_mgl))
```

```
## [1] 3
```

```r
# use na.approx to interpolate missing data
dat <- na.approx(dat, params = 'do_mgl', maxgap = 10)

# decomposition and plot
test <- decomp(dat, param = 'do_mgl', frequency = 'daily')
plot(test)
```

![plot of chunk unnamed-chunk-21](./README_files/figure-html/unnamed-chunk-21.png) 

An alternative approach to time series decomposition is provided by the `decomp_cj` function, which is a simple wrapper to the `decompTs` function in the wq package.  Theory describing this method is described in Cloern and Jassby (2010).  The function is similar to `decomp.swmpr` with a few key differences.  The `decomp.swmpr` function decomposes the time series into a trend, seasonal, and random component, whereas the current function decomposes into the grandmean, annual, seasonal, and events components.  For both functions, the random or events components, respectively, can be considered anomalies that don't follow the trends in the remaining categories.  The `decomp_cj` function provides only a monthly decomposition, which is appropriate for characterizing relatively long-term trends.  This approach is meant for nutrient data that are obtained on a monthly cycle.  The function will also work with continuous water quality or weather data but note that the data are first aggregated on the monthly scale before decomposition.  Accordingly, short-term variation less than one-month will be removed. Additional arguments passed to `decompTs` can be used with `decomp_cj`, such as `startyr`, `endyr`, and `type`.  Values passed to `type` are `mult` (default) or `add`, referring to multiplicative or additive decomposition.  See the documentation for `decompTs` for additional explanation and examples.   


```r
# get data
path <- system.file('zip_ex', package = 'SWMPr')
dat <- import_local(path, 'apacpnut')
dat <- qaqc(dat, qaqc_keep = NULL)

# decomposition of chl, ggplot
decomp_cj(dat, param = 'chla_n')
```

![plot of chunk unnamed-chunk-22](./README_files/figure-html/unnamed-chunk-221.png) 

```r
# monthly decomposition of continuous data
dat2 <- import_local(path, 'apacpwq')
dat2 <- qaqc(dat2)

decomp_cj(dat2, param = 'do_mgl')
```

![plot of chunk unnamed-chunk-22](./README_files/figure-html/unnamed-chunk-222.png) 

A reserve map with all stations can be obtained using the `map_reserve` function.  This function is a simple wrapper to functions in the ggmap package. The current function is limited to Google maps, which allows four map types that can be set with the `map_type` argument: terrain (default), satellite, roadmap, or hybrid.  The `zoom` argument may have to be chosen through trial and error depending on the spatial extent of the reserve.  See the help documentation for ggmap for more info on zoom.  Additionally, station locations are returned using the `site_codes_ind` function if the computer making the request has the IP address registered with CDMO. Otherwise, a local and possibly outdated file is used.  Use the contact at the CDMO [web services](http://cdmo.baruch.sc.edu/webservices.cfm) to register your IP.


```r
# plot the stations at Jacques Cousteau reserve
map_reserve('jac')
```

![plot of chunk unnamed-chunk-23](./README_files/figure-html/unnamed-chunk-231.png) 

```r
# plot the stations at Padilla Bay reserve, satellite
map_reserve('pdb', map_type = 'satellite', zoom = 12)
```

![plot of chunk unnamed-chunk-23](./README_files/figure-html/unnamed-chunk-232.png) 

Finally, several graphics showing seasonal and annual trends for a given SWMP parameter can be obtained using the `plot_summary` function.  The plots include monthly distributions, monthly anomalies, and annual anomalies in multiple formats.  Anomalies are defined as the difference between the monthly or annual average from the grand mean for the parameter.  Monthly anomalies are in relation to the grand mean for the same month across all years.  All data are aggregated for quicker plotting.  Nutrient data are based on monthly averages, whereas weather and water quality data are based on daily averages.  Cumulative precipitation data are based on the daily maximum. The function returns a graphics object (Grob) of multiple ggplot objects.  An interactive Shiny widget that uses this function is available: [https://beckmw.shinyapps.io/swmp_summary/](https://beckmw.shinyapps.io/swmp_summary/).


```r
## import data
path <- system.file('zip_ex', package = 'SWMPr')
dat <- import_local(path, 'apacpwq')
dat <- qaqc(dat)

## plot
plot_summary(dat, param = 'temp')
```

![plot of chunk unnamed-chunk-24](./README_files/figure-html/unnamed-chunk-24.png) 

#Functions

See help documentation for more details on each function (e.g., `?all_params`).

##Retrieve

`all_params` Retrieve up to 100 records starting with the most recent at a given station, all parameters.  Wrapper to `exportAllParamsXMLNew` function on web services. 

`all_params_dtrng` Retrieve records of all parameters within a given date range for a station.  Optional argument for a single parameter.  Maximum of 1000 records. Wrapper to `exportAllParamsDateRangeXMLNew`.

`single_param` Retrieve up to 100 records for a single parameter starting with the most recent at a given station.  Wrapper to `exportSingleParamXMLNew` function on web services. 

`import_local` Import files from a local path.  The files must be in a specific format, specifically those returned from the CDMO using the [zip downloads](http://cdmo.baruch.sc.edu/aqs/zips.cfm) option for a reserve.

##Organize

`qaqc.swmpr` Remove QAQC columns and remove data based on QAQC flag values for a swmpr object.  Only applies if QAQC columns are present.  

`qaqcchk.swmpr` View a summary of the number of observations in a swmpr object that are assigned to different QAQC flags used by CDMO.  The output is used to inform further processing but is not used explicitly. 

`rem_reps.swmpr` Remove replicate nutrient data that occur on the same day.  The default is to average replicates.

`subset.swmpr` Subset by dates and/or columns for a swmpr object.  This is a method passed to the generic `subset' function provided in the base package.

`setstep.swmpr` Format data from a swmpr object to a continuous time series at a given timestep.  The function is used in `comb.swmpr` and can also be used with individual stations.

`comb.swmpr` Combines swmpr objects to a common time series using setstep, such as combining the weather, nutrients, and water quality data for a single station. Only different data types can be combined.

##Analyze

`aggregate.swmpr` Aggregate swmpr objects for different time periods - years, quarters, months,  weeks, days, or hours.  Aggregation function is user-supplied but defaults to mean. 

`smoother.swmpr` Smooth swmpr objects with a moving window average.  Window size and sides can be specified, passed to `filter`.

`na.approx.swmpr` Linearly interpolate missing data (`NA` values) in a swmpr object. The maximum gap size that is interpolated is defined as a maximum number of records with missing data. 

`plot.swmpr` Plot a univariate  time series for a swmpr object.  The parameter name must be specified.

`lines.swmpr` Add lines to an existing swmpr plot.

`hist.swmpr` Plot a histogram for a swmpr object.

`decomp.swmpr` Decompose a swmpr time series into trend, seasonal, and residual components.  This is a simple wrapper to `decompose`.  Decomposition of monthly or daily trends is possible.

`decomp_cj.swmpr` Decompose a swmpr time series into grandmean, annual, seasonal, and events components.  This is a simple wrapper to `decompTs` in the wq package.  Only monthly decomposition is possible.

`plot_summary` Create summary plots of seasonal/annual trends and anomalies for a water quality or weather parameter.

##Miscellaneous

`swmpr` Creates object of swmpr class, used internally in retrieval functions.

`parser` Parses html returned from CDMO web services, used internally in retrieval functions.

`time_vec` Converts time vectors to POSIX objects with correct time zone for a site/station, used internally in retrieval functions.

`site_codes` Metadata for all stations, wrapper to `exportStationCodesXMLNew` function on web services.

`site_codes_ind` Metadata for all stations at a single site, wrapper  to `NERRFilterStationCodesXMLNew` function on web services.

`param_names` Returns column names as a list for the parameter type(s) (nutrients, weather, or water quality).  Includes QAQC columns with 'f_' prefix. Used internally in other functions.

`map_reserve` Create a map of all stations in a reserve using the ggmap package.

# Forthcoming

Analysis functions... metab

DOI/release info (see [here](http://computationalproteomic.blogspot.com/2014/08/making-your-code-citable.html))

