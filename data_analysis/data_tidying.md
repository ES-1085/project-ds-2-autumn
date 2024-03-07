Data Tidying
================
Asher Panikian and Autumn Pauly

# Loading Data

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
#base_dir="/Users/pauly/Documents/GitHub/project-ds-2-autumn/project-ds-2-autumn/data/"

seot = read.csv("/cloud/project/data/seot_morphometricsReproStatus_ak_monson.csv", h = T)
```

# Glimpsing Data

``` r
glimpse(seot)
```

    ## Rows: 4,478
    ## Columns: 49
    ## $ OTTER.NO                      <chr> "BDM-128", "BDM-129", "KWK-55-1", "KWK-5…
    ## $ recap                         <int> 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0…
    ## $ DATE                          <chr> "1947-05-02", "1947-05-02", "1955-04-04"…
    ## $ YEAR                          <int> 1947, 1947, 1955, 1955, 1955, 1955, 1955…
    ## $ LOCATION                      <chr> "simenof", "simenof", "amchitka", "amchi…
    ## $ STATE                         <chr> "Alaska", "Alaska", "Alaska", "Alaska", …
    ## $ REGION                        <chr> "AK peninsula", "AK peninsula", "west al…
    ## $ AREA                          <chr> "WAP", "WAP", "rats", "rats", "rats", "r…
    ## $ LAT                           <dbl> 54.92272, 54.92272, 51.49043, 51.49043, …
    ## $ LONG                          <dbl> -159.2937, -159.2937, 179.0461, 179.0461…
    ## $ SEX                           <chr> "M", "F", "F", "F", "M", "F", "F", "F", …
    ## $ WEIGHT                        <dbl> 34.5, 14.5, 9.5, 7.3, 11.3, 12.7, 11.3, …
    ## $ TAIL_LGTH_1                   <dbl> 36.0, 25.2, -9.0, -9.0, -9.0, -9.0, -9.0…
    ## $ TAIL_LGTH_2                   <chr> "-9", "-9", "-9", "-9", "-9", "-9", "-9"…
    ## $ TAIL_LGTH_3                   <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ mean_tail_lgth                <dbl> 36, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ LGTH1                         <dbl> 147.8, 104.7, -9.0, -9.0, -9.0, -9.0, -9…
    ## $ LGTH2                         <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ LGTH3                         <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ curvilinear_correction        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    ## $ mean_lgth                     <dbl> 147.8, 104.7, -9.0, -9.0, -9.0, -9.0, -9…
    ## $ true_standard_lgth            <dbl> 147.8, 104.7, -9.0, -9.0, -9.0, -9.0, -9…
    ## $ body_lgth                     <dbl> 111.8, -9.0, -9.0, -9.0, -9.0, -9.0, -9.…
    ## $ CURVE_LGTH1                   <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ CURVE_LGTH2                   <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ GIRTH1                        <dbl> 95.0, 65.5, -9.0, -9.0, -9.0, -9.0, -9.0…
    ## $ GIRTH2                        <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ mean_girth                    <dbl> 95.0, 65.5, -9.0, -9.0, -9.0, -9.0, -9.0…
    ## $ PAW                           <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ W_PUP                         <chr> ".", "N", "U", "D", ".", "U", "U", "U", …
    ## $ PUP_NUMBER                    <chr> ".", ".", ".", ".", ".", ".", ".", ".", …
    ## $ PUP_SEX                       <chr> ".", ".", ".", ".", ".", ".", ".", ".", …
    ## $ PUP_WGHT                      <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ PUP_LGTH                      <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ PUP_CURVLGTH                  <int> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ FETUS_PRES                    <chr> "N", ".", ".", ".", ".", ".", ".", ".", …
    ## $ FETUS_NUM                     <int> 0, -9, -9, -9, -9, -9, -9, -9, -9, -9, -…
    ## $ FETUS_SEX                     <chr> ".", ".", ".", ".", ".", ".", ".", ".", …
    ## $ FETUS_WT                      <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ FETUS_LTH                     <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ FE_REP_CON                    <chr> ".", ".", ".", ".", ".", ".", ".", ".", …
    ## $ FE_REP_STA                    <chr> ".", ".", ".", ".", ".", ".", ".", ".", …
    ## $ PREGNANCY_STATUS              <chr> ".", "NP", "NP", "NP", ".", "NP", "NP", …
    ## $ CAN_DIA                       <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ FINAL_AGE                     <int> -9, 0, 0, 0, 0, 0, 0, 0, 0, -9, -9, -9, …
    ## $ AGE_CATEGORY                  <dbl> 7, 0, 0, 0, 0, 0, 0, 0, 0, 7, 7, 7, 7, 7…
    ## $ BACULA_LGTH                   <dbl> -9, -9, -9, -9, -9, -9, -9, -9, -9, -9, …
    ## $ comments                      <chr> ".", ".", ".", ".", ".", ".", ".", ".", …
    ## $ Cause_of_death_Capture_method <int> 1, 1, 0, 4, 0, 0, 0, 4, 0, 0, 0, 0, 4, 0…

A lot of the variables that are included in this dataset will not be
used for analysis - a variety of these variables are used to determine
the value of another variable (i.e. how `LGTH_1` is used to determine
\``body_lgth`) and as such we will not need them in our analysis. We
need to then select out which variables we will want to keep.

``` r
#selecting the variables that we want to keep
seot <- seot %>% 
  select(OTTER.NO, recap, DATE, YEAR, LOCATION, STATE, REGION, AREA, LAT, LONG, SEX, WEIGHT, true_standard_lgth, mean_tail_lgth, body_lgth, mean_girth, PAW, W_PUP, PUP_NUMBER, PUP_SEX, PUP_WGHT, PUP_LGTH, PUP_CURVLGTH, FETUS_PRES, FETUS_NUM, FETUS_SEX, FETUS_WT, FETUS_LTH, FE_REP_CON, FE_REP_STA, PREGNANCY_STATUS, CAN_DIA, AGE_CATEGORY, FINAL_AGE, BACULA_LGTH, Cause_of_death_Capture_method)
```

# Spatial Naming

### Changing Regional Names

A number of the regional names within this dataset are shortened, as you
can see below.

``` r
unique(seot$REGION)
```

    ## [1] "AK peninsula"     "west aleutians"   "kodiak"           "PWS"             
    ## [5] "SE"               "east aleutians"   "lower cook inlet"

We would like to rename these values so that the names are more
intuitive yet still concise. This will help us with our data analysis
and understanding, as neither of the project members are from Alaska nor
are we familiar with what the areas the shortened names are referring
to.

``` r
#changing region variable to factor
seot <- seot %>% 
  mutate(REGION = as.factor(REGION))

#changing regional names
seot <- seot %>% 
  mutate(REGION = recode(REGION, 
                         "AK peninsula" = "alaskan_peninsula", 
                         "PWS" = "prince_william_sound", 
                         "SE" = "southeast_alaska", 
                         "east aleutians" = "east_aleutians", 
                         "lower cook inlet" = "lower_cook_inlet", 
                         "west aleutians" = "west_aleutians"))
```

As we look at the number of observations per region, we can find that
there is a large difference between the `west_aleutians` and the
`east_aleutians`. This may prove to be an issue with spatial analysis,
but further investigation into the locality of the otter capture will
determine if any action is needed on the drastic difference in
observations per region.

``` r
seot %>% 
  count(REGION) %>% 
  arrange(desc(n))
```

    ##                 REGION    n
    ## 1       west_aleutians 3014
    ## 2 prince_william_sound  985
    ## 3     southeast_alaska  174
    ## 4               kodiak  148
    ## 5    alaskan_peninsula   81
    ## 6     lower_cook_inlet   75
    ## 7       east_aleutians    1

### Changing Area Names

As was with the regional names, the area names are also shortned for
ease of data entry. We will be changing these names to increase our
subjective, intuitive analysis of this dataset.

``` r
#changing area variable to factor
seot <- seot %>% 
  mutate(AREA = as.factor(AREA))

#changing area names
seot <- seot %>% 
  mutate(AREA = recode(AREA, 
                         "CSE" = "central_southeast_alaska" , 
                          "EAP" = "eastern_alaskan_peninsula", 
                         "EPWS" = "eastern_prince_william_sound", 
                          "NSE" = "northern_southeast_alaska", 
                          "SSE" = "southern_southeast_alaska", 
                          "WAP" = "western_alaskan_peninsula", 
                          "WPWS" = "western_prince_william_sound", 
                          "fox islands" =  "fox_islands", 
                          "near islands" = "near_islands", 
                          "afognak" = "afognak_island", 
                           "andreanofs" = "andreanof_islands", 
                         "delarofs" = "delarof_islands" , 
                         "kachemak" = "kachemak_bay", 
                          "kodiak" = "kodiak_island" , 
                          "rats" = "rat_islands" , 
                          "shuyak" = "shuyak_island_kodiak" ))
```

Much like the regional observations, there is a large difference between
the number of observations per specific location. This will also need to
be assessed to see if any further action will be needed for spatial
analysis.

``` r
seot %>% 
  count(AREA) %>% 
  arrange(desc(n))
```

    ##                            AREA    n
    ## 1             andreanof_islands 1584
    ## 2                   rat_islands 1254
    ## 3  western_prince_william_sound  778
    ## 4  eastern_prince_william_sound  207
    ## 5               delarof_islands  144
    ## 6                afognak_island  113
    ## 7     northern_southeast_alaska  107
    ## 8                  kachemak_bay   75
    ## 9     western_alaskan_peninsula   51
    ## 10    southern_southeast_alaska   47
    ## 11                 near_islands   32
    ## 12    eastern_alaskan_peninsula   30
    ## 13                kodiak_island   30
    ## 14     central_southeast_alaska   20
    ## 15         shuyak_island_kodiak    5
    ## 16                  fox_islands    1

# Cleaning up NA values

As shown in the codebook, the NA values found within this dataset are
usually a negative integer. Let’s change this so that the values are
truly NA.

``` r
seot <- seot %>% 
#weight of otter
  mutate(WEIGHT = if_else(WEIGHT < 0, NA, WEIGHT)) %>% 
#true standard length of total body
  mutate(true_standard_lgth = if_else(true_standard_lgth < 0, NA, true_standard_lgth)) %>%
#body length (true standard length - tail length)
  mutate(body_lgth = if_else(body_lgth < 0, NA, body_lgth)) %>%
#mean girth of body
  mutate(mean_girth = if_else(mean_girth < 0, NA, mean_girth)) %>%
#paw length  
  mutate(PAW = if_else(PAW < 0, NA, PAW)) %>%
#canine diameter
  mutate(CAN_DIA = if_else(CAN_DIA < 0, NA, CAN_DIA)) %>%
#final age
  mutate(FINAL_AGE = if_else(FINAL_AGE < 0, NA, FINAL_AGE)) %>%
#bacula length
  mutate(BACULA_LGTH = if_else(BACULA_LGTH < 0, NA, BACULA_LGTH)) %>% 
  #mean tail length
 mutate(mean_tail_lgth = if_else(mean_tail_lgth < 0, NA, mean_tail_lgth)) %>% 
#sex of otters
mutate(SEX = if_else(SEX == "U", NA, SEX))
```

# Writing CSV

Now, we should write this new tidied dataset as a CSV so that it can be
read across all the .Rmd files.

``` r
write_csv(seot, "/cloud/project/data/seot.csv")
```
