Data Tidying
================
Autumn Pauly

\#Loading Data

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
glimpse(seot$REGION)
```

    ##  chr [1:4478] "AK peninsula" "AK peninsula" "west aleutians" ...

# Changing Regional Names

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

# Changing Area Names

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

\#Writing CSV

Now, we should write this new tidied dataset as a CSV so that it can be
read across all the .Rmd files.

``` r
write_csv(seot, "/cloud/project/data/seot.csv")
```
