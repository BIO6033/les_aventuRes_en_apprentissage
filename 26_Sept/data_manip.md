---
title: "Manip des données"
author: "Andrew"
date: "26/09/2019"
output:
  html_document: 
    keep_md: true
editor_options: 
  chunk_output_type: console
---



Manipulation des données c'est _fun_.


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────
```

```
## ✔ ggplot2 3.2.1.9000     ✔ purrr   0.3.2     
## ✔ tibble  2.1.3          ✔ dplyr   0.8.3     
## ✔ tidyr   0.8.3          ✔ stringr 1.4.0     
## ✔ readr   1.3.1          ✔ forcats 0.4.0
```

```
## ── Conflicts ──────────────────────────
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(gapminder)
```

## regarde les données


```r
gapminder
```

```
## # A tibble: 1,704 x 6
##    country     continent  year lifeExp      pop gdpPercap
##    <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
##  1 Afghanistan Asia       1952    28.8  8425333      779.
##  2 Afghanistan Asia       1957    30.3  9240934      821.
##  3 Afghanistan Asia       1962    32.0 10267083      853.
##  4 Afghanistan Asia       1967    34.0 11537966      836.
##  5 Afghanistan Asia       1972    36.1 13079460      740.
##  6 Afghanistan Asia       1977    38.4 14880372      786.
##  7 Afghanistan Asia       1982    39.9 12881816      978.
##  8 Afghanistan Asia       1987    40.8 13867957      852.
##  9 Afghanistan Asia       1992    41.7 16317921      649.
## 10 Afghanistan Asia       1997    41.8 22227415      635.
## # … with 1,694 more rows
```

## ggplot2


```r
ggplot(gapminder, aes(x = year, y = lifeExp)) + 
  geom_point()
```

![](data_manip_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Aussi possible avec des boxplots


```r
ggplot(gapminder, aes(x = year, y = lifeExp, group = year)) + 
  geom_boxplot() + 
  geom_point()
```

![](data_manip_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

line plot


```r
ggplot(gapminder,
       aes(x = year,
           y = gdpPercap,
           group = country)) + 
  geom_line(colour = "darkgreen",
            size = 3,
            alpha = 0.1)
```

![](data_manip_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

mapping to a group -- couleur par continent!


```r
ggplot(gapminder,
       aes(x = year,
           y = gdpPercap,
           group = country,
           colour = continent)) + 
  geom_line()
```

![](data_manip_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

facets


```r
ggplot(gapminder,
       aes(x = year,
           y = gdpPercap,
           group = country,
           colour = continent)) + 
  geom_line() + 
  facet_wrap(~continent)
```

![](data_manip_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

