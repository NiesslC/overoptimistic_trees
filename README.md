
<!-- README.md is generated from README.Rmd. Please edit that file -->

# overoptimistic_trees

<!-- badges: start -->
<!-- badges: end -->

The goal of overoptimistic_trees is to …

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->

``` r

sessionInfo()
#> R version 4.2.2 Patched (2022-11-10 r83330)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Debian GNU/Linux 12 (bookworm)
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.21.so
#> 
#> locale:
#>  [1] LC_CTYPE=de_DE.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=de_DE.UTF-8    
#>  [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=de_DE.UTF-8   
#>  [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats4    grid      stats     graphics  grDevices utils     datasets 
#> [8] methods   base     
#> 
#> other attached packages:
#>  [1] nlme_3.1-162        party_1.3-11        strucchange_1.5-3  
#>  [4] sandwich_3.0-2      zoo_1.8-11          modeltools_0.2-23  
#>  [7] mvtnorm_1.1-3       R6_2.5.1            mlr3tuning_0.16.0  
#> [10] paradox_0.10.0      mlr3misc_0.11.0     mlr3pipelines_0.4.2
#> [13] mlr3_0.14.1         purrr_0.3.4         forcats_0.5.1      
#> [16] tidyr_1.2.0         dplyr_1.1.3        
#> 
#> loaded via a namespace (and not attached):
#>  [1] coin_1.4-2           tidyselect_1.2.0     xfun_0.31           
#>  [4] listenv_0.9.0        splines_4.2.2        lattice_0.20-45     
#>  [7] vctrs_0.6.4          generics_0.1.2       htmltools_0.5.5     
#> [10] bbotk_0.7.2          yaml_2.3.5           survival_3.5-3      
#> [13] utf8_1.2.3           rlang_1.1.1          pillar_1.9.0        
#> [16] glue_1.6.2           withr_2.5.0          palmerpenguins_0.1.1
#> [19] uuid_1.1-0           multcomp_1.4-22      matrixStats_1.0.0   
#> [22] lifecycle_1.0.3      stringr_1.5.0        future_1.33.0       
#> [25] codetools_0.2-19     evaluate_0.15        knitr_1.39          
#> [28] fastmap_1.1.1        parallel_4.2.2       fansi_1.0.4         
#> [31] TH.data_1.1-1        backports_1.4.1      checkmate_2.1.0     
#> [34] parallelly_1.34.0    digest_0.6.31        stringi_1.7.12      
#> [37] cli_3.6.1            tools_4.2.2          magrittr_2.0.3      
#> [40] tibble_3.2.1         crayon_1.5.2         pkgconfig_2.0.3     
#> [43] MASS_7.3-58.2        libcoin_1.0-9        Matrix_1.5-3        
#> [46] data.table_1.14.2    rmarkdown_2.14       rstudioapi_0.13     
#> [49] lgr_0.4.4            globals_0.16.2       compiler_4.2.2
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub. -->
