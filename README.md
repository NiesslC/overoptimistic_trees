
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Beyond algorithm hyperparameters: on preprocessing hyperparameters and associated pitfalls in machine learning applications

<!-- badges: start -->
<!-- badges: end -->

This repository contains code and results of the manuscript “Beyond
algorithm hyperparameters: on preprocessing hyperparameters and
associated pitfalls in machine learning applications” by Christina
Sauer, Anne-Laure Boulesteix, Luzia Hanßum, Farina Hodiamont, Claudia
Bausewein, and Theresa Ullmann (<https://arxiv.org/abs/2412.03491>).

## File description

### `/01_data`

This folder contains the data set `data_phaselevel_toy.RData`, a
simulated version of the COMPANION data set generated from overall
summary statistics. Except for some auxiliary variables, these
statistics are also reported in the manuscript. While this toy data set
does not include the actual data used in the analysis, it enables the
provided code to run.

### `/02_code`

This folder contains the code used to generate all results shown in the
manuscript. To run the experiment, only the files containing “script” in
their file name need to be run in numerical order (e.g., 01_script\_
before 02_script\_). The script files serve the following purposes:

- `01_script_preprocessing_fixed.R`:  
  Loads the COMPANION data set and performs preprocessing steps
  described in Supplementary Section B.2.1 of the manuscript.

- `02_script_run_simulation.R`:  
  Runs the experiment described in Section 5.2 of the manuscript.

- `03_script_process_results.R`:  
  Processes simulation results into a unified data set and saves it as
  `03_results/rdata/_resdf.RData`.

- `04_script_analysis_results.R`:  
  Creates Figure 5 (main manuscript) and other figures shown in the
  supplement.

- `02b_script_descriptives_companion.R`:  
  Generates descriptive statistics for the COMPANION data set.  
  *Note*: This file can be executed directly after
  `01_script_preprocessing_fixed.R`, hence the numbering `02b`.

The remaining files in the folder serve the following purposes:

- `_fcts_optim.R`, `_fcts_resampling.R`:  
  Functions for executing the experiment (see in-file function
  descriptions for details).

- `_fcts_preproc.R`:  
  Functions specifying the different preprocessing options.

- `_src_add_mlrlearners.R`:  
  Adds learning algorithms of the experiment to the `mlr3` framework.

- `_src_add_mlrpipeops.R`:  
  Integrates preprocessing steps of the experiment into the `mlr3`
  framework.

- `_src_setup_learners.R`:  
  Configures learning algorithms for the experiment (e.g., search space
  for HP tuning).

### `/03_results`

This folder contains three subfolders: `/plots`, `/rdata`, and `/tex`.

- `/plots`, `/tex`:  
  Contains the figures and LaTeX tables shown in the manuscript (main
  text and supplement).

- `/rdata`:  
  Contains individual prediction error estimates as intermediate results
  (`_resdf.RData`).

## How to reproduce the results

Since the COMPANION data set cannot be made publicly available (it would
be contained in `/01_data`), only the individual prediction error
estimates are provided as intermediate results
(`/03_results/rdata/_resdf.RData`). With this data, the figures shown in
the manuscript (stored in `/03_results/plots`) can be reproduced.

To still allow running the code, the toy data set
`/01_data/data_phaselevel_toy.RData` can be used. With this data set,
the scripts `02_script_run_simulation.R`, `03_script_process_results.R`,
and `04_script_analysis_results.R` can be executed in this order.

## Session information

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
#>  [1] wesanderson_0.3.7     ggtext_0.1.2          ggh4x_0.2.7          
#>  [4] latex2exp_0.9.6       cumstats_1.0          ggpubr_0.6.0         
#>  [7] gridExtra_2.3         RColorBrewer_1.1-3    reshape2_1.4.4       
#> [10] kableExtra_1.3.4      table1_1.4.3          Hmisc_4.8-0          
#> [13] ggplot2_3.4.4         Formula_1.2-5         survival_3.5-3       
#> [16] lattice_0.20-45       data.table_1.14.10    nlme_3.1-162         
#> [19] party_1.3-11          strucchange_1.5-3     sandwich_3.0-2       
#> [22] zoo_1.8-11            modeltools_0.2-23     mvtnorm_1.2-4        
#> [25] R6_2.5.1              mlr3tuning_0.19.2     paradox_0.11.1       
#> [28] mlr3misc_0.13.0       mlr3pipelines_0.5.0-2 mlr3_0.17.2          
#> [31] forcats_1.0.0         haven_2.5.4           stringr_1.5.1        
#> [34] janitor_2.2.0         purrr_1.0.2           readxl_1.4.3         
#> [37] lubridate_1.9.3       tidyr_1.3.0           dplyr_1.1.4          
#> 
#> loaded via a namespace (and not attached):
#>  [1] TH.data_1.1-1        colorspace_2.1-0     ggsignif_0.6.4      
#>  [4] deldir_1.0-6         snakecase_0.11.0     htmlTable_2.4.1     
#>  [7] base64enc_0.1-3      gridtext_0.1.5       rstudioapi_0.15.0   
#> [10] listenv_0.9.0        fansi_1.0.4          coin_1.4-2          
#> [13] xml2_1.3.6           codetools_0.2-19     splines_4.2.2       
#> [16] libcoin_1.0-10       knitr_1.45           RhpcBLASctl_0.23-42 
#> [19] broom_1.0.5          cluster_2.1.4        png_0.1-8           
#> [22] compiler_4.2.2       httr_1.4.7           backports_1.4.1     
#> [25] Matrix_1.5-3         fastmap_1.1.1        cli_3.6.1           
#> [28] htmltools_0.5.5      tools_4.2.2          gtable_0.3.4        
#> [31] glue_1.7.0           Rcpp_1.0.10          carData_3.0-5       
#> [34] cellranger_1.1.0     vctrs_0.6.4          svglite_2.1.1       
#> [37] xfun_0.41            globals_0.16.2       rvest_1.0.3         
#> [40] timechange_0.2.0     lifecycle_1.0.4      rstatix_0.7.2       
#> [43] future_1.33.1        MASS_7.3-58.2        scales_1.3.0        
#> [46] lgr_0.4.4            hms_1.1.3            parallel_4.2.2      
#> [49] yaml_2.3.8           rpart_4.1.19         latticeExtra_0.6-30 
#> [52] stringi_1.7.12       checkmate_2.1.0      palmerpenguins_0.1.1
#> [55] rlang_1.1.1          pkgconfig_2.0.3      systemfonts_1.0.5   
#> [58] matrixStats_1.2.0    evaluate_0.23        htmlwidgets_1.6.2   
#> [61] tidyselect_1.2.0     parallelly_1.34.0    plyr_1.8.8          
#> [64] magrittr_2.0.3       generics_0.1.3       multcomp_1.4-22     
#> [67] pillar_1.9.0         foreign_0.8-84       withr_3.0.0         
#> [70] abind_1.4-5          nnet_7.3-18          tibble_3.2.1        
#> [73] car_3.1-1            crayon_1.5.2         uuid_1.2-0          
#> [76] interp_1.1-3         utf8_1.2.3           rmarkdown_2.25      
#> [79] jpeg_0.1-10          bbotk_0.7.3          digest_0.6.31       
#> [82] webshot_0.5.4        munsell_0.5.0        viridisLite_0.4.2
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub. -->
