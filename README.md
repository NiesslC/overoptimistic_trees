
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Beyond algorithm hyperparameters: on preprocessing hyperparameters and associated pitfalls in machine learning applications

<!-- badges: start -->
<!-- badges: end -->

This repository contains code and intermediate results data of the
manuscript “Beyond algorithm hyperparameters: on preprocessing
hyperparameters and associated pitfalls in machine learning
applications” by Christina Sauer, Luzia Hanßum, Farina Hodiamont,
Claudia Bausewein, Anne-Laure Boulesteix, and Theresa Ullmann.

## File description

### `/02_code`

This folder contains the code used to generate all results shown in the
manuscript. To run the experiment, only the files containing “script” in
their file name need to be run in numerical order (e.g., 01_script\_
before 02_script\_). The script files serve the following purposes:

- **`01_script_preprocessing_fixed.R`**:  
  Loads the COMPANION data set and performs preprocessing steps
  described in Supplementary Section B.2.1 of the manuscript.

- **`02_script_run_simulation.R`**:  
  Runs the experiment described in Section 5.2 of the manuscript.

- **`03_script_process_results.R`**:  
  Processes simulation results into a unified data set and saves it as
  `03_results/rdata/_resdf.RData`.

- **`04_script_analysis_results.R`**:  
  Creates Figure 5 (main manuscript) and other figures shown in the
  supplement.

- **`02b_script_descriptives_companion.R`**:  
  Generates descriptive statistics for the COMPANION data set.  
  *Note*: This file can be executed directly after
  `01_script_preprocessing_fixed.R`, hence the numbering `02b`.

The remaining files in the folder serve the following purposes:

- **`_fcts_optim.R`, `_fcts_resampling.R`**:  
  Functions for executing the experiment (see in-file function
  descriptions for details).

- **`_fcts_preproc.R`**:  
  Functions specifying the different preprocessing options.

- **`_src_add_mlrlearners.R`**:  
  Adds learning algorithms of the experiment to the `mlr3` framework.

- **`_src_add_mlrpipeops.R`**:  
  Integrates preprocessing steps of the experiment into the `mlr3`
  framework.

- **`_src_setup_learners.R`**:  
  Configures learning algorithms for the experiment (e.g., search space
  for HP tuning).

### `/03_results`

This folder contains three subfolders: `/plots`, `/rdata`, and `/tex`.

- **`/plots`**, **`/tex`**:  
  Contains the figures and LaTeX tables shown in the manuscript (main
  text and supplement).

- **`/rdata`**:  
  Contains individual prediction error estimates as intermediate results
  (`_resdf.RData`).

## How to reproduce the results

Since the COMPANION data set cannot be made publicly available (it would
be contained in `01_data`), only the individual prediction error
estimates are provided as intermediate results
(`03_results/rdata/_resdf.RData`).  
With this data, the figures shown in the manuscript (stored in
`03_results/plots`) can be reproduced.

## Session info

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
#>  [1] nlme_3.1-162          party_1.3-11          strucchange_1.5-3    
#>  [4] sandwich_3.0-2        zoo_1.8-11            modeltools_0.2-23    
#>  [7] mvtnorm_1.2-4         R6_2.5.1              mlr3tuning_0.19.2    
#> [10] paradox_0.11.1        mlr3misc_0.13.0       mlr3pipelines_0.5.0-2
#> [13] mlr3_0.17.2           purrr_1.0.2           forcats_1.0.0        
#> [16] tidyr_1.3.0           dplyr_1.1.4          
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyselect_1.2.0     xfun_0.41            coin_1.4-2          
#>  [4] listenv_0.9.0        splines_4.2.2        lattice_0.20-45     
#>  [7] vctrs_0.6.4          generics_0.1.3       htmltools_0.5.5     
#> [10] bbotk_0.7.3          yaml_2.3.8           utf8_1.2.3          
#> [13] survival_3.5-3       rlang_1.1.1          pillar_1.9.0        
#> [16] glue_1.7.0           withr_3.0.0          palmerpenguins_0.1.1
#> [19] uuid_1.2-0           multcomp_1.4-22      matrixStats_1.2.0   
#> [22] lifecycle_1.0.4      future_1.33.1        codetools_0.2-19    
#> [25] evaluate_0.23        knitr_1.45           fastmap_1.1.1       
#> [28] parallel_4.2.2       fansi_1.0.4          TH.data_1.1-1       
#> [31] backports_1.4.1      checkmate_2.1.0      parallelly_1.34.0   
#> [34] RhpcBLASctl_0.23-42  digest_0.6.31        cli_3.6.1           
#> [37] tools_4.2.2          magrittr_2.0.3       tibble_3.2.1        
#> [40] crayon_1.5.2         pkgconfig_2.0.3      MASS_7.3-58.2       
#> [43] libcoin_1.0-10       Matrix_1.5-3         data.table_1.14.10  
#> [46] rmarkdown_2.25       rstudioapi_0.15.0    lgr_0.4.4           
#> [49] globals_0.16.2       compiler_4.2.2
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub. -->
