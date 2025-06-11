# Introduction
This is the R code used in the analyses of "Virologic, immunologic, and metabolic outcomes and mortality in people living with HIV on B/F/TAF versus dolutegravir-based antiretroviral therapies: a retrospective multicenter cohort study in China".

Now the paper is submitted to the **.    

## System requirements
1) All software dependencies and operating systems (including version numbers): Windows XP and later (including 64-bit versions of Windows) on ix86 and x86_64 chips
2) Versions the software has been tested on: R (version 4.4.1) and R studio (version 2024.04.2).
3) Any required non-standard hardware: None.
For more details, please refer to https://cran.r-project.org/bin/windows/base/old/4.4.1/ and https://cran.r-project.org/bin/windows/base/old/4.4.1/README.R-4.4.1


## Installation guide
1) Instructions: Install and run R (version 4.4.1) and R studio (version 2024.04.2), then open and run the R code files from top to bottom. All codes for this study were R codes, and we did not develop softwares. R (version 4.4.1) and corresponding guide can be downloaded from https://cran.r-project.org/bin/windows/base/old/4.4.1/. R studio (version 2024.04.2) corresponding guide can be downloaded from https://docs.posit.co/connect/2024.04.2/user/.
2) Typical install time: around 10 minutes.


## Demo

> **Data files**  
> - `data2.csv`  
> - `data5.csv`

> **Code files**  
> - **Poisson model**: `R code for Poisson model.R` (uses `data2.csv`)  
> - **Mixed-effects model**: `R code for mixed-effects model.R` (uses `data5.csv`)

### Run the demo

1. Open **`R code for Poisson model.R`** in R Studio.  
2. Make sure **`data2.csv`** is in your working directory.  
3. Source/run the script from top to bottom.  
4. Repeat for **`R code for mixed-effects model.R`** with **`data5.csv`**.

### Expected output

- **Poisson model script**  
  - `poisson_fit` object  
  - Breusch–Pagan test results  
  - Robust standard errors and Anderson–Darling test output

- **Mixed-effects model script**  
  - `lmm_model` object  
  - Summary table of fixed effects & their standard errors

### Expected run time

- **Poisson model**: ~10 s on a typical desktop  
- **Mixed-effects model**: ~20 s (depends on data size)


## Instructions for use
1) How to run the software on your data: Install and run R (version 4.4.1) and R studio (version 2024.04.2), then open and run the R code files from top to bottom. Detailed instructions are provided in the R code files.

# Contact
In case of any issues with the R code, please contact the first author Xinsheng Wu (wuxsh25@mail2.sysu.edu.cn).
