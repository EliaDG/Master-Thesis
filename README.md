# Master Thesis: Determinants of Growth in EU28 + CANDID8

Welcome! The structure of this repository is the following:

- In `Latex/` you can find the PDFs and corresponding raw Latex code for the formatting of the final file;
- `Literature/` gathers all papers and articles mentioned in the Bibliography, plus some others which I read during the initial literature exploration phase but turned out to be not relevant for the final Thesis;
- In `Pitch/` you can find the PDFs of my Topic Pitch and Update presentations with my Supervisor;
- In `Project/` is the main body of research with R code and data.
- `Setup.xlxs` is an Excel Table I used to monitor data coverage, notes and sources as I was collecting.

More in detail, `Project/` contains:
- `code/` where all R code is stored for the cleaning, visualisation and modelling of the data. The `code/` folder is organized into several chapters based on specific themes or processes involved in the analysis. Here's an overview of the sections:

  #### A. Data Preparation and Sources
  #### B. Data Imputation
  #### C. Matrix Construction
  #### D. Modeling and Submodels
  #### E. Robustness Checks
  #### F. Visualizations and Tables
  #### Supporting Files:
  - **\_functions.R**: Collection of custom functions used throughout the analysis.
  - **\_packages.R**: Script for loading and managing required R packages.
  - **\_Update.R**: Code for reporting updates during meetings with the Supervisor.

- The original and intermediate data inputs versions I worked with, grouped by source;
- `final-inputs/` containing the final original dataset, its imputed and hot-one encoded versions, and the corresponding spatial matrices for ther regions under scope;
- `final-outputs/` containing the R Workspaces with all results of this research;
- `pictures/` containing .png files I created and used in the final PDF file.

Should you have questions, please do not hesitate to contact me at h12039313@s.wu.ac.at :)
