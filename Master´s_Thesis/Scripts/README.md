# <h1 align="center">📂 Script Pipeline
The workflow is divided into 6 sequential scripts to facilitate data analysis, cleaning, visualization, and statistical inference:

### 1.Limpieza_datos.R
Function: Initial data loading and preparation.

Processes: Import of sampling Excel files by location (El Pardo, Puerta de Hierro, Puente de los Franceses, etc.) and meteorological variables. Creation of control variables (e.g., numerical assignment of Pre/Post treatments), cleaning of unnecessary variables, and conversion of date formats.

### 2.Descriptivos_Generales.R
Function: Initial exploration and global visualization of the dataset.

Processes: Generation of descriptive tables, normality tests (Kolmogorov-Smirnov), and creation of combined time-series plots to compare the overall density of larvae versus pupae across the sampling dates.

### 3.Descriptivos_Larvas.R
Function: Descriptive analysis focused exclusively on the larval stage.

Processes: Evaluation of the temporal evolution of total larval density and generation of paired bar charts to visualize the efficacy of Bti treatments (PRE and POST treatment comparison) segmented by sampling point.

### 4.Inferencial_Larvas.R
Function: Statistical modeling for larval density.

Processes: Execution of non-parametric tests (Mann-Whitney, Spearman) to select significant climatic variables. Implementation of Generalized Linear Mixed Models (GLMM) using the Tweedie family (glmmTMB) to evaluate the impact of the treatment and meteorological variables (temperature, wind, rainfall) by river section.

### 5.Descriptivos_Pupas.R
Function: Descriptive analysis of the pupal community and species dynamics.

Processes: Calculation of densities by species (S. pseudoequinum, S. ornatum, S. erythrocephalum, etc.), visualization of temporal succession using phenological heatmaps, and initiation of multivariate analysis of variance (PERMANOVA).

### 6.Inferencial_Pupas.R
Function: Modeling and prediction for the pupal stage.

Processes: Evaluation of climate impact on pupal density using the Mann-Whitney test and development of Machine Learning algorithms, specifically building and visualizing Decision Trees (rpart) to predict abundances based on cloud cover, wind gusts, photoperiod, and the treatment.

## <h2 align="center">🚀 Usage Instructions
Clone or download this repository to a local directory.

Place the original .xlsx data files (e.g., LARVAS_PUPAS.xlsx, the sampling point Excel files from P01 to P07, and meteorological data) in the root folder of the project, or adjust the path in the 1.Limpieza_datos.R script (currently set to setwd("~/TFM")).

Run the scripts in numerical order from 1 to 6. It is essential to run the cleaning script (1.Limpieza_datos.R) first, as it generates the structured dataframe (df) on which the subsequent descriptive and inferential scripts depend.

## <h2 align="center">⚖️ License and Acknowledgments
Project developed entirely as a Master's Thesis for the Universidad Internacional de La Rioja (UNIR). Sampling data and operational records belong to the Lokímica vector control campaigns for the year 2025.
