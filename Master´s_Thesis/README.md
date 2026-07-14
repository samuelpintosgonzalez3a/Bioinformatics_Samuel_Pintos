# <h1 align="center">🪰🪰 MASTER'S THESIS: SIMULIID CONTROL IN THE MANZANARES RIVER: A PREDICTIVE STUDY OF BLACK FLY TREATMENTS 🪰🪰



This repository contains the computational workflow (R scripts) developed for the Master's Thesis (TFM) titled: "Simuliid control in the Manzanares river: A predictive study of black fly treatments", presented by Samuel Pintos González at the Universidad Internacional de La Rioja (UNIR). 

## <h1 align="center">📑 Project Summary

The proliferation of simuliids (black flies) in Madrid's urban environment represents a growing public health challenge. This study analyzes the population dynamics of these dipterans in the Manzanares river during the 2025 campaign. The core of the project consists of evaluating the operational efficacy of larvicide treatments with Bacillus thuringiensis var. israelensis (Bti) through the integration of environmental variables and the development of advanced predictive models. Unlike reactive management models, this study implements a predictive system capable of anticipating demographic peaks based on meteorological forecasts.  

## <h1 align="center">🎯 Research Objectives

The study addresses three main lines of analysis:

**1. Descriptive Study:** To understand the variables affecting the life cycle of simuliids, their temporal behavior, and relative presence according to size and stage (larva/pupa), segregated by sampling points and climatic conditions.

**2. Inferential Study:** To identify the variables with the greatest weight in the population fluctuation (increase or decrease) of larvae and pupae.

**3. Predictive Modeling:** To develop mixed models to quantify population density and decision trees to determine which climatic variables (wind, temperature, cloud cover, photoperiod) act as the main "stressors" or "stimulators" of the pest.

## <h1 align="center">🧪Methodology and Algorithms

The analysis processes a dataset of 147 records (based on 7 sampling points and 21 sampling dates) enriched with precise climatic data from the Visual Crossing platform. 

Environment: `R` (v4.5.1) and `RStudio`.  

**Statistical Models:** Use of `Generalized Linear Mixed Models` (GLMM) with the Tweedie family (via `glmmTMB`) to model abundance and density.

**Machine Learning:** Implementation of `Elastic Net` for predictor variable selection and `Decision Trees` (rpart) for the visual estimation of larval and pupal abundance.

**Validation:** Use of analysis of variance, `Tukey post-hoc tests`, and `PERMANOVA` to validate differences in community structure between locations and species.  

## <h1 align="center">💡 Main Conclusions

**🪰 Treatment Efficacy:** Bti demonstrated high potential, achieving population reductions exceeding 91% under optimal summer conditions. However, the average operational efficacy was 31.44%, showing fragility during episodes of autumn instability.

**🪰 Climatic Influence:** Cloud cover and wind gusts act as potent density stimulators, while increased temperatures and precipitation generally operate as limiting factors.

**🪰 Anticipatory Management:** The study concludes that it is imperative to transition towards a predictive management model that uses these algorithms to calculate the ideal dosing windows, optimizing resources and minimizing environmental impact.
