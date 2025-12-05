## Overview
This repository contains analysis code for estimating SARS-CoV-2 infection risk after South Korea’s Omicron wave using nationwide serological data (K-SEROSMART) linked with vaccination and confirmed-case surveillance. Both confirmed and serology-inferred infections are incorporated to provide a more accurate assessment of post-Omicron immunity.

## Immunity Groups
Participants were categorized using S/N antibody results and vaccination history:

- Hybrid immunity  
- Vaccine-induced immunity  
- Infection-induced immunity  
- Naïve

## Outcomes
Two infection outcomes were defined:

- **Conservative:** laboratory-confirmed cases only  
- **Inclusive:** confirmed cases + serology-inferred infections  
  - N seroconversion (vaccine-only)  
  - Significant N-titer increase (hybrid / infection-only)

Missing infection dates for inferred cases were imputed using predictive mean matching (m=50).

## Methods
- Participants vaccinated between Waves 1 and 2 were excluded.  
- Age-standardized Kaplan–Meier estimators were used for cumulative risk.  
- Time-varying Cox models fitted hazards over 4-month follow-up (proportional hazards violated).  
- Adjustments included age and sex; landmark analyses were performed for validation.

## Key Findings
- **4-month cumulative risk (inclusive outcome):**
  - Hybrid: 20%  
  - Infection-induced: 24%  
  - Vaccine-only: 40%

- **Unconfirmed infections were common:**
  - Hybrid: 92%  
  - Infection-induced: 81%  
  - Vaccine-only: 50%

- **Hybrid immunity showed strong early protection**, but waned rapidly when asymptomatic/mild infections were included.  
- Infection-induced immunity demonstrated **similar or greater protection** than hybrid immunity by month 4.

## Conclusions
Including serology-inferred infections reveals a much higher true infection burden and a faster decline in hybrid immunity than suggested by confirmed cases alone. Serological surveillance is essential for tracking silent transmission and guiding booster strategies.

## Repository Structure
- `data_processing/` – serology cleaning, infection classification, imputation  
- `survival_analysis/` – KM curves, Cox models, diagnostics  
- `figures/` – scripts for classification diagrams, KM plots, hazard ratio curves  
- `tables/` – Table 1–3 generation  
- `sensitivity/` – N-titer thresholds and delta-adjusted imputation

## Citation
Please cite:  
**“Protective effectiveness of SARS-CoV-2 infection risk among hybrid, vaccine, and infection-induced immunity against the Omicron variant, K-SEROSMART.”**