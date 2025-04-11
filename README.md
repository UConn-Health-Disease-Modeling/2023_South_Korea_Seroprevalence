# Infection Risk Evaluation After Omicron Using Serological and Confirmed Data

## Overview

This repository contains analysis code and supporting materials for a study evaluating infection risk following the Omicron wave in South Korea. The study compares hybrid immunity (infection + vaccination) and vaccine-only immunity, incorporating both confirmed and unconfirmed infections based on serological data.

## Background

Evaluating risk after an Omicron wave is particularly challenging due to a high number of unconfirmed infections. To address this, we analyzed infection risk in populations with different immune statuses using both confirmed cases from national surveillance and unconfirmed infections inferred from antibody dynamics.

## Methods

- Nationwide serological and surveillance data were used.
- Individuals were categorized into **hybrid immunity** or **vaccine-only immunity** groups based on S and N antibody test results.
- Infections were identified by:
  - Confirmed dates from the KDCA database.
  - Inferred cases from N seroconversion or significant N titer increases (Aug–Dec 2022).
- Two outcome definitions were applied:
  - **Conservative**: Only confirmed infections.
  - **Inclusive**: Confirmed + unconfirmed infections.
- We assessed misclassification using varying thresholds for N titer increases.
- Differences in survival and risk between groups were evaluated using Cox proportional hazards models over a 4-month follow-up.

## Results

- **Hybrid group**: Infection risk increased from **2% (conservative)** to **20% (inclusive)**.
- **Vaccine-only group**: Risk rose from **30% to 40%**.
- The protective advantage of hybrid immunity dropped from ~5-fold to ~2-fold over four months.
- **Unconfirmed infections**:
  - 92% of infections in the hybrid group were unconfirmed (62 confirmed vs. 563 unconfirmed).
  - 45% in the vaccine-only group were unconfirmed (821 confirmed vs. 1,063 unconfirmed).
- Middle-aged individuals (ages 40–59) and recent infections (<6 months) were more likely to be unconfirmed cases, especially in the hybrid group.

## Conclusions

Operational definitions of infection significantly affect incidence estimates. Including serologically inferred infections reveals a much larger infection burden, especially in those with recent infections or hybrid immunity. Surveillance and interventions (e.g., updated boosters) should prioritize middle-aged and vulnerable individuals who may experience undetected infections due to slower antibody responses or faster waning.
