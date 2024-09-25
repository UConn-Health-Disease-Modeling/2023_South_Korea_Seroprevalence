# Antibody Decay and Infection Risk in Hybrid vs. Vaccine-Induced Immunity Populations: A South Korean Study

## Overview

This repository contains data and analysis scripts for our study that investigates antibody decay trends and infection risks across different age groups in South Korea, focusing on the comparison between hybrid and vaccine-induced immunity populations after the Omicron wave. The study aims to understand how antibody levels decay over time and how this affects the likelihood of breakthrough infections.

## Background

Research has suggested that vaccine effectiveness may decrease over time. However, the relationship between antibody decay and actual protection from breakthrough infections is not well understood. In this study, we analyze data from community-based nationwide surveillance conducted in South Korea. The research focuses on comparing infection risks and antibody levels between individuals with hybrid immunity (from both vaccination and prior infection) and those with vaccine-induced immunity alone.

## Methods

- **Cohort Construction**: Two cohorts were constructed based on serology information (S and N status) to distinguish between hybrid and vaccine-induced immunity populations.
- **Kaplan-Meier Survival Analysis**: We used survival analysis to estimate the probability of remaining infection-free over time for both cohorts.
- **Cox Proportional Hazards Regression**: This model was used to assess differences in infection risk between the two cohorts, accounting for age group differences.
- **Mixed-Effects Logistic Regression**: To quantify protective efficacy associated with antibody levels, we applied mixed-effects models across various age groups.

## Results

- **Antibody Levels**: The hybrid immunity group had approximately twice the antibody levels (~10,000) at 12 months compared to the vaccine group.
- **Age Group Differences**: Although antibody decay was similar across age groups, protection against infection decreased more quickly in individuals over 60 years old, while those under 20 saw the largest reduction in protection over time.
- **Protection Thresholds**: To maintain an 80% probability of remaining infection-free, antibody levels thresholds were identified as >250,000 for individuals under 20, 20,000 for those over 60, and 15,000 for individuals aged 40-60 in the vaccine group.

## Conclusion

- **Children and Booster Shots**: Children without previous infections are at high risk and should be prioritized for booster vaccinations.
- **Older Adults**: Adults aged 65 years or older may need more frequent vaccination to maintain adequate immunity levels.

## Repository Contents

- `Code/`: R scripts used for data processing, survival analysis, and regression modeling.
- `Manuscripts/`: Outputs including survival curves, regression results, and antibody decay plots.
- `References/`: Latex .bib file of the references. 

## Contact

For questions or inquiries, please contact [Your Name] at [email@example.com].