# European Social Survey: Populism Analysis

## Research Question
**Are economic conditions primarily responsible for the support of radical right-wing populist parties?**

## Authors
- 40294
- 46146 
- 39109
- 42044
- 40503

## Project Overview
This repository contains a comprehensive analysis investigating the drivers of radical right-wing populist support across five European countries. Using cross-sectional survey data from Round 11 of the European Social Survey (ESS) alongside party classifications from The PopuList database, we examine whether economic insecurity, cultural attitudes, or political alienation best explain the rise of right-wing populism.

## Data Sources
- **European Social Survey (ESS) Round 11 (2023/24)** - Cross-national survey of attitudes and behaviors
- **The PopuList 3.0** - Expert-coded database of populist, far-right, and far-left parties in Europe

## Countries Analyzed
- France
- Germany
- Hungary
- Italy
- Spain

## Key Findings

Our research reveals:

1. **Cultural attitudes** (immigration views, social values) show the strongest and most consistent relationship with populist support
2. **Economic factors** play a more complex role than previously theorized, often interacting with political trust and cultural perceptions
3. **National context** significantly shapes populist dynamics, with drivers varying between countries:
   - France: Identity and cultural values dominate
   - Italy: Economic dissatisfaction and institutional distrust are more predictive
   - Hungary: Shows distinctive patterns of populist support including higher trust in government

Machine learning models (CatBoost) with SHAP analysis highlighted the non-linear interactions between economic, cultural, and political factors, suggesting that a multidimensional explanation better captures the phenomenon than economic determinism alone.

## Repository Contents

- `Group_7.qmd` - Full project report with code, visualizations and analysis
- `Group_7.html` - HTML version of the report for easy viewing
- `ESS10_appendix_a7_e03_1.pdf` - Data dictionary for understanding survey questions
- `final_df_with_populist_splits.csv` - Cleaned and joined dataset
- `IND_Reflections` - Individual reflections from team members

## Methodology
Our analytical approach combines:

1. Exploratory data analysis with descriptive statistics
2. Baseline logistic regression modeling
3. Advanced CatBoost classification with hyperparameter tuning
4. SHAP (SHapley Additive exPlanations) value interpretation
5. Country-specific models for comparative analysis

## Research Limitations

Key limitations include:
- Cross-sectional nature of data prevents causal inference
- Reliance on subjective perceptions rather than objective indicators
- Party classification challenges for emerging political movements
- Model interpretability trade-offs with machine learning approaches

## Future Research Directions

Potential extensions include:
- Longitudinal analysis to track changes over time
- Applying methods to non-European contexts
- Comparison of right-wing vs. left-wing populist support drivers
- Regional-level analysis incorporating contextual economic factors