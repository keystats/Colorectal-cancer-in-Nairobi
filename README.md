## Colorectal Cancer Analysis in Nairobi
This project uses Nairobi County health records (2018–2022) to map and model colorectal cancer across time, demographics, and geography. The goal was to translate raw data into actionable insights: identifying high-risk groups and areas, understanding trends, and guiding targeted interventions. Using 510 de-identified patient records, I applied a mix of data science and epidemiology methods. 

I first cleaned the data (removing duplicates and impossible dates) and applied multiple imputation (MICE) for missing values. For time series, I aggregated monthly case counts and compared three forecasting models (SARIMA, ETS, regression+ARIMA). Model comparison (via RMSE and MAPE) showed that a seasonal ARIMA was most accurate, capturing the year-to-year cycles. We then projected case counts to 2025 under this model. 

# Figure: Forecast of monthly colorectal cancer cases 
The model captures the seasonal peaks and an overall rising trend.

In spatial analysis, I linked each case to a Nairobi sub-region (constituency),for analysis
# Figure: nairobi map

A bar chart of cases by region and sex highlights the geographic distribution: Embakasi had by far the most cases (128 total), followed by Dagoretti (75) and Kibera (62). (Females and males had comparable counts in each region.) Notably, Embakasi’s bar towers over others, confirming it as a case epicenter.

# figure regional bar chart

To explore temporal and subgroup trends, I also examined annual counts and stratified time plots. The figure below combines three panels: annual trends by cancer site (colon vs rectum), monthly trends by sex and site, and age-group trends. These reveal subtle patterns over time:
# combined trend lines

Finally, I performed survival analysis. Using follow-up times and vital status, I fit Kaplan–Meier curves and a Cox model for mortality. The Kaplan–Meier plots (overall and stratified by gender, age, residence) are shown here:
# combined survival

The Cox model (adjusting for age, sex, and location) is summarized by this forest plot of hazard ratios (HR)
# forest plot

Key insights: In Nairobi, colorectal cancer is rising rapidly; urgent action is needed. The data suggest focusing on screening and education in areas like Embakasi/Kibera. Age <45 should not be overlooked, as younger patients increasingly present with advanced disease. Although the adjusted hazard ratios hint at disparities (e.g. Kibera vs Westlands), no predictor reached statistical significance— likely due to limited events. my transparent workflow (shared code and visualizations) aims to inform both public health officials and fellow analysts, bridging data science with health impact.

Project Details: All analyses were performed in R using packages like tidyverse, forecast, sf, survival, and ggsurvfit. Data management (imputation, cleaning) and model selection followed best practices. The README and code (including this document’s figures) are designed for reproducibility and clarity.

Conclusion: This study transforms raw Nairobi cancer registry data into actionable intelligence. By combining statistical rigor with clear visuals, it uncovers hidden patterns (like the Embakasi hotspot and the under-45 stage trend). Policymakers and clinicians can use these insights to target interventions. As new data become available, this analysis framework can be updated to track progress and guide future cancer control efforts.
