# Colorectal Cancer Analysis in Nairobi
This project uses Nairobi County health records (2018–2022) to map and model colorectal cancer across time, demographics, and geography. The goal was to translate raw data into actionable insights: identifying high-risk groups and areas, understanding trends, and guiding targeted interventions. Using 510 de-identified patient records, I applied a mix of data science and epidemiology methods. 

# Data Cleaning and Trend Forecasting
I first cleaned the data (removing duplicates and impossible dates) and applied multiple imputation (MICE) for missing values. For time series, I aggregated monthly case counts and compared three forecasting models (SARIMA, ETS, regression+ARIMA). Model comparison (via RMSE and MAPE) showed that a seasonal ARIMA was most accurate, capturing the year-to-year cycles. We then projected case counts to 2025 under this model. 

![Monthly Colorectal Cancer Cases-Forecast to 2025](https://github.com/user-attachments/assets/c527db0f-75b1-48e2-9dd8-c60ac39283c1)

The model captures the seasonal peaks and an overall rising trend.

# GeoSpatial analysis
In spatial analysis, I linked each case to a Nairobi sub-region (constituency),for analysis
![nairobi_map](https://github.com/user-attachments/assets/33d7d75f-9676-4eec-9679-ec3bff88746a)

# Regional Bar chart
A bar chart of cases by region and sex highlights the geographic distribution: Embakasi had by far the most cases (128 total), followed by Dagoretti (75) and Kibera (62). (Females and males had comparable counts in each region.) Notably, Embakasi’s bar towers over others, confirming it as a case epicenter.

![regional_distribution](https://github.com/user-attachments/assets/e3fdc913-cd24-466c-a48e-2dd67ef48826)

# Temporal analysis
To explore temporal and subgroup trends, I also examined annual counts and stratified time plots. The figures below shows three panels: annual trends by cancer site (colon vs rectum), monthly trends by sex and site, and age-group trends. These reveal subtle patterns over time:
![Annual trends by cancer site](https://github.com/user-attachments/assets/6b5061b2-f6b4-465a-9d24-293499e38b2c)
![monthly trends by gender and site](https://github.com/user-attachments/assets/e7a7eb48-7b83-49ef-9151-dfc5588de7a6)
![Age specific temporal trends](https://github.com/user-attachments/assets/ae80b4fd-f160-4ab5-aa43-f2611f6b48d1)

# Survival Analysis

Finally, I performed survival analysis. Using follow-up times and vital status, I fit Kaplan–Meier curves and a Cox model for mortality. The Kaplan–Meier plots (overall and stratified by gender, age, residence) are shown here:
![Combined survival curve](https://github.com/user-attachments/assets/f8b30b79-5db1-4bae-b9e8-7b10ede7dd9c)

# Cox model
The Cox model (adjusting for age, sex, and location) is summarized by this forest plot of hazard ratios (HR)
![hazard_ratios_plot](https://github.com/user-attachments/assets/f6ddff45-0d82-4293-9232-11e50cd7ce48)


# Key insights
In Nairobi, colorectal cancer is rising rapidly; urgent action is needed. The data suggest focusing on screening and education in areas like Embakasi/Kibera. Age <45 should not be overlooked, as younger patients increasingly present with advanced disease. Although the adjusted hazard ratios hint at disparities (e.g. Kibera vs Westlands), no predictor reached statistical significance— likely due to limited events. my transparent workflow (shared code and visualizations) aims to inform both public health officials and fellow analysts, bridging data science with health impact.

# Project Details
All analyses were performed in R using packages like tidyverse, forecast, sf, survival, and ggsurvfit. Data management (imputation, cleaning) and model selection followed best practices. 

# Conclusion
This study transforms raw Nairobi cancer registry data into actionable intelligence. By combining statistical rigor with clear visuals, it uncovers hidden patterns (like the Embakasi hotspot and the under-45 stage trend). Policymakers and clinicians can use these insights to target interventions. As new data become available, this analysis framework can be updated to track progress and guide future cancer control efforts.
