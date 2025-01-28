# Indicator Tracking Tool (ITT)

The **Indicator Tracking Tool (ITT)** is a user-friendly application designed to empower stakeholders with actionable insights for informed decision-making in the supply chain. Developed by users for users, ITT leverages periodic and aggregated logistics data from the [Kenya Health Information System (KHIS)](https://hiskenya.org/) to analyze and visualize supply chain performance in critical health programs.

## Key Features

The tool focuses on the following health programs and areas:

- **Family Planning**: 
  - Reporting rates
  - Data quality
  - Stock status
  - Stock outs
- **Immunization**:
  - Coverage analysis
  - Wastage analysis
- **Malaria** and **Nutrition** programs

### Data Update Frequency
- **Current Year Data**: Updated daily.
- **Previous Year Data**: For any changes made in KHIS that require updates in the tool, please inform us.
- **Last Month's Data**: Loaded after the 15th of the current month.

## Tracer Commodities and Indicators

The tool explores supply chain performance using tracer commodities, providing insights through a variety of calculated indicators:

### Family Planning Indicators
1. **Reporting Rates**: Percentage of facilities reporting within the required timeframe.
2. **Data Quality**: Accuracy and completeness of the reported data.
3. **Stock Status**: Current stock levels compared to demand.
4. **Stock Outs**: Instances where stock levels fall to zero.

### Immunization Indicators
1. **Coverage**: Proportion of the target population reached with immunizations.
2. **Wastage**: Percentage of vaccines wasted during the reporting period.

### Malaria and Nutrition
Additional indicators specific to these programs will be included in future iterations.

## Installation and Setup

To use the ITT in R, follow these steps:

1. Clone this repository:
   ```bash
   git clone https://github.com/danielmaangi/insupplyITT.git

