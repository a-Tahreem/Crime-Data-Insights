# Crime Data Insights

An interactive R Shiny application for analyzing crime data in Los Angeles from 2021 to 2023.

## Features
- **Crime Trends**: Visualize crime trends over time and explore patterns by area, type, and time of day.
- **Correlation Analysis**: Generate correlation matrices to identify relationships between variables such as victim demographics and crime categories.
- **Geographic Insights**: Interactive maps and visualizations based on latitude and longitude data.
- **Principal Component Analysis (PCA)**: Explore relationships between multiple variables using PCA.
- **Customizable Filters**: Filter data by date, crime type, area, and more.

## Data
The dataset includes 500 crime records with details such as:
- **Date and Time**: Precise date and time of the crime (`DATE OCC`, `TIME OCC`).
- **Location**: Latitude, longitude, and area name (`LAT`, `LON`, `AREA NAME`).
- **Victim Details**: Age, gender, and descent of victims.
- **Crime Type**: Description and classification of crimes (`Crm Cd Desc`).

## Technologies Used
- **R**: For data processing and statistical analysis.
- **Shiny**: To create an interactive web application.
- **ggplot2**: For detailed visualizations.
- **dplyr**: For efficient data manipulation.
- **FactoMineR & factoextra**: For Principal Component Analysis (PCA).
- **corrplot**: To generate correlation matrices.

## How to Run
1. Clone this repository to your local machine:
   ```bash
   git clone https://github.com/<a-Tahreem>/CrimeDataInsights.git
2. Open the project in RStudio 
3. Install the required R packages:
    install.packages(c("shiny", "ggplot2", "dplyr", "FactoMineR", "factoextra", "corrplot", "lubridate", "scales"))
4. Run the Shiny app:
   shiny::runApp()

## Visualizations

Examples of visualizations provided by the application:
- Crime trends: Line charts showing changes in crime frequency over time.
- Correlation heatmaps: Displaying relationships between variables.
- PCA graphs: Highlighting key components in the dataset.

## Future Improvements

- Integrate real-time crime data updates from APIs.
- Expand dataset to include more years and additional features
- Develop predictive models to identify potential hotspots
