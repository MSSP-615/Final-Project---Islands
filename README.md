# US Virgin Islands: Climate, Demographics, and Economic Analysis

A comprehensive data analysis project examining hurricane impacts, tourism trends, weather patterns, and demographics in the US Virgin Islands (USVI) from 1975-2025.

## Project Overview

This project combines multiple data sources to analyze:
- **Hurricane frequency and intensity** (1989-2019)
- **Tourism trends** - Air arrivals and cruise passengers (1995-2024)
- **Weather and climate patterns** (1975-2025)
- **Demographics and population** (2020 Census)
- **Economic recovery** after major hurricane events

### Key Findings

- 13 hurricanes struck the USVI between 1989-2019 (average: one every 2.3 years)
- 2017 Hurricanes Irma and Maria caused tourism to collapse by 27-36%
- Economic recovery from major hurricanes takes 3-4 years
- Population: 87,146 (2020), 71.4% Black/African American

## Data Sources

### Hurricane Data
- **Source**: NOAA National Hurricane Center
- **Period**: 1989-2019
- **Format**: Manually compiled from tropical cyclone reports and wikipedia
- **Variables**: Hurricane name, category, landfall date, year

### Tourism Arrivals
- **Source**: USVI Bureau of Economic Research
- **Period**: 1995-2024
- **Format**: Monthly PDF reports (extracted using custom R script)
- **Variables**: Air arrivals, cruise passengers by island (St. Thomas/St. John, St. Croix)

### Weather Data
- **Source**: NOAA National Centers for Environmental Information
- **Period**: 1975-2025
- **Method**: `rnoaa` R package with API access
- **Variables**: Precipitation, max/min temperature, storm events

### Demographics
- **Source**: U.S. Census Bureau, 2020 Decennial Census
- **Note**: Data manually compiled (Census API doesn't support territories)
- **Variables**: Race/ethnicity breakdown, total population

### Economic Indicators
- **Source**: World Bank World Development Indicators
- **Period**: 1975-2023
- **Variables**: Life expectancy, fertility rate, labor force, net migration

### Additional Sources:

-   Britannica. (2024). *United States Virgin Islands*. Retrieved from https://www.britannica.com/place/United-States-Virgin-Islands
-   U.S. Department of the Interior. (2024). *U.S. Virgin Islands*. Retrieved from https://www.doi.gov/oia/islands/virgin-islands

### Image Sources: 
-   https://www.visitusvi.com/
-   https://www.brides.com/us-virgin-islands-honeymoon-7373656
-   https://www.virginbookings.com/us-virgin-islands


## Resources

- [NOAA National Hurricane Center](https://www.nhc.noaa.gov/)
- [Wikipedia](https://en.wikipedia.org/wiki/Hurricanes_in_the_Virgin_Islands#cite_note-1)
- [USVI Bureau of Economic Research](https://usviber.org/archived-data/)
- [US Census Bureau - Island Areas](https://www.census.gov/programs-surveys/decennial-census/about/island-areas.html)
- [World Bank Open Data](https://data.worldbank.org/)
- [NOAA NCEI](https://www.ncei.noaa.gov/)

## Acknowledgments

- NOAA for hurricane and weather data
- USVI Bureau of Economic Research for tourism statistics
- US Census Bureau for demographic data
- World Bank for economic indicators
- Anthropic's Claude for analysis assistance, as well as, ChatGPT


