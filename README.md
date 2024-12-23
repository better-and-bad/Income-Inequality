# Income Inequality

## Overview
This repo exists because I wanted to see if income inequality is actually 25% its reported value of John Early et al assert in their book The Myth of American Inequality. Turns out they're right! Download the data from the Tax Policy Center and downloaded my code and see for yourself!

## Key questions addressed:

How does the inclusion of taxes and transfers impact reported income inequality metrics?
What trends emerge when evaluating adjusted income data over time?
How can accurate metrics inform better economic policy?

## Data
[Income Quintiles](income_quintiles_5.xlsx) data comes from the Tax Policy Center. I came away with the same results as The Myth of American Inequality book by John Early et al.

## Setup
Requirements
R version: 4.0 or higher
Libraries: tidycensus, dplyr, ggplot2, scales
Installation
Clone the repository:

bash
Copy code
git clone git@github.com:your-username/income-inequality-analysis.git
cd income-inequality-analysis
Install required R packages:

R
Copy code
install.packages(c("tidycensus", "dplyr", "ggplot2", "scales"))
Open the R project file:

bash
Copy code
open Income-Inequality.Rproj
