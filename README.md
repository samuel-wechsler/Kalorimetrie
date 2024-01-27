# Project README

## Overview

This GitHub repository contains R scripts for a project focused on experimental measurements related to calorimetry, solvation enthalpy, and titration. The project involves analyzing temperature-time data, calculating heat capacities, determining specific heat capacities of liquids, and exploring solvation enthalpy.

### Repository Contents:

1. **calibrations.R:** Analyzes temperature-time data from calibration experiments, calculates system and water heat capacities, and saves the results in a PDF file.

2. **ethanol.R:** Analyzes temperature-time data from experiments involving the heat capacity of various liquids, particularly ethanol-water mixtures. It calculates mass percentages and specific heat capacities, providing results and confidence intervals.

3. **solving.R:** Analyzes temperature-time data from experiments related to solvation enthalpy. It calculates molar solution enthalpy of NH4NO3 and reports the mean value along with a confidence interval.

4. **titration.R:** Analyzes titration data, including continuous and discrete titration experiments. It generates plots and saves them as PDF files.

5. **functions.R:** Contains reusable functions used across the scripts, such as parsing data, calculating mean and confidence intervals, plotting temperature-time data, and performing various calculations.

## Instructions

### Clone Repository:

To run the scripts, clone the repository to your local machine using the following command:

```bash
git clone https://github.com/samuel-wechsler/Kalorimetrie
```

Note: The scripts may depend on specific data files, so make sure the data files are located in the correct directories or modify the file paths accordingly.

## Data Files
The scripts use various data files for calibration, heat capacity experiments, solvation enthalpy experiments, and titration experiments. Ensure these files are present in the appropriate directories or update the file paths in the scripts.

## Results
Upon successful execution, the scripts generate plots and print relevant results, such as heat capacities, specific heat capacities, molar solution enthalpy, and titration information.
