# StaViC (Stacked Probability Visualization & Comparison) Shiny App            

This repository contains the code for **StaViC**, a Shiny app designed to visualize and compare patient health state probabilities over time in hospital-acquired infection models.  

## 🏥 Overview  

StaViC helps users understand the dynamics of patients who acquire an infection after hospital admission by:  

- **Visualizing the probability of patients being in different health states over time**  
- **Enhancing graphical representation of disease progression**  
- **Facilitating direct comparisons of interventions between groups or in simulation scenarios**  

This tool aims to improve visualization clarity and support decision-making in clinical and research settings.  

## 📥 Installation  

1. Download all the files in this repository.  
2. Open **`run.R`** in your R environment.  
3. Install the required packages (lines 31–37 in `run.R`).  
4. Run the entire script and specify the directory where the app is located.  

## 📂 Files  

- **`run.R`** – Main script to run the app.  
- **`app.R`** – Loads packages, sources other files, and initializes the UI.  
- **`functions.R`** – Contains all essential functions used in the app.  
- **`interface.R`** – Defines the app's user interface.  
- **`www/`** – Directory containing graphics and other static assets.  

## 🛠️ License  

**Copyright © 2025 Jean-Pierre Gnimatin, Marlon Grodd, Susanne Weber, Derek Hazard, Martin Wolkewitz**  

This program is free software: you can redistribute and/or modify it under the terms of the **GNU General Public License (GPL)**, either version 3 or any later version.  

This program is distributed in the hope that it will be useful, but without any warranty, without even the implied warranty of merchantability or fitness for a particular purpose. See the GNU General Public License for more details.  

You should have received a copy of the **GNU General Public License** along with this program. If not, visit [www.gnu.org/licenses](http://www.gnu.org/licenses/).  
