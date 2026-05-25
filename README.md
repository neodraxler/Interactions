Moderation Regression Simulator

An interactive Shiny application for exploring moderation regression models, interaction effects, and model interpretation in a visual and beginner-friendly way.

Overview

This app helps users understand how predictors, moderators, and interaction terms influence regression outcomes. It is designed for teaching, learning, and experimenting with moderation analysis using simulated or uploaded datasets.

The application includes:

* Moderation regression modeling
* Visual interaction plots
* Refit model comparisons
* Signal-to-noise exploration
* Statistical interpretation tools
* Simulated example datasets
* CSV upload support

Features

Moderation Analysis

Fit moderation regression models using:

Y = b_0 + b_1X + b_2M + b_3(X \times M)

Where:

* Y = outcome variable
* X = predictor variable
* M = moderator variable
* X × M = interaction effect

Visual Learning Tools

The app provides visual explanations for:

* Strong vs weak predictors
* Interaction effects
* Positive and negative relationships
* Model fit changes
* Refit vs visual-only models
* Signal-to-noise ratio concepts

Dataset Support

Users can:

* Upload their own CSV files
* Use built-in simulated datasets
* Select numeric variables dynamically
* Explore regression outcomes interactively

Technologies Used

* R
* Shiny
* ggplot2
* dplyr
* tidyr
* broom
* bslib

Installation

Clone the Repository

git clone https://github.com/yourusername/moderation-regression-simulator.git
cd moderation-regression-simulator

Install Dependencies

Open R or RStudio and run:

install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "purrr",
  "ggplot2",
  "broom",
  "htmltools"
))

Run the App

shiny::runApp("app.R")

Example Use Cases

* Teaching moderation regression concepts
* Demonstrating interaction effects in statistics courses
* Exploring simulated research scenarios
* Testing regression behavior under different noise conditions
* Learning how moderators influence relationships between variables

Project Structure

.
├── app.R
├── README.md
└── data/

Future Improvements

Potential future additions may include:

* Mediation analysis support
* Three-way interactions
* Exportable regression reports
* Advanced diagnostics
* Structural equation modeling integration
* Additional visualization options

License

This project is provided for educational and research purposes.

Author

Neo Draxler
