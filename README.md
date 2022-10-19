# FluxBandit

## Shiny app for interactive processing and calculation of greenhouse gas emissions 

### What is it

App used to perform processing of greenhouse gas flux measurements in chambers using interactive plots. The application supports both commercial (Los Gatos Research - ultraportable/microportable analyzers) and [DIY](https://bg.copernicus.org/articles/17/3659/2020/bg-17-3659-2020.pdf) type sensor systems.

The app takes the user through all the necessary steps to calculate CO<sub>2</sub> and CH<sub>4</sub> fluxes:

1.  Select sensor type

2.  Import data (‘.csv’ file)

3.  Adjust metadata

4.  Define measurement:

    -   Select time range using slider (coarse adjustment)

    -   Select time range interactively in *Overview plot* (fine adjustment)

    -   Inspect selection in *Zoom plot* and adjust if necessary

5.  Save measurement (repeat steps 2-4)

6.  Download data the results as a '.csv' file

![](fluxbandit_image.png)

### Running the app

There are two options for using the app:

1.  Run the app [online](https://kennethtm.shinyapps.io/FluxBandit/).

2.  Download this repository as a '.zip' file and run in e.g. RStudio. Make sure to have the required packages (*shiny*, *dplyr*, and *lubridate*) installed.

Try out the functionality using the two example files included in the 'demo/' folder.

### Release notes

v0.5 - Fixed an error in the flux calculation which resulted in incorrect values for CO<sub>2</sub> and CH<sub>4</sub> fluxes in the previous versions.

### Contact

Kenneth Thorø Martinsen (kenneth2810@gmail.com)
