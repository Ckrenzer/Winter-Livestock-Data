# Winter-Livestock-Data
Collects data from Winter Livestock La Junta Sale Tuesday reports each week and organizes them into a csv. Updated Weekly.

Please note that this repository can only be updated after the folks over at Winter Livestock release their market reports, meaning there will be a few day's lag between the auction and new data in this repository.

# Output

## Dashboards
To see the Lajunta dashboard, run the following code in your R console:

```
if(!require(shiny)) install.packages("shiny")
shiny::runGitHub(repo = "Winter-Livestock-Data",
                 username = "Ckrenzer",
                 subdir = "scripts/dashboards/Lajunta_Dashboard",
                 ref = "main")
```

The app will occasionally be hosted on [shinyapps.io](https://www.shinyapps.io/), so--if the app is live--you can use [this link](http://7phynv-connor0krenzer.shinyapps.io/La_Junta_Dashboard) to access it in your browser.


## Archived Output
To see the output for 'La Junta Price Estimation.Rmd', follow [this link](https://htmlpreview.github.io/?https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/Archived/Output/La-Junta-Price-Estimation.html).

To see the output for 'La Junta Classification Models.Rmd', follow [this link](https://htmlpreview.github.io/?https://raw.githubusercontent.com/Ckrenzer/Winter-Livestock-Data/main/Archived/Output/La-Junta-Classification-Models.html).
