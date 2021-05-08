# Winter-Livestock-Data
Collects data from Winter Livestock La Junta Sale Tuesday reports each week and organizes them into a csv. Updated Weekly.

Please note that this repository can only be updated after the folks over at Winter Livestock release their market reports, meaning there will be a few day's lag between the auction and new data in this repository.


### UPDATE 2/2/2021 ----------------------------------------------
Winter Livestock changed their market report format for La Junta, CO. The "La Junta.R" script works well for market reports with the old format, in which the text looks well spaced with paragraphs on your browser. For those market reports that have no spacing between the values and look like one really long paragraph, use the "La Junta long format.R" script.

The version that is just one giant paragraph has no "\r" characters, so we have to use newline characters in the strsplit() call instead. That's the only difference between the files. Due to the way it is written, the "\r" is needed in the original format (the format "La Junta.R" deals with) and "\n" is needed in the long format.

Could that difference easily get turned into a function, or could both R scripts get thrown into different code chunks of the same Rmd file? Sure. But I think using two scripts is easier for the user. Feel free to disagree.


### UPDATE 2/5/2021 ----------------------------------------------
Further, there is now a script ("La Junta cleaning.R") that formats the data so that you no longer have to manually enter the column names each time you create a new file with these scripts.

In addition, the 99 categories of cattle are reduced to just six! Many types of cattle have very few observations, so they were lumped in with other groups.

Finally, there is a shiny app you can run to view an analysis of the data, and even predict the price of cattle!


### UPDATE 2/7/2021 ----------------------------------------------
What was once a simple project in web scraping has turned into my go-to dataset for different statistical techniques and a base to build my programming portfolio.

This repository is the first search result on Duck Duck Go for "winter livestock data"!!!


### UPDATE 2/11/2021
Updated the scraper so that you are no longer required to remove the csv file on your computer. Further, the cleaning script now arranges the data in chronological order.

### UPDATE 3/12/2021
The collection files have been replaced by a more comprehensive Rmd file. This Rmd file is capable of going through different market report IDs and finding La Junta market reports--we can now collect historical data! The number of observations has increased threefold. About five percent of the data is missing the date value, and I am unsure whether the date simply wasn't provided or if my scraper is missing something.

### UPDATE 3/13/2021
"La Junta Modeling.Rmd" now imputes missing values for the Reprod column using KNN.

### UPDATE 3/25/2021
"La Junta Collection.Rmd" no longer requires you to run all of collection() to update one week's worth of data.

### UPDATE 4/18/2021-4/19/2021
The collection() function is now better at identifying data in the market reports. About 6000 new entries of historical data are available. The collection() function is also capable of filtering out some repeat market reports (reducing repeated data).

Be mindful that some dates may be estimated. In cases where the date could not be determined (see CASE 4 in the collection() function), one week is added to the date of the previous sale.

### UPDATE 5/7/2021-5/8/2021
Now contains files performing ARIMA models on cattle prices. Expect file reorganization soon.
