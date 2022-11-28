# UPDATES
Major changes to this repo are listed with each update.
Updates are written arbitrarily, but--when something big changes--I usually know "it's time."
Think of this as a history lesson rather than a feature overview.


### UPDATE 11/28/2022
This update builds upon those from 11/26/2022:

1. lajunta.awk has been improved to extract the market
and date properly and now hits all sales without reading
any lines in the 'ESTIMATE' section.
1. The R package dependencies have been reduced to data.table,
stringr, and lubridate. These, along with curl and GNU awk being
environment variables, are the only requirements to run the
collection code.
1. The URL extraction process has been simplified to use
curl and awk instead of selenium, making the process
faster and more portable.
1. The bug overwriting wl_reportIDs.txt with the current
report ID instead of appending the current report ID has
been fixed. wl_reportIDs.txt is now sorted and must be
sorted to pass the raw validation step.
1. Added checks ensuring dates do not repeat at
the same market for any two reportIDs.
1. The sections of the collection process have been formalized
into three sections: raw, refine, and clean. The raw section
reads in the HTML and organizes the sales data into a list
of data frames. The refine section extracts the date
and market from the raw text of each report, preserving
the original raw text. The refine section ends by binding
the list into a single data frame, writing it to disk
(wl_raw.csv). The clean section performs attribute cleaning.
This overwrites values in the type, reprod, or buyer fields
and saves the results to disk (wl_market_reports.csv).
This is the final data set.

### UPDATE 11/26/2022
This update overhauls the collection process, moving
the collection files directly under **scripts/**.
The collection process itself has been revamped in
the following ways:

1. The scraper now parses raw HTML to identify relevant data
in the market reports instead of the results from rvest functions.
This reduces 3rd party package dependencies for scraping and
basic data formatting. Curl and awk now perform the initial
processing.
1. The number of scripts have been reduced and many helpers
are now consolidated into a single file.
1. The tests on the scraper have been removed in favor of
more robust error checks during the collection process.
1. Attribute cleaning is now separate from the initial
scrape. This allows for the preservation of information
lost during the old cleaning process--particularly
in the type field.
1. The market report ID (the number in the URL field) is now
used to determine the distinctness of a URL because you can
get to the same web page using different URLs so long as it
points to the correct report ID. Therefore,
data-info/reports/wl_reportIDs.txt has replaced data/urls.txt.
1. The automatic commits and pushes in control.R have been removed.
I will use local scripts to update the repository.
1. A gitignore file has been added.
1. Removed mentioning of the dashboard from README.md.

### UPDATE 6/5/2022
All archived output has been removed from the repository.

The names of files are being standardized--'La Junta Collection.R' is now 'control.R',
'La Junta Market Reports.csv' is now 'ljmr.csv',
'La Junta URLS.txt' is now 'urls.txt',
and file names no longer contain parentheses.

The file hierarchy was restructured to allow the repo to scale beyond La Junta, CO.

A new directory, tests/ was created to ensure the integrity of the data.
These tests write their results to the log file, preventing the automatic
committing and pushing of new data when called non-interactively.

### UPDATE 2/4/2022
The file structure of this repo has been simplified.
The 'Collection' and 'Dashboards' directories are now subdirectories of 'scripts', renamed to lowercase.

### UPDATE 8/31/2021
Changes revolve around streamlining automation of market report updates.

'La Junta Collection.Rmd' has been replaced by three R scripts.
'La Junta Collection.R' does the scraping and updating to the repo,
'testing.R' provides visual inspection on the data,
and 'La Junta url options.R' provides different parsings of the market report URLs to make scraping data easier in the future.

The changes made to 'La Junta Collection.R' mean the user has to make a few changes
to make the code work on his or her local device.
Namely, the section committing and pushing changes to GitHub should be ignored or removed.
The user may also need to set up RSelenium and FireFox before the this script
works verbatim--RSelenium is only used to get the current market report URL so
you may also ignore this code if you choose to get the URL yourself.

### UPDATE 8/21/2021
The Shiny app has been reworked to load in helper functions and other data in by sourcing dedicated scripts.
The linear and random forest models have been fitted before running and are now read in via an RDS file.
This drastically reduces start up time and prevents fitting on the full dataset
(as new data will not be included), which helps combat overfitting.
Finally, the app has been published on shinyapps.io!

### UPDATE 8/16/2021
The collection() function has been split up into several different scripts
to make the code easier to follow.
The algorithm has been improved to miss slightly fewer cases than previous implementations.
Similarly, the market report cleaning has been put into a function and moved
to a script that is called inside the collection() function.
This means that the 'before cleaning' csv will no longer be available.

### UPDATE 7/6/2021
An RPA is now used to scrape the Winter Livestock site for new data.
It is scheduled to run at 8:30pm on Thursdays.
The collection() function was updated to prevent previouly-used URLs from
adding data (in the event there was no sale on a particular week).

### UPDATE 7/1/2021
Added a new Shiny app serving as a dashboard for the Lajunta, CO market.

Further, many previous output files are being moved to the newly created Archived folder.
The Lajunta dashboard incorporates the 'greatest hits' of many of these files.
These files, in the state they are at the time of writing, will be kept in the
Archived folder instead of being removed in the event someone wants a bit
more detail about the methodologies used than the end result provided in the Lajunta dashboard.

### UPDATE 5/28/2021
Added all URLs to a text file to shorten the collection() function's runtime,
simplified the logic in the use and creation of csv files.

### UPDATE 5/17/2021
Yeah, actually, I will be keeping the HTML output in the repository.
It will be in it's own dedicated folder, 'Output'.

### UPDATE 5/7/2021-5/8/2021
Now contains a file performing ARIMA models on cattle prices.

The files have been reorganized so that the file names clearly indicate the contents of the file.

'La Junta Modeling.Rmd' has been renamed 'La Junta Classification Models.Rmd'
'La Junta Predictions.Rmd' has been renamed 'Overview.Rmd'
'Further Exploration.Rmd' has been renamed 'La Junta Price Estimation.Rmd'

New experiments with this data will be made behind the scenes.
When I create something interesting, it will be added to the repo.
Random thoughts will be exluded!

Finally, HTML output will now be ignored and removed from the repo.
If you want to see the results, either ask me or knit the documents yourself!

### UPDATE 4/18/2021-4/19/2021
The collection() function is now better at identifying data in the market reports.
About 6000 new entries of historical data are available.
The collection() function is also capable of filtering out some repeat market reports (reducing repeated data).

Be mindful that some dates may be estimated.
In cases where the date could not be determined (see CASE 4 in the collection() function),
one week is added to the date of the previous sale.

### UPDATE 3/25/2021
"La Junta Collection.Rmd" no longer requires you to run all of collection() to update one week's worth of data.

### UPDATE 3/13/2021
"La Junta Modeling.Rmd" now imputes missing values for the Reprod column using KNN.

### UPDATE 3/12/2021
The collection files have been replaced by a more comprehensive Rmd file.
This Rmd file is capable of going through different market report IDs and finding
La Junta market reports--we can now collect historical data!
The number of observations has increased threefold.
About five percent of the data is missing the date value, and I am unsure whether
the date simply wasn't provided or if my scraper is missing something.

### UPDATE 2/11/2021
Updated the scraper so that you are no longer required to remove the csv file on your computer.
Further, the cleaning script now arranges the data in chronological order.

### UPDATE 2/7/2021
What was once a simple project in web scraping has turned into my go-to dataset
for different statistical techniques and a base to build my programming portfolio.

This repository is the first search result on Duck Duck Go for "winter livestock data"!!!

### UPDATE 2/5/2021
Further, there is now a script ("La Junta cleaning.R") that formats the data
so that you no longer have to manually enter the column names each time you
create a new file with these scripts.

In addition, the 99 categories of cattle are reduced to just six!
Many types of cattle have very few observations, so they were lumped in with other groups.

Finally, there is a shiny app you can run to view an analysis of the data,
and even predict the price of cattle!

### UPDATE 2/2/2021
Winter Livestock changed their market report format for La Junta, CO.
The "La Junta.R" script works well for market reports with the old format,
in which the text looks well spaced with paragraphs on your browser.
For those market reports that have no spacing between the values and look like
one really long paragraph, use the "La Junta long format.R" script.

The version that is just one giant paragraph has no "\r" characters,
so we have to use newline characters in the strsplit() call instead.
That's the only difference between the files.
Due to the way it is written, the "\r" is needed in the original format
(the format "La Junta.R" deals with) and "\n" is needed in the long format.

Could that difference easily get turned into a function, or could both R scripts
get thrown into different code chunks of the same Rmd file?
Sure.
But I think using two scripts is easier for the user. Feel free to disagree.
