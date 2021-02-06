# Winter-Livestock-Data
Collects data from Winter Livestock La Junta Sale Tuesday reports each week and organizes them into a csv.

This is sales data.

### UPDATE ------------------------------------------------------
Winter Livestock changed their market report format for La Junta, CO. The "La Junta.R" script works well for market reports with the old format, in which the text looks well spaced with paragraphs on your browser. For those market reports that have no spacing between the values and look like one really long paragraph, use the "La Junta long format.R" script.

The version that is just one giant paragraph has no "\r" characters, so we have to use newline characters in the strsplit() call instead. That's the only difference between the files. Due to the way it is written, the "\r" is needed in the original format (the format "La Junta.R" deals with) and "\n" is needed in the long format.

Could that difference easily get turned into a function, or could both R scripts get thrown into different code chunks of the same Rmd file? Sure. But I think using two scripts is easier for the user. Feel free to disagree.


Further, there is now a script ("La Junta cleaning.R") that formats the data so that you no longer have to manually enter the column names each time you create a new file with these scripts.

In addition, the 99 categories of cattle are reduced to just six! Many types of cattle have very few observations, so they were lumped in with other groups.