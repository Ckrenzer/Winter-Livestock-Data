#! /bin/gawk -f

BEGIN{
    FS = "\t"
    IGNORECASE = 1

    MAX_LINE_LENGTH = 200
    NUMLINES_TO_CHECK_FOR_DATE = 8
}

# Find the market and date
/<span class="medstr" style="color:white;">.*winter[[:space:]]*livestock/ \
       && !/http:/{
    market = "MARKET: " $0
    datetext = "DATE: " $0
    numlines_checked = 0
    # Keep checking subsequent lines if the date wasn't
    # on the line containing the market
    while(numlines_checked < NUMLINES_TO_CHECK_FOR_DATE \
          && datetext !~ /[0-9]/){
        numlines_checked++
        getline
        datetext = "DATE: " $0
    }
}

# Format the text
{
   if($0 ~ /estimate/) nextfile;
   if(length($0) < MAX_LINE_LENGTH && $0 ~ /[0-9]{2,4}[ \t][0-9.]{3,}[^0-9]*<br \/>/){
       # Remove characters after the price
       # (everything after {3,} in the conditional's pattern)
       gsub(/[^0-9]+$/, "", $0)

       # Fields with only one character are
       # likely typos and should be removed
       for(i = 1; i <= NF; i++){
           if(length($i) == 1) $i = ""
       }

       # Get or set buyer name
       # (condition 1 addresses lines that begin with quantity)
       # (condition 2 addresses space-delimited lines)
       if($1 !~ /^[0-9]/ && length($1) > 1){
           name = $1
       } else {
           $1 = name " " $1
       }

       # Update $0, remove fields that do not belong
       $(NF + 1) = datetext
       $(NF + 1) = market
       $(NF + 1) = "URL: " url

       print $0
   }
}

