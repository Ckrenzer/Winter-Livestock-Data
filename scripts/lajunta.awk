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
        if($0 ~ /junta/) market = "MARKET: " $0
        datetext = "DATE: " $0
    }

    # The records with market report data begin
    # on the lines following the date line
    startrecord = FNR
}

# Insert data
FNR == startrecord, FNR == EOF {
   if($0 ~ /estimate/) nextfile;
   if($0 ~ /[0-9]{2,4}[ \t]+[0-9.]{3,}[^0-9]*<br \/>/ \
      && length($0) < MAX_LINE_LENGTH){

       $(NF + 1) = datetext " " market " URL: " url
       print $0
   }
}

