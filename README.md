# ExperimentDataProcessing

This respository contains scripts used to process and validate the data collected by Frinex experients.

# JSON REST Interface
The Frinex administration system has a REST interface which can be accessed for each experiment via the endpoints: stimulusresponses, tagpairevents, tagevents, participants, groupdata, audiodata, screenviews and timestamps.

The JSON response follows HATEOAS rules and is therefore self documenting. See the "_links" section of the JSON data for further information.

All connections will require authentication via the same credentials as the normal admin pages.

# R Example
library(jsonlite)

stimulusresponses <- fromJSON("https://\<frinex-url\>/\<experiment-admin\>/stimulusresponses")

paste(stimulusresponses$_embedded$stimulusresponses$stimulusId, ":", stimulusresponses$_embedded$participants$userAgent)

# CURL Example
\# login and store the requred JSESSIONID in a file called cookies.txt

curl -i -X POST -d username=############### -d password=############### -c cookies.txt https://\<frinex-url\>/\<experiment-admin\>/login

\# select data from the REST service, authenticated with the required JSESSIONID from the file cookies.txt

curl -i --header "Accept:application/json" -X GET -b cookies.txt https://\<frinex-url\>/\<experiment-admin\>/stimulusresponses
