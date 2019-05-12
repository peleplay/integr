## Test environments
* local win7 install, R 3.6.0
* ubuntu (14.04) on Travis CI, R 3.6.0
* local ubuntu 18.04, R 3.5.3

## R CMD check results
There were no ERRORs, no WARNINGs, no NOTEs

## Downstream dependencies
Currently there are no downstream dependencies.

## Resubmission 
This is the resubmission of the first submission. The following has been fixed
in this version (according to the request of the CRAN team member Matthias
Sterrer):

* Updated the DESCRIPTION file so that package and software names are put in 
single quotes, the Description field does not start with the package name, the
reference link is at the end of the field, and 'data.frame' is replaced with
'data frame' so that it refers to an object rather than to a method.

* Updated ALL .Rd files so that there is at least one executable example, with
excepiton of igToPS() (as it takes more than 5 seconds to execute)

* Updated output functions to prevent default writing to a user 
directory without explicit user's input. 
