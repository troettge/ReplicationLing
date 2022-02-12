# ReplicationLing

> Preliminary title: **Assessing the replication landscape in experimental linguistics**

The study splits into two parts answering separate but related questions with regards to replications on the journal and on the study level, respectively:

***1) How often do journals mention the term replicat\*?***

This part of the study comprised a string matching technique with which we analysed over 50.000 articles from 98 journals.

***2) How many articles containing the term replicat\* are actual replications?***

For this part of the study, we manually coded 210 articles and checked for each whether and which kind of replication it contained. We also noted which kinds of changes were made in the design choices compared to the initial study and coded for factors like author overlap, language under investigation and citation counts.


## Data and File Overview
### 42ArticlesToCode
nothing to worry about, can be deleted as soon as TR has downloaded the folder
### data
This folder contains all data. `mention.csv` contains the rates of replication mention (i.e. hits for the search string "replicat*" on Web of Science) for 98 experimental linguistic journals. `guidelines.csv` contains the same journals coded for their journal impact factors, options for open access publishing and journal submission guidelines. 'Coding_Articles.csv' contains the 210 articles submitted to manual coding (factors like type of replication, author overlap, citation counts, language under investigation). `coded.csv` contains the same data, but as a ','-separated csv-file, while the former is a '|'-separated csv-file.
### jml_manuscript 
This folder contains the final report as well as other files for submission to Journal of Memory and Language.
### plots
This folder contains the figures used to visualize our main findings.
### preregistration
The preregistration file can be inspected here.
### scripts
This folder contains the R-scripts used to analyze the results and generate plots.
