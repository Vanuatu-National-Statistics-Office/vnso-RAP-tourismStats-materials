# Secure outputs from the trade statistics analysis pipeline (not public by default)

## Suggested naming convention

**TYPE_STATUS_SOURCE_DESC_DATE.suffix**

- **TYPE**: [OPN|SEC] to show whether this is an open (OPN) or secure (SEC) dataset
- **STATUS**: [RAW|PROC|FINAL] to show whether this is a raw (RAW), processed (PROC) or final (FINAL) dataset
- **SOURCE**: where the data came from (ASY for Asycuda, for example)
- **DESC**: brief [`camelCase`](https://en.wikipedia.org/wiki/Camel_case) description of dataset
- **DATE**: [dd-mm-YY] formatted date of when dataset last updated
- **suffix**: [csv|xlsx|tsv|txt|etc.] file format abbreviation

## Dataset dictionary

Datasets currently present in secure folder:

- `SEC_FINAL_MAN_FinalTradeStatisticsTables_31-01-20_ORIGINAL.xlsx` - final formatted tables for monthly report up to 31st January 2020
- `SEC_FINAL_MAN_FinalTradeStatisticsTables_30-11-19_ORIGINAL.xlsx` - final formatted tables for monthly report up to 30th November 2020
- `SEC_FINAL_MAN_FinalTradeStatisticsTables_30-11-19_WORKING.xlsx` - working copy of the final formatted tables for monthly report up to 31st January 2020 - used while building reproducible analytical pipeline
