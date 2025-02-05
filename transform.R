#+ Merging and appending data frames
#+ Feb 5, 2025

# load packages
  library(tidyverse)


# APPEND ------------------------------
#+ Add new obs/cases to a frame
#+ command: bind_rows()
#+ Requires:
#+ - same var names for matched vars
#+ - same var types for matched vars

# two data frames to combine
  cd1 <- readxl::read_excel('cds Alab NewY.xlsx')
  cd2 <- readxl::read_excel('cds NorthC Wyo.xlsx')

## Add to original (requires same col names)  
  cds <- bind_rows(cd1, cd2)
  cdsB <- bind_rows(
    list(a = cd1, b = cd2), 
    .id = 'source' # adds id variable for source
  )

#+ if it doesn't work, check var names and fix (DistNum?)


# MERGE/JOIN --------------------------
#+ A mutating join to add variables
#+ Matches cases by unique ids

## Data to incorporate
  states <- read_csv('states.csv')
  inc <- read_csv('incumb.csv')
  demog <- read_csv('cddem.csv')

## Merge two frames by unique identifier
  df1 <-
    left_join(
      cds,    # df 1
      demog,  # df 2
      by = join_by(District) # unique id var
    )
#+ note what happened to 'party' variable in that join.
#+ drop duplicates up front where possible (select(-party))

#+ right_join() ... keeps only records in second frame
#+ inner_join() ... keeps only records in BOTH frames
#+ full_join() ... keeps all records from each frame

## Where id vars don't match: merge sate data
  df2 <-
    left_join(
      df1, 
      states, 
      by = join_by(state == stcode)
    ) 
# can you find the new additions?


## where multiple columns jointly identify cases: join with inc    
  df3 <-
    left_join(
      df2, 
      inc,
      by = join_by(inc.first, inc.last)
    )

#+ we didn't do great with names, and we have duplicates
#+ we'd need to clean up now or go back and clean as we code


# PIVOT/RESHAPE -----------------------
## data
  wdi <- readxl::read_excel('WDI pull.xlsx', na = '..')

## lengthening pivots -----------
  ## small frame as example
  mx <- 
    wdi %>%
    filter(
      cName == 'Mexico',
      vCode == 'EN.ATM.CO2E.PP.GD'
    )

  ## "tidy" by lengthening
  mx_long <-
    mx %>%
    pivot_longer(
      cols = 5:15, # try different specs
      names_to = 'Year', # new var recording col names
      names_transform = list(Year = as.integer),
      values_to = 'CO2emit'  # new var recording the data
    )
  
  ## widening pivot --------------
  ## start with small frame for example
  y2005 <-
    wdi %>%
    select(cName:`2005`)  # use `...` when var name is a number
  
  ## widen the data
  wdi_wide <-
    y2005 %>%
    pivot_wider(
      names_from = c(vCode, vName), # try only one and see what happens
      values_from = `2005`,
      names_glue = '{vCode}' # try w/o this line
    )


# FIX THE WDI -------------------------
# lengthen first
  wdi_long <-
    wdi %>%
    pivot_longer(
      cols = -c(1:4),
      names_to = 'year',
      names_transform = list(year = as.integer),
      values_to = 'scores'
    )

# widen from there
  wdi2 <-
    longwdi %>%
    select(-vName) %>%
    pivot_wider(
      names_from = vCode,
      values_from = scores
    )


#+ Why bother with pivots?
#+ widen for making tables
#+ lengthen for graphs and basic tidy

#+ TRY ON YOUR OWN:  
#+ Compare Brazil and Canada in C02 emissions
#+ Select the countries and vars from wdi2
#+ Use group_by() and summarize() to get your stats
#+ Pivot to make a nicer table

