# News

# 0.4.4
8th July 2020

## Changes

* change dropdown from 'options' to 'settings'

# 0.4.3
3rd July 2020

* move user interface functions from DailyMeasure package to dMBillings
  + user interface functions found in `userInterface.R`

# 0.4.2
26th May 2020

* add .payerCode, payerCode and payerCodeR
  private/active/reactive versions of payerCode
  used by `$list_billings` and `$list_services`
* changes for check_subscription, no longer changes
  date if no valid subscription for chosen clinicians
  and date range
* `list_services_allclinicians`
  payerCode argument
  removal of 'lazy' mode
  removal of reactive version 'services_list_allcliniciansR'
* bugfix - correction of HTML popup display of billing if there
  is only a single row of billing to display (unusual edge case...)

# 0.4.1
3rd May 2020

* `$gpmp_list` check for recent GPMP

# 0.4.0
29th April 2020

## Changes

* tag_billings_list adds billingtag and billintag_print to a billing list
* as a result, list_billings no longer has a screentag/screentag_print argument
  and list_billings not longer has a rawbilling option
  list_billings returns MBSitem, Description and Provider lists for
  each appointment row

# 0.3.4
26th April 2020

* rawbilling option for list_billings
* payerCode filtering option for list_billings
* introduction changes for COVID-19 bulk-billing incentive
  and list_billings payerCode filtering.
  Note that COVID-19 code is currently entirely within
  'DailyMeasure' frontend.

# 0.3.3
10th April 2020

* add rintrojs introduction (steps_introductioin_df)

# 0.3.2
8th April 2020

* fix error where list_billings only shows billings for patients in appointment book

# 0.3.1
28th March 2020

* fix error if no internalID in list_billings

# 0.3.0
19th March 2020

* InternalID filtering available for list_services and list_services_allclinicians
* add Provider return to list_services_allclinicians

# 0.2.2
4th March 2020

* reduce database queries with $list_services

0.2.1
3rd March 2020

* better filtered database retrieval in $list_services

# 0.2.0
9th February 2020

* access dMeasure$check_subscription (subscription restrictions)
