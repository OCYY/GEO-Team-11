---
title: Project Proposal by Team 11
author: Neo Jia Hui, Ong Chen Yu
date: '2020-10-10'
slug: project-proposal
categories:
  - R
tags: []
---

## IS415 Geospatial Analytics Project Proposal

### Title 
Assessing and improving age-friendliness in Singapore through GIS

### Problem and motivation
Aging population has always been a prominent issue in Singapore. The percentage of individuals over age 65 between 2010 and 2020 increased from 9% to 15.2% (link).  While there are many efforts by the government to solve this problem, it is inevitable that people will age and all of us will grow old one day. 

**However, in this small city-state, would its current urban landscape be enough to support its elderly?**

It is essential that relevant facilities and amenities are strategically built and readily available for elderly in this land-scarce Singapore.

### Project objectives
Through this project, we aim to build a Shiny application to:
- Analyse the supply and demand of age-friendly amenities and facilities in subzone planning area
- Analyse the availability of age-friendly amenities and facilities in subzone planning area
- Determine which areas needs more age-friendly amenities and facilities

### Literature reviews 

### Approach/ methodology 
Web scraping using R
- To gather data from websites where data is not readily available

Kernel Density Estimation 
- To calculate the density of aged population
- To calculate the density of age friendly amenities and facilities

Local Moran’s I 
- Identifying outliers or clusters of amenities and facilities

Getis-Ord Gi*
- Identifying hot and cold spots of elderly/ amenities and facilities

Geographically weighted correlation

### Selected Datasets
Singapore Residents by Planning AreaSubzone, Age Group, Sex and Type of Dwelling, June 2011-2020
- https://www.singstat.gov.sg/find-data/search-by-theme/population/geographic-distribution/latest-data

Master Plan 2014 Subzone Boundary (Web)
- https://data.gov.sg/dataset/master-plan-2014-subzone-boundary-web

Community Clubs
- https://data.gov.sg/dataset/community-clubs

Eldercare Services
- https://data.gov.sg/dataset/eldercare-services

CHAS Clinics
- https://data.gov.sg/dataset/chas-clinics

Upcoming and Completed Healthcare Facilities
- https://www.moh.gov.sg/upcoming-and-completed-healthcare-facilities

TCM Clinics
- https://prs.moh.gov.sg/prs/internet/profSearch/main.action?hpe=TCM

### Application Design Storyboard
![View Data tab with selectors](/post/2020-10-10-project-proposal_files/Storyboard-2.png)
View Data tab with selectors

![KDE Plot tab with filters](/post/2020-10-10-project-proposal_files/Storyboard-3.png)
KDE Plot tab with filters

### Tools and technologies

### Application system architecture
![](/post/2020-10-10-project-proposal_files/architechture.png)

### Scope of work

