### Introduction

This repo contains 2 Shiny apps: 
    
(i) BROWSE - to browse all available clinical trials at ACI and at match patients to trials

(ii) CURATE - to add information about trials to our database to be used as above

<br/>

### Setup

The clinical trials database is a MongoDB that is run through Docker. 

    docker-compose up --build

    docker-compose down

<br/>


This will perform all the setup necessary and initialize the db with trials.ndjson and use it for the 2 Shiny apps.

<br/>

### Run Apps
Access the Shiny apps through a web browser at: 

`http://127.0.0.1:3838/trial-match/R/browseApp/` 

`http://127.0.0.1:3838/trial-match/R/curateApp/`  