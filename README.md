### Introduction

<br/>

This repo contains 2 Shiny apps: 
    
(i) BROWSE - to browse all available clinical trials at ACI and match patients to trials

(ii) CURATE - to add information about trials to our database to be used 

<br/>
<br/>

### Application Setup

The app can be run in 2 ways - with and without docker. 

<br/>

1. Docker usage: 

   Edit the `docker-compose.yml` file according to your filepaths. 

        docker-compose up --build

        docker-compose down


    This will perform all the setup necessary and initialize the db with trials.ndjson and use it for the 2 Shiny apps.

    <br/>

    Access the Shiny apps through a web browser at: 

    `http://127.0.0.1:3838/trial-match/R/browseApp/` 

    `http://127.0.0.1:3838/trial-match/R/curateApp/`

<br/>

2. Local usage:

    (a) Use the Dockerfile as a guide to install R, required R packages, and ShinyServer.

    (b) Make sure that your installation of ShinyServer has access to the source code of this repo. 

    (c) Install MongoDB using the instructions available [here](https://www.mongodb.com/docs/manual/installation/).

    (d) Run an active monogod process with the path to where the database files are stored. 
  
    ```
    mongod --dbpath /Users/aanu/Documents/repos/trial-match/data/demo
    ```

<br/>
<br/>

Edit the `config.yml` file according to where your data is stored. 