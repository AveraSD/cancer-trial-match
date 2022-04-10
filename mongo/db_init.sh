#!/bin/sh

cat /data/trials/*.full.ndjson | \
mongoimport \
    --collection ClinicalTrials \
    --db aci \
    --uri "mongodb://127.0.0.1:27017"
    