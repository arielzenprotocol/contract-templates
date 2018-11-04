#!/bin/bash

PROGRAM="../src/bin/Debug/contract-templates.exe"
VIEWFILE="Bet.json"

$PROGRAM extract Bet.fst -v $VIEWFILE
sed -i 's/Bet.fst/Bet2.fst/g' $VIEWFILE
sed -i 's/{ "name" : "ticker", "value" : "AMD", "type" : "string" }/{ "name" : "ticker", "value" : "XYZ", "type" : "string" }/g' $VIEWFILE
sed -i 's/{ "name" : "strike", "value" : 65000, "type" : "uint64" }/{ "name" : "strike", "value" : 1234, "type" : "uint64" }/g' $VIEWFILE
$PROGRAM generate Bet.fst -v Bet.json
echo "Please manually compare Bet.fst and Bet2.fst..."
