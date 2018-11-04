#!/bin/bash

PROGRAM="../src/bin/Debug/contract-templates.exe"
VIEWFILE="Bet.json"

CLRPRE='\033[1;34m'
CLRPST='\033[1;35m'
NC='\033[0m' # No Color

testMsg() {
	echo -e ${CLRPRE}"("$1"...)"${NC}
}

afterMsg() {
	echo -e ${CLRPST}"("$1")"${NC}
}

testMsg "Testing a valid execution"
( set -x ; $PROGRAM extract Bet.fst -v $VIEWFILE )
sed -i 's/Bet.fst/Bet2.fst/g' $VIEWFILE
sed -i 's/{ "name" : "ticker", "value" : "AMD", "type" : "string" }/{ "name" : "ticker", "value" : "XYZ", "type" : "string" }/g' $VIEWFILE
sed -i 's/{ "name" : "strike", "value" : 65000, "type" : "uint64" }/{ "name" : "strike", "value" : 1234, "type" : "uint64" }/g' $VIEWFILE
( set -x ; $PROGRAM generate Bet.fst -v $VIEWFILE )
afterMsg "Please manually compare Bet.fst and Bet2.fst"

testMsg "Testing extracting contract not found"
( set -x ; $PROGRAM extract Notfound.fst )

testMsg "Testing extracting invalid contract"
( set -x ; $PROGRAM extract Invalid.fst )

testMsg "Testing generating view file not found"
( set x ; $PROGRAM generate Bet.fst -v Notfound.json )


