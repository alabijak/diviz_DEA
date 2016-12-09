#!/bin/sh
cd ../src
R --slave --vanilla --file=ImpreciseDEASMAAValueADDPreferenceRelationsCLI_XMCDAv2.R --args "${PWD}/../tests/in3.v2" "${PWD}/../tests/out3.v2"
