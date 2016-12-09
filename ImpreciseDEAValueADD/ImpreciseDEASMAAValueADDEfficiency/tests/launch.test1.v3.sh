#!/bin/sh
cd ../src
R --slave --vanilla --file=ImpreciseDEASMAAValueADDEfficiencyCLI_XMCDAv3.R --args "${PWD}/../tests/in1.v3" "${PWD}/../tests/out1.v3"
