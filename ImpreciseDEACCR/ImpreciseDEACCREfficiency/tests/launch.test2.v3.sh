#!/bin/sh
cd ../src
R --slave --vanilla --file=ImpreciseDEACCREfficiencyCLI_XMCDAv3.R --args "${PWD}/../tests/in2.v3" "${PWD}/../tests/out2.v3"
