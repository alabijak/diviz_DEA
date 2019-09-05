#!/bin/sh
cd ../src
R --slave --vanilla --file=HierarchicalDEASMAACCREfficienciesCLI_XMCDAv3.R --args "${PWD}/../tests/in3.v3" "${PWD}/../tests/out3.v3"
