#!/bin/sh
cd ../src
R --slave --vanilla --file=HierarchicalDEASMAACCRPreferenceRelationsCLI_XMCDAv3.R --args "${PWD}/../tests/in4.v3" "${PWD}/../tests/out4.v3" "0"
