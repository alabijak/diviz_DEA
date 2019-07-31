#!/bin/sh
cd ../src
R --slave --vanilla --file=HierarchicalDEACCRPreferenceRelationsCLI_XMCDAv2.R --args "${PWD}/../tests/in4.v2" "${PWD}/../tests/out4.v2"
