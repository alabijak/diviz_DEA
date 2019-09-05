#! /bin/bash
# Usage:
#  HierarchicalDEASMAAValueADDEfficiency.sh [--v2|--v3] input_dir output_dir

# Adapt if needed.  R v3.x is required
# Use the full path here
R3=/usr/bin/R

if [ ! -x "${R3}" ]; then
  echo "Please edit '$0': R exec not found" >&2
  exit 1
fi
if [ ! "version 3" = "$(${R3} --version | head -1 | grep -o 'version [0-9]')" ]; then
  echo "Please edit '$0': $R3 is not R version 3.x" >&2
  exit 2
fi

# -- You normally do not need to change anything beyond this point --

if [ $# -lt 3 ]; then
  echo "Usage: $0 [--v2|--v3] input_dir output_dir" >&2
  exit 3
elif [ $1 = "--v3" ]; then
  R --slave --vanilla --file=src/HierarchicalDEASMAAValueADDEfficiencyCLI_XMCDAv3.R --args $2 $3
  ret=$?
elif [ $1 = "--v2" ]; then
  R --slave --vanilla --file=src/HierarchicalDEASMAAValueADDEfficiencyCLI_XMCDAv2.R --args $2 $3
  ret=$?
else
  echo "Usage: $0 [--v2|--v3] input_dir output_dir" >&2
  exit 4
fi


exit $ret
