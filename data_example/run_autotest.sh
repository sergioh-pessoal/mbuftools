#!/bin/bash
#----------------------------------------------------------------------
# Script to run a test to check if the mbufrtools is working correctly
#----------------------------------------------------------------------
# Processing:
# A bufrfile is decoded, than encoded, than decoded again. 
# Compare the contents of the original BUFR file with the final BUFR file to see
# if the test was successful. They should be the same.

original=AIRCRAFT.bufr
f2=$original.txt
f3=$f2.bufr
final=$f3.txt
echo " *** Runing BUFRDUMP to decode file ***"

set -x
bufrdump -i $original -o $f2
set +x

echo "*** Runing BUFRGEN to reencode the file ***"
set -x
bufrgen -i $f2 -o $f3
set +x

echo "*** Runing BUFRDUMP to decode the reencoded file ***"
set -x
bufrdump -i $f3 -o $final
set +x

