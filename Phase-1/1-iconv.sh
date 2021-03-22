#!/bin/bash

iconv -f EBCDIC-IT -t latin1 1-communes.dat &> tmp-communes.dat

tail +2 tmp-communes.dat &> 2-communes.dat

rm tmp-communes.dat 