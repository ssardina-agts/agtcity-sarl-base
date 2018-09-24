#!/bin/bash

mvn -o exec:java | grep -v -e  \<.*\> -e WARNING -e '^ sent' -e '^ received'


