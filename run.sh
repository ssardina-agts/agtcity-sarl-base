#!/bin/bash

mvn exec:java | grep -v -e  \<.*\> -e WARNING -e '^ sent' -e '^ received'


