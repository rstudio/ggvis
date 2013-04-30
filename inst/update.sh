#!/bin/bash

set -e
cd www/lib
wget https://raw.github.com/trifacta/vega/master/vega.js
wget https://raw.github.com/trifacta/vega/master/vega.min.js
wget https://github.com/mbostock/d3/raw/master/d3.js
wget https://github.com/mbostock/d3/raw/master/d3.min.js