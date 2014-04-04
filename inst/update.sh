#!/bin/bash

set -e

cd www/lib

cd vega
curl -O https://raw.github.com/trifacta/vega/master/vega.js
curl -O https://raw.github.com/trifacta/vega/master/vega.min.js

cd ../d3
curl -O https://raw.github.com/mbostock/d3/master/d3.js
curl -O https://raw.github.com/mbostock/d3/master/d3.min.js

cd ../jquery
curl http://code.jquery.com/jquery-1.11.0.min.js > jquery.min.js
curl http://code.jquery.com/jquery-1.11.0.js > jquery.js

cd ../../..
