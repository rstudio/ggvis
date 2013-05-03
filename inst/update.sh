#!/bin/bash

set -e
cd www/lib
curl -O https://raw.github.com/trifacta/vega/master/vega.js
curl -O https://raw.github.com/trifacta/vega/master/vega.min.js
curl -O https://raw.github.com/mbostock/d3/master/d3.js
curl -O https://raw.github.com/mbostock/d3/master/d3.min.js
curl -O http://code.jquery.com/jquery-1.9.1.min.js
curl -O http://code.jquery.com/jquery-1.9.1.js
curl -O https://raw.github.com/mikechambers/ExamplesByMesh/master/JavaScript/QuadTree/src/QuadTree.js

