// We'll stick everything on "ggvis" global variable.
window.ggvis = window.ggvis || {};  // If already defined, just extend it

// This is where we'll store charts. Key = name, value = vega view
ggvis.charts = {};


// Socket communications

var pendingSendQueue = [];
var socket = ggvis.socket = new WebSocket('ws://' + window.location.host);
socket.onopen = function() {
  pendingSendQueue.forEach(function(msg) {
    socket.send(msg);
  });
};
socket.onmessage = function(evt) {
  // evt.data
};
socket.onerror = function(evt) {
  alert('Error: ' + evt.data);
};
socket.onclose = function() {
  $(window.document.body).css('backgroundColor', '#999');
};


function getSceneBounds(view) {
  return view.model().scene().items[0].bounds;
}

// Function that retrieves vega scene items using a hard coded path. This will
// obviously not work when our charts start getting interesting.
function getItems(view) {
  return view.model().scene().items[0].items[0].items;
}

// Register a chart with the ggvis system
ggvis.addChart = function(name, view) {
  ggvis.charts[name] = view;

  // Hook up brushing/linking event handlers
  subscribe(view);

  // Cache all items on view.itemIndex
  // This will need to be rebuilt if data can change.
  view.itemIndex = new ItemIndex({x: 0, y: 0, width: 1000, height: 1000});
  var items = getItems(view);
  for (var i = 0; i < items.length; i++) {
    view.itemIndex.add(items[i]);
  }
};

// Run the iterator over every registered chart.
// The iterator can take up to two parameters: view, and name
ggvis.eachChart = function(iterator) {
  var charts = ggvis.charts;
  for (var name in charts) {
    if (charts.hasOwnProperty(name))
      if (iterator(ggvis.charts[name], name) === false)
        break;
  }
};

// In the future, this will be really helpful for quickly detecting
// brushed items. However, QuadTree doesn't seem to work correctly
// (or at least I can't figure out how to use it).
function ItemIndex(bounds) {
  this.quadtree = new QuadTree(bounds, false, 8, 25);
  this.keys = {};
}
// Add a scene item
ItemIndex.prototype.add = function(item) {
  this.quadtree.insert({
    x: item.bounds.x1,
    y: item.bounds.y1,
    width: item.bounds.width(),
    height: item.bounds.height(),
    key: item.key
  });
  this.keys[item.key] = item;
};
// [DOESN'T WORK] Get all the items that fit in the rect. (TODO: provide
// options about what it means to be "in" the rect; intersecting? Enclosed?
// Center is contained?)
ItemIndex.prototype.getByRect = function(x, y, width, height) {
  var infos = this.quadtree.retrieve({
    x: x,
    y: y,
    width: width,
    height: height
  });
  var items = [];
  for (var i = 0; i < infos.length; i++) {
    items.push(this.keys[infos[i].key]);
  }
  return items;
};
// Given an item key, return the item, if it exists.
ItemIndex.prototype.getByKey = function(key) {
  return this.keys[key];
};


function mouseOffset(e) {
  var bounds = e.target.getBoundingClientRect();
  return {
    x: e.clientX - bounds.left,
    y: e.clientY - bounds.top
  };
}

function subscribe(view) {
  var $el = $(view._el);
  // Hook up handlers
  $el.on('mousedown.ggvis', function (event, item) {
    startBrushing(removePadding(mouseOffset(event)));
  });
  $el.on('mouseup.ggvis', function (event, item) {
    stopBrushing();
  });
  $el.on('mousemove.ggvis', function (event, item) {
    brushTo(removePadding(mouseOffset(event)));
  });

  var dragStart = null;

  // x/y coords are relative to the containing div. We need to account for the
  // padding that surrounds the data area by removing the padding before we
  // compare it to any scene item bounds.
  // TODO: Do we need to multiply by x/y ratio?
  function removePadding(point) {
    return {
      x: point.x - view.model().defs().padding.left,
      y: point.y - view.model().defs().padding.top
    };
  }

  function startBrushing(point) {
    // Clear all brushes and highlights
    ggvis.eachChart(function (view) {
      view.data({
        brush: [
          {x: 0, y: 0, width: 0, height: 0}
        ]
      });
      view.update();
    });

    // Record the start point
    dragStart = point
  }

  function stopBrushing() {
    dragStart = null;
  }

  function brushTo(point) {
    if (!dragStart)
      return; // we're not brushing right now

    var limits = getSceneBounds(view);

    // Calculate the bounds based on start and end points
    var end = point;
    var maxX = Math.min(Math.max(dragStart.x, end.x), limits.x2);
    var minX = Math.max(Math.min(dragStart.x, end.x), limits.x1);
    var maxY = Math.min(Math.max(dragStart.y, end.y), limits.y2);
    var minY = Math.max(Math.min(dragStart.y, end.y), limits.y1);
    var bounds = new vg.Bounds().set(minX, minY, maxX, maxY);

    // Update the brush bounding box
    view.data({brush: [
      {x: bounds.x1, y: bounds.y1, width: bounds.width(), height: bounds.height()}
    ]})

    // Find the items in the current scene that match
    var items = getItems(view);
    var matchingKeys = [];
    for (var i = 0; i < items.length; i++) {
      // TODO: Intersects vs. encloses
      if (bounds.intersects(items[i].bounds)) {
        matchingKeys.push(items[i].key);
      }
    }

    // Now perform highlighting on each chart
    ggvis.eachChart(function (view) {
      selectKeys(view, matchingKeys);
    });
  }
}

// Keys is an array of strings
function selectKeys(view, keys) {
  // Clear existing selection
  view.update();

  if (keys.length === 0)
    return;

  // Map the keys to items
  var matches = [];
  for (var i = 0; i < keys.length; i++) {
    matches.push(view.itemIndex.getByKey(keys[i]));
  }

  if (matches.length === 0)
    return;

  // Paint the matched items
  view.update({props: 'selected', items: matches});
}
