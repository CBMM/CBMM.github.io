function setToArray(s){
    var a = [];
    s.forEach(function(x) {a.push(x)});
    return a;
}

var Config = function() {
    // The start time from POV of the org chart
    this.bigbang   = new Date('2014-01-01');
    // The end time from the POV of the org chart
    this.bigcrunch = new Date('2016-01-01');
    // Time of viewing
    this.now       = undefined;
};

var cfg = new Config();
cfg.now = Date.now();

var margin = {top: 10, bottom: 50, left: 50, right: 50},
    width  = 500 - margin.left - margin.right,
    height = 300 - margin.top  - margin.bottom;

var x = d3.scale.linear()
         .domain([cfg.bigbang, cfg.bigcrunch])
         .range([0, 300]);

var xAxis = d3.svg.axis()
    .scale(x)
    .ticks(1000000)
    .orient("top");

// Utility function
function headingTags(h,s){
    h.tags.forEach(function(t) {s.add(t)});
    h.subHeadings.forEach(function(sh){
        headingTags(sh,s);
    });
}

// Utility function
function allTags(d){
    tags = new Set();
    d.documentHeadings.forEach(function(h){
        headingTags(h,tags);
    });
    return tags;
}


// Setup headlines
var chart = d3.select(".headlinesChart")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform","translate(" + margin.left + "," + margin.top + ")");

d3.json("todo.json", function(error,data){

    // Setup tag buttons
    t = d3.select(".tagsDiv").selectAll("div")
        .data( setToArray(allTags(data)) )
        .text( function(d) {return d});

    t.enter().append("div")
        .text( function(d) {return d} )

    chart.append("g")
        .attr("class","x axis")
        .attr("transform","translate(0," + height + ")")
        .call(xAxis);
    //hs = d3.select(".headlinesDiv").selectAll("div")
    //     .data(data.documentHeadings);

    chart.selectAll(".bar")
        .data(data.documentHeadings)
      .enter().append("div")
        .attr("class","headingDiv")
        .text( function(dh) {return dh.title} )
        .attr("x", 100)
        .attr("y", 100)
        .attr("fill","rgba(0,0,0,0)")
        .attr("stroke","rgba(0,0,0,1)")
        .attr("height",20)
        .attr("width", 100);


});
