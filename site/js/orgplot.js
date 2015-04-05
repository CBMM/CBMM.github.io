function setToArray(s){
    var a = [];
    s.forEach(function(x) {a.push(x);});
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

// Utility function
function headingTags(h,s){
    h.tags.forEach(function(t) {s.add(t);});
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
        .text( function(d) {return d;});

    t.enter().append("div")
        .text( function(d) {return d;});

    var nHeadings = data.documentHeadings.length;
    hs = _.zip(_.range(nHeadings), d.documentHeadings);

    //var y = d3.scale.ordinal()
    //        .domain(_.range(nHeadings))
    //        .rangeRoundBands([0,height], 0.1);
    var y = d3.scale.ordinal()
            .rangeRoundBands([0,height], 0.1)
            .domain( _.range(nHeadings) );

    var yAxis = d3.svg.axis()
            .scale(y)
            .orient("left");

    var x = d3.scale.linear()
            .domain([cfg.bigbang, cfg.bigcrunch])
            .range([0, width]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .ticks(2)
        .orient("//TODO: op");


    chart.append("g")
        .attr("class","x axis")
        .attr("transform","translate(0," + height + ")")
        .call(xAxis);

    chart.append("g")
        .attr("class","y axis")
        .call(yAxis);

    chart.selectAll(".headlineBar")
        .data(hs)
      .enter().append("rect")
        .attr("class","headingDiv2")
        .attr("stroke","rgba(0,0,0,0)")
        .attr("y", function(t){return(y(t[0]));})
        .attr("x", function(t){
            var l = headingTimeLimits(t);
            return x(l[0]);
        })
        .attr("height",y.rangeBand())
        .attr("width", function(t){
            var l = headingTimeLimits(t);
            return x(l[1]) - x(l[0]);
        });

});

function decorateHeadings(indAndHeading){
    var i = indAndHeading[0];
    var h = indAndHeading[1];
    console.log('TEST' + indAndHeading);
}
function headingTimeLimits(h){
    var lim  = [cfg.bigbang,cfg.bigcrunch];
    var plns = h[1].section.sectionPlannings;
    if (plns.hasOwnProperty('CLOSED')){
        lim[1] = orgTSToTime(plns.CLOSED.tsTime);
    }
    if (plns.hasOwnProperty('DEADLINE')){
        lim[1] = orgTSToTime(plns.DEADLINE.tsTime);
    }
    if (plns.hasOwnProperty('SCHEDULED')){
        lim[0] = orgTSToTime(plns.DEADLINE.tsTime);
    }
    return lim;
}

function orgTSToTime(ots){
    var ts;
    var ymd = ots.yearMonthDay;
    if (ots.hourMinute == undefined){
        ts = new Date(ymd.ymdYear,ymd.ymdMonth,ymd.ymdDay);
    } else {
        var hm = ots.hourMinute;
        ts = new Date(ymd.ymdYear,ymd.ymdMonth,ymd.ymdDay,hm[0],hm[1]);
    }
    return ts;
}
