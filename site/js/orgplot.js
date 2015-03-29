p = d3.select(".my_div").selectAll("p")
    .data([4,8,15,23,42])
    .style("color", function(d,i){
        return "hsl(" + Math.random() * 360 + ",50%,50%)"})
    .style("font-size",function(d,i){ return d + "px"})

p.enter().append("p")
    .text(function(d) {return "my d:" + d}).transition()
    .style("color",function(d){
        return "hsl(" + Math.random() * 360 + ",50%,50%)"
    }).transition().delay(function(d,i) { return 1000;})
    .style("color",function(d){
        return "hsl(" + Math.random() * 360 + ",50%,50%)"
    });

p.exit().remove();

function headingTags(h,s){
    h.tags.forEach(function(t) {s.add(t)});
    h.subHeadings.forEach(function(sh){
        headingTags(sh,s);
    });
}

function allTags(d){
    tags = new Set();
    d.documentHeadings.forEach(function(h){
        headingTags(h,tags);
    });
    return tags;
}

var Config = function() {
    // The start time from POV of the org chart
    this.bang   = new Date('2014-01-01');
    // The end time from the POV of the org chart
    this.crunch = new Date('2016-01-01');
};
