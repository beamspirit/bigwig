var chart;
var num = 1;

$(document).ready(function(){
    $('#md_title').html($('<h2>MD Statistic</h2>'));
    $("#counter").flipCounter({
        number:0, // the initial number the counter should display, overrides the hidden field
        numIntegralDigits:6, // number of places left of the decimal point to maintain
        numFractionalDigits:0, // number of places right of the decimal point to maintain
        digitClass:"counter-digit", // class of the counter digits
        counterFieldName:"counter-value", // name of the hidden field
        digitHeight:40, // the height of each digit in the flipCounter-medium.png sprite image
        digitWidth:30, // the width of each digit in the flipCounter-medium.png sprite image
        imagePath:"/static/images/flipCounter-medium.png", // the path to the sprite image relative to your html document
        easing: false, // the easing function to apply to animations, you can override this with a jQuery.easing method
        duration:10000, // duration of animations
        onAnimationStarted:false, // call back for animation upon starting
        onAnimationStopped:false, // call back for animation upon stopping
        onAnimationPaused:false, // call back for animation upon pausing
        onAnimationResumed:false // call back for animation upon resuming from pause
    });
    chart = new Highcharts.Chart({
            chart: {
                type: 'column',
                defaultSeriesType: 'spline',
                renderTo: 'charts'
            },
            title: {
                text: 'MD Statistic'
            },
            xAxis: {
                categories: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]
            },
            yAxis: {
                min: 0,
                title: {
                    text: 'Total Count On Every MD Node'
                },
                stackLabels: {
                    enabled: true,
                    style: {
                        fontWeight: 'bold',
                        color: (Highcharts.theme && Highcharts.theme.textColor) || 'gray'
                    }
                }
            },
            legend: {
                align: 'right',
                x: -70,
                verticalAlign: 'top',
                y: 20,
                floating: true,
                backgroundColor: (Highcharts.theme && Highcharts.theme.legendBackgroundColorSolid) || 'white',
                borderColor: '#CCC',
                borderWidth: 1,
                shadow: false
            },
            tooltip: {
                formatter: function() {
                    return '<b>'+ this.x +'</b><br/>'+
                        this.series.name +': '+ this.y +'<br/>'+
                        'Total: '+ this.point.stackTotal;
                }
            },
            plotOptions: {
                column: {
                    stacking: 'normal',
                    dataLabels: {
                        enabled: true,
                        color: (Highcharts.theme && Highcharts.theme.dataLabelsColor) || 'white'
                    }
                }
            },
            series: [{
                name: 'John',
                data: [5, 3, 4, 7, 2]
            }, {
                name: 'Jane',
                data: [2, 2, 3, 2, 1]
            }, {
                name: 'Joe',
                data: [3, 4, 4, 2, 5]
            }{
                name: 'market_dispatch@dispatch.lk.com',
                data: []
            }]
        });
    connect("/md/stream");
});

function connect(to)
{
       var host = document.location.host;
       websocket = new WebSocket("ws://"+host+to);
       websocket.onopen = function(evt) { onOpen(evt) }; 
       websocket.onclose = function(evt) { onClose(evt) }; 
       websocket.onmessage = function(evt) { onMessage(evt) }; 
};  
      
function onOpen(evt) { 
};  

function onClose(evt) { 
};  

function onMessage(evt) {
    document.getElementById("md_statistic").innerHTML="";
    var msg = JSON.parse(evt.data);
    for(var node in msg)
	  {
    	$('#md_statistic').append('<p>' + node + '</p>');
    	$("#counter").flipCounter("setNumber", msg[node]);
	  }
};  

function getForm () {
    chart.series[2].addPoint([0, num], true, false);
    num++;
}

