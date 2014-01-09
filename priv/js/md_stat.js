var chart;
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
    $('#bandwidth').highcharts({
        chart: {
            type: 'area'
        },
        title: {
            text: 'BandWidth Consumption Of Every MD Node'
        },
        xAxis: {
            categories: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]
        },
        yAxis: {
            title: {
                text: 'BandWidth Consumption'
            },
            labels: {
                formatter: function() {
                    return this.value +'k';
                }
            }
        },
        tooltip: {
            pointFormat: '{series.name} produced <b>{point.y:,.0f}</b><br/>warheads in {point.x}'
        },
        plotOptions: {
            area: {
                pointStart: 0,
                marker: {
                    enabled: false,
                    symbol: 'circle',
                    radius: 2,
                    states: {
                        hover: {
                            enabled: true
                        }
                    }
                }
            }
        },
        series: [{
            name: 'market_dispatch1@dispatch.lk.com',
            data: [6 , 11, 32, 11, 23, 36, 64,
                10, 14, 20, 30, 46, 64, 98, 15, 20, 24,
                27, 29, 31, 31, 32, 31, 29]
        }, {
            name: 'market_dispatch2@dispatch.lk.com',
            data: [5, 25, 50, 12, 15, 20, 42, 66, 86, 10, 16, 24, 33,
            42, 52, 61, 70, 39, 93, 38, 43, 13, 78,
            15]
        }]
    });
    chart = new Highcharts.Chart({
            chart: {
                renderTo: 'charts',
                type: 'column'
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
                name: 'market_dispatch1@dispatch.lk.com',
                data: [5, 3, 4, 6, 7, 8, 10, 15, 6, 11, 13, 16, 21, 14, 12, 7, 50, 24, 23, 20, 17, 22, 17, 15]
            }, {
                name: 'market_dispatch2@dispatch.lk.com',
                data: [3, 4, 4, 5, 3, 4, 6, 7, 8, 10, 15, 6, 11, 13, 16, 21, 14, 12, 7, 50, 24, 23, 20, 17]
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
        var series  = chart.series[0];
        var data = [];
        data.push([0, 4]);
        data.push([1, 7]);
        data.push([2, 8]);
        data.push([3, 9]);
        data.push([4, 10]);
        data.push([5, 13]);
        data.push([6, 14]);
        data.push([7, 9]);
        data.push([8, 8]);
        data.push([9, 16]);
        data.push([10, 22]);
        data.push([11, 24]);
        data.push([12, 18]);  
        data.push([13, 29]);
        data.push([14, 16]);
        data.push([15, 22]);
        data.push([16, 11]);
        data.push([17, 44]);
        data.push([18, 33]);
        data.push([19, 19]);
        data.push([20, 24]);
        data.push([21, 23]);
        data.push([22, 13]);
        data.push([23, msg[node]]);
        series.setData(data);
	  }
    

};  

