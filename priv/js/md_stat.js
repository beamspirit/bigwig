var chart;
var bandwidth;
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
    bandwidth = new Highcharts.Chart({
        chart: {
            renderTo: 'bandwidth',
            type: 'area'
        },
        title: {
            text: 'BandWidth Consumption Of Every MD Node'
        },
        xAxis: {
            categories: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
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
            pointFormat: '{series.name} consumed <b>{point.y:,.0f} k</b><br/>bandwidth in {point.x}'
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
            name: 'market_dispatch@dispatch.lk.com',
            data: []
        }, {
            name: 'market_dispatch2@dispatch.lk.com',
            data: []
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
                categories: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
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
                name: 'market_dispatch@dispatch.lk.com',
                data: []
            }, {
                name: 'market_dispatch2@dispatch.lk.com',
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
    console.log(msg);
    var clientData = [];
    var instrumentData = [];
    var series  = chart.series[0];
    var series1 = bandwidth.series[0];
    var node;
    for(var i = 0; i < msg.length; i++)
    {
        var array = msg[i];
        node  = array[0];
        var time  = array[1];
        var clientCount     = array[2];
        var instrumentCount = array[3];

        clientData.push([time - 1, clientCount]);
        instrumentData.push([time - 1, instrumentCount]);
        $("#counter").flipCounter("setNumber", clientCount);
    }
    $('#md_statistic').append('<p>' + node + '</p>');
    series.setData(clientData);
    series1.setData(instrumentData);
};  

