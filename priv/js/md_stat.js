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
    $('#container').highcharts({
                title: {
                    text: 'Monthly Average Temperature',
                    x: -20 //center
                },
                xAxis: {
                    categories: [0, 1, 2, 3, 4, 5,
                        6, 7, 8, 9, 10, 11, 12, 13, 
                        14, 15, 16, 17, 18, 19, 20,
                        21, 22, 23]
                },
                yAxis: {
                    title: {
                        text: 'Count'
                    },
                    plotLines: [{
                        value: 0,
                        width: 1,
                        color: '#808080'
                    }]
                },
                tooltip: {
                    valueSuffix: '°C'
                },
                legend: {
                    layout: 'vertical',
                    align: 'right',
                    verticalAlign: 'middle',
                    borderWidth: 0
                },
                series: [{
                    name: 'market_dispatch@dispatch.lk.com',
                    data: [5, 20, 15, 30, 0, 11, 16, 25, 35, 80, 67, 38]
                }]
            });
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

