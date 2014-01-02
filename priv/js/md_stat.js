$(document).ready(function(){
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
    var x  = (evt.data).split("\"");
    var x1 = x[1].split("\":");
    var x2 = x1[1].split("}");

    $('#md_statistic').append('<p>' + "MDNode:" + x1[0] + '</p>');
    $('#md_statistic').append('<p>' + "Count:"  + x2[0] + '</p>');

};  

