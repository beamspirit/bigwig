$(document).ready(function(){
    
}
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
function sendTxt(txt) {
    websocket.send(txt);
};
function onClose(evt) { 
};  

function onMessage(evt) { 
    $('#trace_log').prepend('<p>' + evt.data + '</p>');
};  

$("body").delegate("a.tracer", "click", function(e){
     var x1 = $(this).text();
     sendTxt(x1);
     e.preventDefault();
});