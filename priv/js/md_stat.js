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
    var obj = evt.data;
    for(var node in obj)
	{
		console.log(obj);
    	$('#md_statistic').append('<p>' + "MDNode: " + node + '</p>');
    	$('#md_statistic').append('<p>' + "Count: " + obj[node] + '</p>');
	}
    

};  

