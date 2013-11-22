
BigWig = (function() {
  function activate(tool) {
    $('#main .tool').removeClass('active');
    $('#'+tool).addClass('active');
    $('#tools .tool').removeClass('current');
    $('#tools *[href='+tool+']').addClass('current');
    if(tool == "appmon") {
      APPMON.start('nonode@nohost');
    }
  }
  (function() {
    $('#tools .tool').click(function() {
      activate($(this).attr('href'));
      return false;
    });
    // load in applications
    $('#applications').bind('onupdate', function(e, data) {
      TPL.update('running', data.running);
      //TPL.update('loaded', data.loaded);
    });
    activate('dashboard');
  })();

  return {
    activate: activate
  };
})();

$(function() {
    $.getJSON('/vm', function(json) {
        $('#dashboard').append($('<h1>Dashboard dump</h1>'));
        $('#dashboard').append($('<pre class="dbdump">' + JSON.stringify(json, null, 4) + '</pre>'));
    });
    $.getJSON('/lager/status', function(json) {
        $('#lager_status').append($('<h2>Lager Status</h2>'));
        for(var i=0; i<json.ActiveTraces.length; ++i)
         {
          json.ActiveTraces[i]=($('<a class="tracer" href="#">' + json.ActiveTraces[i] + '</a>'))[0].outerHTML;
         }
         var jsonText=JSON.stringify(json, null, 4);
         var result=jsonText.replace(/\\"/g,"\"");
         var result1=result.replace(/  /g,"");
        $('#lager_status').append($('<pre class="dbdump">' + result1 + '</pre>'));
    });
    connect("/lager/stream");
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