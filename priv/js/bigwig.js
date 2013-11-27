
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
    $('#start_trace').bind('click', function(event) {
      var trace=$('#tracer').val();
      var x = trace.split(":");
      var tracer = '';
      for(var i=0; i< x.length; ++i)
      {
        tracer = tracer + x[i] + ',';
      }
      tracer = tracer.substring(0,tracer.length-1);

      var url = '/lager/tracer/'+tracer;

      $.ajax({
              url:url,
              type:'PUT',
              success: function(resp){
                  alert('Add tracer success');
                  window.location.reload();
              }
            })  
    });
    $('#clear_all_traces').bind('click', function(event) {
      var url = '/lager/tracer/all';
      $.ajax({
              url:url,
              type:'DELETE',
              success: function(resp){
                  alert('Clear all traces success');
                  window.location.reload();
              }
            })  
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
function onClose(evt) { 
};  
function onMessage(evt) { 
    var x = (evt.data).split(":\"");
    var x1 = x[1].split(" ");
    var x2 = "";
    for(var i=4; i < x1.length; ++i)
    {
       x2 = x2+ x1[i];
    }
    $('#trace_dialog').append(   $('<tr></tr>')
                        .append('<td>'+x1[0]+x1[1]+'</td>')
                        .append('<td> </td>')
                        .append('<td>'+x1[2]+'</td>')
                        .append('<td> </td>')
                        .append('<td>'+x1[3]+'</td>')
                        .append('<td> </td>')
                        .append('<td>'+x2+'</td>')
                        );                      
};
function sendTxt(txt) {
    websocket.send(txt);
    $('<div id="trace_dialog" class="trace_dialog"></div>')
    .append(   $('<tr></tr>')
                        .append('<td>'+"Time"+'</td>')
                        .append('<td> </td>')
                        .append('<td>'+"Node"+'</td>')
                        .append('<td> </td>')
                        .append('<td>'+"Level"+'</td>')
                        .append('<td> </td>')
                        .append('<td>'+"Message"+'</td>')
                    )
    .dialog({
      width: 550,
      height: 650,
      title: 'routingkey(' + txt + ') &nbsp; ',
      close: function(event, ui) {
        closeSubscribe();
      },
      buttons: {
        "Close": function() { $(this).dialog("close"); },
        "Stop Trace": function() {}
      }
      })
};
function closeSubscribe(){
  websocket.send('kill');
}
$("body").delegate("a.tracer", "click", function(e){
     var x1 = $(this).text().split("<<");
     var x2 =x1[1].split(">>");
     var x3 = x2[0].replace(/\"/g, "");
     sendTxt(x3);
     e.preventDefault();
});