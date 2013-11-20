
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
});