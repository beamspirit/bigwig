
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
});
