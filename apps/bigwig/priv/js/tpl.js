

tpl = (function() {
  var title = function(title) {
    $('#title').text(title);  
  }
  title("Loading...");  

  var li = function(p, el) {
    var id = el.id
    var li = $('li[data-id='+id+']', p);
    if(li.length==0) {
        li = $('li._tpl',p).clone(false, false);
        li.removeClass('_tpl');
        p.append(li);
    }
    li.text(id);
  } 
  var host = document.location.host;
  var sock = new WebSocket("ws://"+host+"/wh/ws");
  sock.onopen = function() {

  }
  sock.onmessage = function(msg) {
    var json = $.parseJSON(msg.data);
    console.log(json);
    title(json.title);
    var apps = $('#apps');
    for(var i=0; i<json.apps.length; i++) {
      li(apps, json.apps[i]);
    }
  }
  sock.onclose = function() {

  }
  return {
  }
})();
