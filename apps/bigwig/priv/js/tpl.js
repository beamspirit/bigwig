

tpl = (function() {
  var title = function(title) {
    $('#title').text(title);  
  }
  title("Loading...");  

  var host = document.location.host;
  var sock = new WebSocket("ws://"+host+"/wh/ws");
  sock.onopen = function() {
  }
  var visit = function(href) {
    sock.send(JSON.stringify({visit:href}));
  }

  var href_click = function(href) {
    return function() {
      visit(href); 
      return false;
    }
  }
  var li = function(type, p, data) {
    var id = data.id;
    var li = $('li[data-id='+id+']', p);
    if(li.length==0) {
      li = $('li._tpl',p).clone(false, false);
      li.attr('data-id', id);
      li.removeClass('_tpl');
      p.append(li);
    }
    var permalink = $('.permalink', li);
    if(permalink) {
      var uniq = {type: type, id: id};
      var href = "wh/"+type+"/"+id;
      permalink.attr('href', href);
      permalink.bind('click', href_click(uniq));
    }
    for(var k in data) {
      li.data(k, data[k]);
      var child = $("."+k, li);
      if(child) {
        $("."+k, li).text(data[k]);
      }
    }
  } 
  var update = function(k, data) {
    var el = $('#'+k);
    var jt = typeof data;
    if(jt == "string") {
      el.text(data);
    } else if(jt == "object") {
      for(var i=0; i<data.length; i++) {
        li(k, el, data[i]);
      }
    }
  }
  sock.onmessage = function(msg) {
    var json = $.parseJSON(msg.data);
    console.log(json);
    title(json.title);
    for(var k in json) {
      update(k, json[k]);
    }
  }
  sock.onclose = function() {

  }
  return {
    visit: visit
  }
})();
