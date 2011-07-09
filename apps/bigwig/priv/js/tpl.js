

var TPL = (function() {
  var title = function(title) {
    $('#title').text(title);  
  }

  var host = document.location.host;
  var sock = null;
  
  var onopen = function() {
  }

  var visit = function(href) {
    sock.send(JSON.stringify({visit:href}));
  }

  var nn = function(d) {
    if(d < 10) {
      return "0" + d;
    }
    return d;
  }

  var href_click = function(href) {
    return function() {
      visit(href); 
      return false;
    }
  }

  var str = function(v) {
    var t = type(v);
    if(t == "pid") {
      return v.data;
    } else if(t == "date") {
      var d = v.data;
      return d[0] + "-" + nn(d[1]) + "-" + nn(d[2])
     + " " + nn(d[4]) + ":" + nn(d[5]);
    }
    return v;
  }

  var updateChildren = function(el, data) {
    for(var k in data) {
      var v = str(data[k]);
      el.data(k, v);
      var child = $("."+k, el);
      if(child) {
        $("."+k, el).text(v);
      }
    }
  }

  var type = function(v) {
    var t = typeof v;
    if(t == "object") {
      if(v['_type'] != undefined) {
        t = v['_type'];
      }
    }
    return t;
  }

  var li = function(type, p, data) {
    var id = data.id;
    var li = $('li[data-id='+id+']', p);
    if(li.length==0) {
      li = $('li._tpl',p).clone(false, false);
      li.attr('data-id', id);
      li.removeClass('_tpl');
      if(p.attr('data-sort')=='desc') {
        p.prepend(li);
      } else {
        p.append(li);
      }
    } else {
      li = li.first();
    }
    var permalink = $('.permalink', li);
    if(permalink) {
      var uniq = {type: type, id: id};
      var href = "wh/"+type+"/"+id;
      permalink.attr('href', href);
      permalink.bind('click', href_click(uniq));
    }
    updateChildren(li, data);
  } 

  var update = function(k, data) {
    var el = $('#'+k);
    if(el.length==0) {
      el = $('*[data-id='+k+']').first();
    }
    var jt = type(data);

    if(jt == "string") {
      el.text(data);
    } else if(jt == "object") {
      if(data[0]) {
        for(var i=0; i<data.length; i++) {
          li(k, el, data[i]);
        }
      } else {
        updateChildren(el, data);
      }
    }
  }

  var onmessage = function(msg) {
    var json = $.parseJSON(msg.data);
    console.log(json);
    for(var k in json) {
      update(k, json[k]);
    }
  }
  var onclose = function() {

  }
  return {
    visit: visit,
    connect: function(to) {
      sock = new WebSocket("ws://"+host+to);
      sock.onopen = onopen
      sock.onmessage = onmessage
      sock.onclose = onclose
    }
  }
})();
