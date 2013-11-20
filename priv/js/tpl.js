
/* 
 * <div id="stats">
 *   <span class="load"></span>
 *   <span class="processes"></span>
 * </div>
 *
 * TPL.connect('/path/to/websocket');
 *
 * WSPid ! js:term_to_json([
 *   {stats, [
 *     {load, 2},
 *     {processes, 25}
 *   ]}
 * ]).
 *
 */

var TPL = (function() {
  var host = document.location.host;
  var socks = {};
  
  var onopen = function() {
  }

  var visit = function(href) {
    //sock.send(JSON.stringify({visit:href}));
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

  var render = function(v) {
    var t = type(v);
    if(t == "pid") {
      return RENDERER.json(v);
    } else if(t == "date") {
      var d = v.data;
      return d[0] + "-" + nn(d[1]) + "-" + nn(d[2])
     + " " + nn(d[3]) + ":" + nn(d[4]) + ":" + nn(d[5]);
    }
    return v;
  }

  var updateChildren = function(el, data) {
    for(var k in data) {
      var v = render(data[k]);
      el.data(k, v);
      var child = $("."+k, el);
      if(child.length > 0 && v) {
        try {
          child.html(v);
        } catch(err) {
        }
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

  var li = function(p, data) {
    var id = data.id;
    var li = $('li[data-id='+id+']', p);
    var tpl = $('li._tpl',p).first();
    var processFun = tpl.data('process');
    var sort = p.attr('data-sort');
    if(li.length==0) {
      li = tpl.clone(false, false);
      li.attr('data-id', id);
      li.removeClass('_tpl');
      if(sort=='desc') {
        p.prepend(li);
      } else {
        p.append(li);
      }
      var limit = p.attr('data-limit');
      if(limit != undefined) {
        limit = parseInt(limit);
        var count = parseInt(p.attr('data-count'));
        if(count >= limit) {
          var all = $('li',p);
          if(sort=="desc") {
            var last = all.last();
            if(last && last.hasClass('_tpl')) {
              last = last.prev();
            }
            if(last) {
              last.remove();
            }
          } else {
            var first = all.first();
            if(first && first.hasClass('_tpl')) {
              first = first.next();
            }
            if(first) {
              first.remove();
            }
          }
        } else {
          count++;
        }
        p.attr('data-count', count);
      }
    } else {
      li = li.first();
    }
    var permalink = $('.permalink', li);
    if(permalink) {
      //var uniq = {type: type, id: id};
      //var href = "wh/"+type+"/"+id;
      //permalink.attr('href', href);
      //permalink.bind('click', href_click(uniq));
    }
    updateChildren(li, data);
    if(processFun) {
      processFun(li, data);
    }
  } 

  var update = function(k, data) {
    var el;
    if(typeof k == 'object') {
      el = k;
    } else {
      el = $('#'+k);
      if(el.length==0) {
        el = $('*[data-id='+k+']').first();
      }
    }
    var jt = type(data);
    if(jt == "string") {
      try {
        el.html(data);
      } catch(err) {
      }
    } else if(jt == "object") {
      if(data[0]) {
        for(var i=0; i<data.length; i++) {
          li(el, data[i])
        }
      } else {
        updateChildren(el, data);
      }
    }
    el.triggerHandler('onupdate', data);
  }
  var onjson = function(json) {
    for(var k in json) {
      update(k, json[k]);
    }
  }
  var onmessage = function(msg) {
    onjson($.parseJSON(msg.data));
  }
  var onclose = function() {
  }
  var fetch = function(url) {
    $.getJSON(url, onjson);
  }
  return {
    visit: visit,
    update: update,
    connect: function(to) {
      socks[to] = new WebSocket("ws://"+host+to);
      socks[to].onopen = onopen;
      socks[to].onmessage = onmessage;
      socks[to].onclose = onclose;
    },
    fetch: fetch
  }
})();

(function ($) {
  $.extend({      
    getQueryString: function (name, qs) {           
      var q = qs;
      if(!q) {
        q = window.location.search.substring(1);
      }
      var params = {},
          e,
          a = /\+/g,  // Regex for replacing addition symbol with a space
          r = /([^&=]+)=?([^&]*)/g,
          d = function (s) { return decodeURIComponent(s.replace(a, " ")); };

      while(e = r.exec(q)) {
        var k = d(e[1]);
        if(k==name) {
          return d(e[2]);
        }
      }
    }
  });
})(jQuery);
