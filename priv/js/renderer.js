var RENDERER = (function() {
    var show_pid_dialog = function(Pid) {
        var url = '/pid/'+ Pid;
        $.ajax({
                    url: url,
                    dataType: 'json',
                    success: function(json){
                        $('<div class="pid_dialog"></div>')
                            .append( gen_pid_html(json) )
                            .dialog({
                                title: 'process_info(' + Pid + ') &nbsp; ' + 
                                       '<small>(<a href="'+url+'">json</a>)</small>',
                                width: 550,
                                buttons: { 
                                    "Close": function() { $(this).dialog("close"); }, 
                                    "Send Msg":  function() { 
                                        var msg = prompt("Enter Erlang term to send to <" + Pid + ">");
                                        if(msg)
                                        {
                                            var thisdiag = $(this);
                                            $.ajax({
                                                url:url,
                                                type:'POST',
                                                data: {msg: msg},
                                                success: function(resp){
                                                    alert('Sent ok');
                                                }
                                            })                                                
                                        }
                                    }, 
                                    "Kill" :  function() { 
                                        if(confirm("Sure you want to kill <" + Pid + ">"))
                                        {
                                            var thisdiag = $(this);
                                            $.ajax({
                                                url:url,
                                                type:'DELETE',
                                                success: function(resp){
                                                    thisdiag.dialog("close"); 
                                                }
                                            })                                                
                                        }
                                    } 
                                }
                        });
                    }
                });
    }

    var show_mfa_dialog = function(m, f, a) {
        var url = '/module/' + m;
        $.ajax({
                    url: url,
                    dataType: 'json',
                    success: function(json){
                        $('<div class="mfa_dialog"></div>')
                            .append( gen_pid_html(json) )
                            .dialog({
                                title: m + ':module_info() &nbsp; ' + 
                                       '<small>(<a href="'+url+'">json</a>)</small>',
                                width: 550,
                                buttons: { 
                                    "Close": function() { $(this).dialog("close"); }, 
                                    "Reload Module":  function() { 
                                        if(confirm("Sure you want to reload this module?"))
                                        {
                                            var thisdiag = $(this);
                                            $.ajax({
                                                url:url,
                                                type:'POST',
                                                data: {reload: "yes"},
                                                success: function(resp){
                                                    alert('Module reloaded');
                                                }
                                            })                                                
                                        }
                                    }
                                }
                            })
                    }});
    }


    var gen_pid_html = function( json ) {
        var t = $('<table></table>');
        for(var key in json)
        {
        t = t.append(   $('<tr></tr>')
                        .append('<td>'+key+'</td>')
                        .append('<td></td>')
                        .append(render_json_val(json[key]))
                    );
        }
        return t;
        
    }

    var render_list = function( list) {
        var d = $('<span>[</span>');
        if(list.length == 0)
        {
            return d.append(' ]');
        }
        else
        {
            var first = true;
            for(var i in list)
            {
                if(!first) {
                    d.append(', ');
                }
                d.append( render_json_val(list[i]) );
                first = false;
            }
            d.append(']');
            return d;
        }
    }

    var pad = function pad(number, length) {
        var str = '' + number;
        while (str.length < length) str = '0' + str;
        return str;
    }

    var render_object = function( obj ) {
        switch (obj._type)
        {
            // Node we use delegate to bind onclick for all a.pid, a.mfa
            case 'tuple':
                if(obj.data.length == 3 && (typeof obj.data[2] == 'number'))
                {
                    return $('<a class="mfa" href="#">' + obj.data[0] + ':' + obj.data[1] + '/' + obj.data[2] + '</a>'); 
                } 
                return render_list(obj.data, '{', '}');
            case 'pid':
                var Pid = obj.data.replace(/[<>]/g, '');
                return $('<a class="_pid" href="#">&lt;'+Pid+'&gt;</a>');
            case 'port':
                return $(obj.data)
            case 'date':
                var o = obj.data;
                var str = o[0] + '-' + pad(o[1],2) + '-' + pad(o[2],2) +
                          ' ' + pad(o[3],2) + ':' + pad(o[4],2) + ':' + pad(o[5],2);
                return $('<span>' + str + '</span>');
            case 'bag':
                var d = $('<span>{</span>');
                var first = true;
                for(var k=0;k<obj.data.length;++k)
                {
                    if(!first) {
                        d.append(', ');
                    }
                    d.append( render_json_val(obj.data[k]) );
                    first = false;
                }
                d.append('}');
                return d;
                    
            default:
                var d = $('<span>{</span>');
                if(obj.length == 0)
                {
                    return d.append(' }');
                }
                else
                {
                    var first = true;
                    for(var i in obj)
                    {
                        if(!first) {
                            d.append(', ');
                        }
                        d.append( render_json_val(i) );
                        d.append(' : ');
                        d.append( render_json_val(obj[i]) );
                        first = false;
                    }
                    d.append('}');
                    return d;
                }
        }
    }

    var render_json_val = function( obj ) {
        if( obj === true ) return $('<span>true</span>');
        if( obj === false ) return $('<span>false</span>');
        if( typeof obj == 'number' ) return $('<span>' + obj + '</span>');
        if( typeof obj == 'string' ) return $('<span>' + obj + '</span>');
        if( $.isArray(obj) ) return render_list(obj);
        if( typeof obj == 'object' ) return render_object(obj);
        console.log("ERRRR what type is this");
    }
    var stringify = function(json) {
        return (JSON.stringify(json, null, 4)).replace(/\<(\d+)\.(\d+)\.(\d+)\>/, '<a class="_pid" href="#">&lt;$1.$2.$3&gt;</a>');
    }


    return {
        show_pid_dialog: show_pid_dialog,
        show_mfa_dialog: show_mfa_dialog,
        gen_pid_html: gen_pid_html,
        render_list: render_list,
        render_object: render_object,
        render_json_val: render_json_val,
        json: render_json_val,
        stringify: stringify
    }

})();

$("body").delegate("a._pid", "click", function(e){
    var pid = $(this).text().replace(/[<>]/g,'');
    RENDERER.show_pid_dialog(pid);
    e.preventDefault();
});
$("body").delegate("a.mfa", "click", function(e){
    var x1 = $(this).text().split(':');
    var x2 = x1[1].split('/');
    RENDERER.show_mfa_dialog(x1[0],x2[0],parseInt(x2[1]));
    e.preventDefault();
});

