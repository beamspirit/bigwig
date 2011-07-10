var RENDERER = (function() {
    var show_pid_dialog = function(Pid) {
        var url = '/pid/' + Pid;
        $.ajax({
                    url: url,
                    dataType: 'json',
                    success: function(json){
                        $('<div class="pid_dialog"></div>')
                            .append( gen_pid_html(json) )
                            .dialog({
                                title: 'process_info(' + Pid + ') &nbsp; ' + 
                                       '<small>(<a href="'+url+'">json</a>)</small>',
                                width: 460,
                                buttons: { "Close": function() { $(this).dialog("close"); } }
                        });
                    }
                });
    }

    var show_mfa_dialog = function(m, f, a) {
        alert('MFA DIALOG HERE: ' + m + ':' + f + '/' + a);
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

    var render_list = function( list, stag, etag ) {
        var d = $('<span>'+stag+'</span>');
        if(list.length == 0)
        {
            return d.append(' ' + etag);
        }
        else
        {
            d.append( render_json_val(list[0]) );
            for(var i = 1 ; i < list.length; ++i)
            {
                d.append(', ');
                d.append( render_json_val(list[i]) );
            }
            d.append(etag);
            return d;
        }
    }

    var render_json_val = function( obj ) {
        if( obj === true ) return $('<span>true</span>');
        if( obj === false ) return $('<span>false</span>');
        if( typeof obj == 'number' ) return $('<span>' + obj + '</span>');
        if( typeof obj == 'string' ) return $('<span>' + obj + '</span>');
        if( $.isArray(obj) ) return render_list(obj, '[', ']');
        if( typeof obj == 'object' ) 
        {
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
                    return $('<a class="pid" href="#">&lt;'+Pid+'&gt;</a>');
                case 'port':
                    return $(obj.data)
                default:
                    return render_list(obj, '{', '}');
            }
        }
    }


    return {
        show_pid_dialog: show_pid_dialog,
        show_mfa_dialog: show_mfa_dialog,
        gen_pid_html: gen_pid_html,
        render_list: render_list,
        render_json_val: render_json_val,
    }

})();

$("body").delegate("a.pid", "click", function(e){
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

