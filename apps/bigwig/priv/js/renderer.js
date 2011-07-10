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
        if( obj === true ) return 'true';
        if( obj === false ) return 'false';
        if( typeof obj == 'number' ) return obj;
        if( typeof obj == 'string' ) return obj;
        if( $.isArray(obj) ) return render_list(obj, '[', ']');
        if( typeof obj == 'object' ) 
        {
            switch (obj._type)
            {
                case 'tuple':
                    if(obj.data.length == 3 && (typeof obj.data[2] == 'number'))
                    {
                        return $('<a class="mfa" href="#">' + obj.data[0] + ':' + obj.data[1] + '/' + obj.data[2] + '</a>').click(function(e){ show_mfa_dialog(obj.data[0],obj.data[1],obj.data[2]); e.preventDefault()});
                    } 
                    return render_list(obj.data, '{', '}');
                case 'pid':
                    var Pid = obj.data.replace(/[<>]/g, '');
                    return $('<a href="#">&lt;'+Pid+'&gt;</a>')
                        .click(function(e){ show_pid_dialog(Pid); e.preventDefault();});
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
