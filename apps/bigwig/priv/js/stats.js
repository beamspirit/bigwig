var MAX_POINTS = 30;
var stats_data = {};
<!-- connect to realtime stats feed -->
$('#stats').bind('onupdate', function(e, data) {
    // generate sparklines for most of these stats:
    for(var key in data)
    {
        if( key == 'date' )   continue;
        if( key == 'uptime' ) continue;
        if(! stats_data[key]) stats_data[key]=[];

        stats_data[key].push(data[key]);
        stats_data[key] = stats_data[key].slice( - (MAX_POINTS+1) );
        // generate array of deltas to make sparkline:
        var sparkdata = [];
        var prevdata = stats_data[key][0];
        for(var i = 1; i<stats_data[key].length; ++i)
        {
            thisdata = stats_data[key][i];
            sparkdata.push( thisdata - prevdata );
            prevdata = thisdata;
        }
        $('.' + key).sparkline( sparkdata );
    }
    var formattedUptime = (new Date).clearTime()
                          .addSeconds(parseInt(data['uptime']/1000))
                          .toString('H:mm:ss');
    var numDays = Math.floor( data['uptime']/86400000 );
    if(numDays) formattedUptime = numDays + ':' + formattedUptime;                
    $('.uptime').text( formattedUptime );
});
TPL.connect("/stats-stream");
