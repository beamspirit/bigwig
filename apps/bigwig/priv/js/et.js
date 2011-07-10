$(document).ready(function() {
  var oTable = $('#etop_table').dataTable({
    "bProcessing": false,
    "iDisplayLength": 25,
    "sAjaxSource": '/top',
    // Get data piggy-backed on the main table and update the page
    "fnServerData": function(sSource, aoData, fnCallback) {
                      $.getJSON( sSource, aoData, function(json) {
                        $('#accumulate').attr('checked', json.accumulate);
                        var header = json.header;
                        var gs = ['node','clock','cpu','tot','bin','nprocs','procs','code','runqueue','atom','ets'];
                        for (var i = 0; i < gs.length; i++) {
                          var g = gs[i];
                          if (g in header) {
                            $('#glob_' + g).html(header[g]);
                          }
                        }
                        fnCallback(json);
                      });
                    }
  });
  $(document).everyTime(5000, "poll_etop", function() {oTable.fnReloadAjax();});
  $('#accumulate').bind('click', function() {
    $.ajax({
      type: 'POST',
      url: '/top/config/accumulate/' + this.checked
    });
  });
  $('#pause').bind('click', function() {
    if (this.checked) {
      $(document).stopTime("poll_etop");
    } else {
      $(document).everyTime(5000, "poll_etop", function() {oTable.fnReloadAjax();});
    }
  });
});
