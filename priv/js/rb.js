
RB = (function() {
  function getFilter(level) {
    return level.split("_")[0];
  }

  function fmt_process_start(data) {
    var name = data.started.name;
    if(name._type == 'tuple') {
      name = name.data[0];
    }
    return {
      'started_pid': data.started.pid,
      'started_name': name
    };
  }
  function fmt_application_start(data) {
    return data;
  }
  var dataSources = ['data','generic_data','crashing_process'];
  $('#report ._tpl').data("process", function(el, report) {
    el.addClass(getFilter(report['report_level']));
    var data = null;
    for(var i in dataSources) {
      var k = dataSources[i];
      if(report[k]) {
        data = report[k];
        break;
      }
    }
    $('.report_level', el).click(function() {
      el.toggleClass('selected');
      return false;
    });

    if(data && data.started) {
      el.addClass('started');
      TPL.update($('.started',el), fmt_process_start(data));
    } else if(data && data.application) {
      el.addClass('application_start');
      TPL.update($('.application_start',el), fmt_application_start(data));
    } else {
      //el.addClass('selected');
    }
    if(!data) {
      data = report;
    }
    $('.data', el).html(RENDERER.render_json_val(data));
  });

  $('.filter').click(function() {
    var href = $(this).attr('href');
    var href_parts = href.split("?");
    var filter = "all";
    if(href_parts.length > 1) {
      filter = $.getQueryString('level', href_parts[1]);
    }
    var current = $('#report').attr('data-filter');
    if(filter != current) {
      $('.filter').removeClass('current');
      $(this).addClass('current');
      $('#report').removeClass(current);
      $('#report').addClass(filter);
      $('#report').attr('data-filter', filter);
    }
    return false;
  });

  TPL.connect("/rb/stream");
  TPL.fetch("/rb/reports?limit=200");

  return {

  };
})();

