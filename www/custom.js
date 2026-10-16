// Custom JavaScript for the EDA application

// Function to add tooltip to download buttons
// $(document).ready(function() {
    // Set tooltips for download buttons
   // $('.custom-download-btn').tooltip({
     // title: "Download transformed data with current parameters",
      // placement: "top",
      // container: 'body'
    // });
    
    // Detect when a download starts and add animation class
    // $(document).on('click', '.custom-download-btn', function() {
      // var buttonId = $(this).attr('id');
      // $(this).addClass('btn-downloading');
      
      // Remove the class after a timeout (simulating download completion)
      // setTimeout(function() {
        // $('#' + buttonId).removeClass('btn-downloading');
      // }, 3000);
    //});
  // });
  
  // Handler for custom messages from Shiny
 // Shiny.addCustomMessageHandler('toggleDownloadButtonClass', function(message) {
   // var button = $('#' + message.id);
    
  //  if (message.downloading) {
    //  button.addClass('btn-downloading');
    //} else {
    //  button.removeClass('btn-downloading');
    //}
  // });
  
  // Show additional info when hovering over univariate download button
  // $(document).on('mouseenter', '#download_univariate', function() {
    // var transformType = $('#transformation_univ').val();
    // var variableName = $('#variable_univ option:selected').text();
    
    // if (variableName && variableName !== "N/A") {
      // var infoText = 'Download "' + variableName + '" with transformation "' + transformType + '"';
      
      // $('<div class="download-info-badge">' + infoText + '</div>').insertBefore($(this));
    // }
  // }).on('mouseleave', '#download_univariate', function() {
    // $('.download-info-badge').remove();
  // });
  
  // Show info for multivariate download buttons
  // $(document).on('mouseenter', '#download_multivariate_sum, #download_multivariate_individual', function() {
    // var buttonId = $(this).attr('id');
    // var infoText = '';
    
    // if (buttonId === 'download_multivariate_sum') {
      // infoText = 'Download summed variables with transformation';
    // } else {
      // infoText = 'Download variables with individual transformations';
    // }
    
    // $('<div class="download-info-badge">' + infoText + '</div>').insertBefore($(this));
  // }).on('mouseleave', '#download_multivariate_sum, #download_multivariate_individual', function() {
    // $('.download-info-badge').remove();
  // });
// Custom JavaScript for the EDA application

$(document).ready(function() {
  // Removed tooltip setup for download buttons

  // Detect when a download starts and add animation class
  $(document).on('click', '.custom-download-btn', function() {
    var buttonId = $(this).attr('id');
    $(this).addClass('btn-downloading');
    
    // Remove the class after a timeout (simulating download completion)
    setTimeout(function() {
      $('#' + buttonId).removeClass('btn-downloading');
    }, 3000);
  });
});

// Handler for custom messages from Shiny
Shiny.addCustomMessageHandler('toggleDownloadButtonClass', function(message) {
  var button = $('#' + message.id);

  if (message.downloading) {
    button.addClass('btn-downloading');
  } else {
    button.removeClass('btn-downloading');
  }
});

// Removed all hover-based info badges for download buttons

// --- FIX: datepicker behind navbar/tabs (move popup to <body>) ---
(function () {
  function moveDatepickerToBody($input) {
    // Shiny/Bootstrap Datepicker creates .datepicker-dropdown
    var $dp = $('.datepicker.datepicker-dropdown');
    if (!$dp.length) return;

    // Detach and append to body to escape stacking contexts
    if (!$dp.parent().is('body')) {
      $dp.detach().appendTo('body');
    }

    // Force on top
    $dp.css({
      'z-index': 999999,
      'position': 'absolute'
    });

    // Reposition under the input
    var off = $input.offset();
    if (!off) return;

    var h = $input.outerHeight() || 0;
    $dp.css({
      top: (off.top + h + 6) + 'px',
      left: off.left + 'px'
    });
  }

  // Delegate focus/click for dynamically-created inputs too (report_dates appears only in CUSTOM)
  $(document).on('focus click', 'input[name="analysis_dates"], input[name="report_dates"]', function () {
    var $inp = $(this);
    // Wait a tick for the datepicker DOM to be created
    setTimeout(function () {
      moveDatepickerToBody($inp);
    }, 0);
  });

  // Also re-position when window scrolls/resizes while open
  $(window).on('scroll resize', function () {
    var $active = $('input[name="analysis_dates"]:focus, input[name="report_dates"]:focus');
    if ($active.length) {
      moveDatepickerToBody($active.first());
    }
  });
})();
