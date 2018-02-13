$( document ).ready(function() {
	if ($(window).width() >= 750) {
		$('#target').slideDown('slow');
	} else {
		$('#target').slideUp('slow');
	}
});

$( window ).resize(function() {
	if ($(window).width() >= 750) {
		$('#target').slideDown('slow');
	} else {
		$('#target').slideUp('slow');
	}
});

$('.toggle').click(function() {
	if ($(window).width() < 750) {
		$('#target').slideToggle('slow');
	}
});
