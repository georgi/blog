(function() {

    function convertToTextarea(pre) {
	var text = '';
	var rows = 0;

	$('li', pre).each(function() {
	    text += $(this).text() + "\n";
	    rows += 1;
	});

	$(pre).replaceWith('<textarea class="code" cols="80" rows="' + rows + ' ">' + text + '</textarea>');

	setTimeout(function() {
	    $('textarea.code').blur(function() {
		$(this).replaceWith(pre);
		$(pre).click(function() {
		    convertToTextarea(this);
		});
	    });
	}, 100);
    }


    $('pre.highlight').click(function() {
	convertToTextarea(this);
    });

})();
