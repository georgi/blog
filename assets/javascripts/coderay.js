(function() {

    function convertToTextarea(pre) {
	var text = '';
	var rows = 0;

	$('li', this).each(function() {
	    text += $(this).text() + "\n";
	    rows += 1;
	});

	$(this).replaceWith('<textarea class="highlight" cols="80" rows="' + rows + ' ">' + text + '</textarea>');

	$('textarea.highlight').blur(function() {
	    $(this).replaceWith(pre);
	    $(pre).click(function() {
		convertToTextarea(this);
	    });
	});
    }


    $('pre.highlight').click(function() {
	convertToTextarea(this);
    });

})();
