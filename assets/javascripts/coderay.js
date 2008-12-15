
$('pre.highlight').each(function() {
    var pre = $(this);
    var text = $('li', pre).map(function() { return $(this).text(); }).get();
    var textarea = $('<textarea cols="80" rows="' + text.length + ' ">' + text.join("\n") + '</textarea>');
    var div = $('<div style="text-align:right"><a href="#">Toggle Code</a></div>').insertAfter(pre);
    var toggle = false;
    $('a', div).click(function() {
	toggle ? textarea.replaceWith(pre) : pre.replaceWith(textarea);
	toggle = !toggle;
	return false;
    });
});
