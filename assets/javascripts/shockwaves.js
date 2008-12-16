$(function() {
    $('.swfobject').each(function() {
	swfobject.embedSWF('/shockwaves/' + this.id + '.swf', this.id, "400", "320", "9.0.0");
    });
});


