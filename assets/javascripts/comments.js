$('.comment-form form').ajaxForm({
    url: '/comments',
    type: 'POST',
    resetForm: true,
    beforeSubmit: function(values) {
	if (values[1].value && values[3].value) {
            $('.comment-form-loading').show();
	    return true;
	}
	else {
	    alert('Please enter name and text!');
	    return false;
	}
    },
    success: function(data) {
        $('.comment-form-loading').hide();
	$('.comments').html(data);
    }
});
