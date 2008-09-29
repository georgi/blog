
var EditorPanel = Ext.extend(Ext.Panel, {

    initComponent: function() {
	Ext.apply(this, {
	    layout: 'fit',
            closable: true,
            tbar: [{
		text: 'Save',
		scope : this,
		handler: this.save
            }]
	});

	this.addEvents({ save: true });

	EditorPanel.superclass.initComponent.call(this);
    },

    load: function(node) {
        this.setTitle(node.text);
        this.currentNode = node;

        this.progress = Ext.Msg.wait('Loading...');

        Ext.Ajax.request({
            url: '/admin_controller/' + this.currentNode.id,
	    params: { action: 'data' },
            method: 'GET',
            scope: this,
            success: this.onLoad
        });
    },

    onLoad: function(response) {
        this.progress.hide();
        this.setValue(response.responseText);
    },

    save: function() {
        this.progress = Ext.Msg.wait('Saving...');

        Ext.Ajax.request({
            url: this.currentNode.id,
	    params: { action: 'data' },
            method: 'PUT',
            params: this.getValue(),
            scope: this,
            success: this.onSave
        });
    },

    onSave: function() {
        this.progress.hide();
        this.fireEvent('save', this, this.currentNode);
    }

});
