var CodeEditor = Ext.extend(EditorPanel, {

    initComponent: function() {
	CodeEditor.superclass.initComponent.call(this);

	this.editor = new Ext.ux.CodePress({
            language: this.language
	});

	this.add(this.editor);
    },

    getValue: function() {
        return this.editor.getValue();
    },

    setValue: function(value) {
	this.editor.onResize();
	
        (function() {
            this.editor.setValue(value);
        }).defer(100, this);
    }

});
