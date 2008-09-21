/*
 * Patroon - Javascript Template Engine
 *
 * Copyright (c) 2008 Matthias Georgi (matthias-georgi.de)
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

function Template(id) {
    this.element = document.getElementById(id);

    if (this.element) {
        this.element.parentNode.removeChild(this.element);
    } else {
        throw "template not found: " + id;
    }
};


Template.Helper = {

    linkTo: function(text, url) {
	if (url.indexOf('http://') == -1 && url[0] != '/' && url[0] != '#') {
	    url = 'http://' + url;
	}
	return '<a href="' + url +'">' + text + '</a>';
    }

};

Template.prototype = {

    expand: function(data) {
        var node = this.element.cloneNode(true);
        this.expandData(data, node);
        return node;
    },

    expandData: function(data, node) {
        if (data.constructor == Array) {
            this.expandArray(data, node);
        }
	else {
            this.expandObject(data, node);
        }
    },

    expandArray: function(data, node) {
        var parent = node.parentNode;
        parent.removeChild(node);
        for (var i = 0; i < data.length; i++) {
            var child = node.cloneNode(true);
            parent.appendChild(child);
            this.expandData(data[i], child);
        }
    },

    cache: {},

    compile: function(str) {
        var len = str.length;
        var expr = false;
	var cur = '';
        var out = [];
	var braces = 0;

        for (var i = 0; i < len; i++) {
            var c = str[i];

	    if (expr) {
		if (c == '{') {
		    braces += 1;
		}
		if (c == '}') {
		    braces -= 1;
		    if (braces == 0) {
			expr = false;
			if (cur.length > 0) {
			    out.push("(" + cur + ")");
			}
			cur = "";
		    }
		}
		else {
		    cur += c;
		}
	    }
	    else {
		switch (c) {
		case "'":
                    cur += "\\'";
		    break;
		case "\\":
                    cur += "\\\\";
		    break;
		case '{':
		    expr = true;
		    braces += 1;
                    if (cur.length > 0) {
			out.push("'" + cur + "'");
                    }
                    cur = "";
                    break;
		case "\n":
		    break;
		default:
                    cur += c;
                }
            }
        }

        if (cur.length > 0) {
            out.push("'" + cur + "'");
        }

        var code = '(function (data) { with(Template.Helper) with (data) return ' + out.join('+') + '; } )';
	console.log(code);
        return eval(code);
    },

    evaluate: function(str, data) {
        var fn = this.cache[str];
        if (!fn) {
            fn = this.cache[str] = this.compile(str);
        }
        return fn(data);
    },

    expandByContext: function(object, node) {
	var names = node.className.split(' ');
	var found = false;

	for (var i = 0; i < names.length; i++) {
	    var name = names[i];
	    if (object[name]) {
		this.expandData(object[name], node);
		found = true;
	    }
	}

	if (!found) {
	    this.expandObject(object, node);
	}
    },

    expandObject: function(object, node) {
        var i;
        var attr = node.attributes;
        var nodes = [];

	for (i = 0; i < node.childNodes.length; i++) {
	    nodes.push(node.childNodes[i]);
	}

        for (i = 0; i < attr.length; i++) {
            var value = attr[i].value;
            if (value.indexOf('{') != -1) {
                attr[i].value = this.evaluate(value, object);
            }
        }

        for (i = 0; i < nodes.length; i++) {
            var child = nodes[i];
            if (child.nodeType == 1) {
		this.expandByContext(object, child);
            }
            if (child.nodeType == 3 && child.nodeValue.indexOf('{') != -1)  {
		var span = document.createElement('span');
                span.innerHTML = this.evaluate(child.nodeValue, object);
		child.parentNode.replaceChild(span, child);
            }
        }
    }
};

if ( typeof jQuery != "undefined" ) {
    jQuery.fn.expand = function(template, data) {
        return this.html(template.expand(data));
    };
}
