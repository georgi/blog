--- 
category: Javascript
date: 2008-09-21
tags: template, delicious
title: Using Javascript Templates for a Delicious Sidebar
---








Processing JSON data from an external source with Javascript templates
is a natural fit. Create a template inside your *HTML Document* by
adding class names and variables and write a few lines for fetching
the *JSON*, that's all. This tutorial is an example for my Javascript
Template Engine called **[Patroon][1]**.


### Writing the template

In my sidebar you can see the result of my example. My latest
bookmarks are shown as a list. Quite simple. The template looks like
this:

    @@html

    <div class="bookmarks">
      <ul id="bookmarks-template">
        <li class="bookmark">
          <a href="{u}">{d}</a>
        </li>
      </ul>
    </div>

There a two variables here `u` and `d`. I don't know if *Delicious*
wants to save some bytes here, but descriptive names wouldn't hurt in
this case. `u` is refering to the url of the bookmark and `d` is the
title. We are expanding an array of bookmarks into the `li` element,
which is marked by the class name `bookmark`.


### Fetching the JSON Feed

The Feed resides on a different domain, so we have to use a `script`
tag to fetch the data. This is because of security restrictions, which
limits *AJAX* calls to the same domain of the current web page.

The feed url for your bookmarks looks like this:

    http://feeds.delicious.com/v2/json/{username}

If you want to fetch some of the other feeds, just look at the
[documentation][2], which describes 18 different feed types.

A very useful option in our case is to provide a callback function,
which gets called after the *JSON* script was loaded. We define
`renderBookmarks` as our callback.

The following code inserts the script tag to load the *Delicious*
*JSON* feed of my bookmarks. This is done when the page is loaded:

    @@javascript
 
    $(function() {
        var head = document.getElementsByTagName("head")[0];
        var script = document.createElement('script');
     
        script.setAttribute("src", "http://feeds.delicious.com/v2/json/matthias_georgi?callback=renderBookmarks");
        script.setAttribute("type", "text/javascript");
     
        head.appendChild(script);
    });

I'm using jQuery here for the window load event. Other libraries would
need some other api call.


### Rendering the JSON data

The code for rendering consists of just two lines. First we are
instantiating the Template. We have to provide the id of the template
node (the template is part of your document).

Second we expand the template using the jQuery helper. The variable
`data` contains just the array of bookmarks. To match the `li` element
of the template, which has the class name `bookmark`, we must set the
template variable `bookmark` to hold the bookmarks array.

    @@javascript

    function renderBookmarks(data) {
      var template = new Template('bookmarks-template');
      $('.bookmarks').expand(template, { bookmark: data });
    }


### Result

The resulting *HTML* of my bookmark sidebar looks like this:

    @@html

    <div class="bookmarks">
      <ul id="bookmarks-template">            
        <li class="bookmark">
          <a href="http://delicious.com/help/json/">
            <span>delicious/help/feeds</span>
          </a>
        </li>
        <li class="bookmark">
          <a href="http://code.google.com/apis/youtube/reference.html">
            <span>Reference Guide: Data API Protocol - YouTube APIs and Tools - Google Code</span>
          </a>
        </li>
        <li class="bookmark">
          <a href="http://rewrite.rubyforge.org/">
          <span>rewrite</span>
          </a>
        </li>
        <li class="bookmark">
          <a href="http://www.infoq.com/interviews/Rewrite-Reginald-Braithwaite">
            <span>InfoQ: Reginald Braithwaite on Rewrite</span>
          </a>
        </li>
      </ul>
    </div>

You may wonder, why there are extra span elements around the variable
expansions. Well this is necessary for inserting *HTML* from a
variable. If I want to replace a text node with some *HTML*, I have to
insert a *span* element and use the `innerHTML` property. If you know
something better, please let me know.


### Conclusion

Using *Javascript* templates with *JSON* feeds is simple and
efficient. You write standards-compliant *HTML* sprinkled with some
variables and expand this with some *JSON* data, that's all.


### Related Work

There are some other libraries for javascript templating, which are
related to **Patroon**:

* [PURE][3]
* [jsRepeater][4]
* [TrimPath][5]
* [EmbeddedJS][6]

Patroon is probably the smallest templating solution around and
consists only of 130 lines of code.


[1]: http://www.matthias-georgi.de/2008/9/patroon-a-javascript-template-engine-part-2.html
[2]: http://delicious.com/help/json/
[3]: http://beebole.com/pure/
[4]: http://jsrepeater.devprog.com/
[5]: http://code.google.com/p/trimpath/wiki/JavaScriptTemplates
[6]: http://embeddedjs.com/
