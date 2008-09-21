$(function() {
    var head = document.getElementsByTagName("head")[0];
    var script = document.createElement('script');

    script.setAttribute("src", "http://feeds.delicious.com/v2/json/matthias_georgi?callback=renderBookmarks");
    script.setAttribute("type", "text/javascript");

    head.appendChild(script);
});

function renderBookmarks(data) {
     var template = new Template('bookmarks-template');
    $('.bookmarks').expand(template, { bookmark: data });
}