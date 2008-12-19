--- 
category: Ruby
date: 2007-04-08
tags: flickr, delicious, rails
languages: ruby, html
tags: flickr, delicious, rails

Building a del.ico.us and flickr sidebar in 5 minutes
=====================================================

You need a del.icio.us sidebar which shows recent bookmarks or one of
these nice flickr badges? This is really is easy as the [Typo Weblog
Engine][1] already includes an flickr and del.icio.us aggregator.

Download the files [delicous.rb][2] and [flickr.rb][3] and drop them
into your rails lib folder.

Now add these two little helpers to your application_helper.rb:

    @@ruby

    require 'delicious'
    require 'flickr'
     
    def delicious(tag)
      Delicious.new("http://del.icio.us/rss/tag/#{tag}")
    end
     
    def flickr(tag)
      url = "http://api.flickr.com/services/feeds/photos_public.gne?"
      url << "tags=#{tag}&format=rss_200"
      FlickrAggregation.new(url)
    end

We are now able to fetch the feeds for our desired tags with a simple
method call. Next step is to render a list of links for our
sidebar. So for rendering the del.icio.us sidebar, you need something
like this:

    @@rhtml

    <ul>
      <% for item in delicious(:ruby).items[0, 10] %>
        <li>
          <%= link_to item.title, item.link %>
        </li>
      <% end %>
    </ul>

This is pretty self-explanatory. We take the first ten items of the
del.icio.us feed and for each item we output a list element containing
a link to the item. The view for the flickr badge is similar:

    @@rhtml

    <ul>
      <% for item in flickr(:ruby).pics[0, 10] %>
       <%= image_tag item.square, :size => '48x48' %>
      <% end %>
    </ul>


We take the first ten items of the flickr feed and for each item we
render an image tag which shows a square thumbnail of the size
48x48. Congratulations, you have just written a del.icio.us and flickr
sidebar in 5 minutes using only 18 lines of code. Now you can spend
the rest of the day pimping up your sidebar with all kinds of feeds
using one of these plugins of the typo weblog.

[1]: http://trac.typosphere.org/
[2]: http://trac.typosphere.org/browser/trunk/vendor/plugins/delicious_sidebar/lib/delicious.rb?format=raw
[3]: http://trac.typosphere.org/browser/trunk/vendor/plugins/flickr_sidebar/lib/flickr.rb?format=raw