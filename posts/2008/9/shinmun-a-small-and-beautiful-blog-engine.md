--- 
category: Ruby
guid: 7ad04f10-5dd6-012b-b53c-001a92975b89
title: Shinmun, a small and beautiful blog engine
tags: ruby, blog
date: 2008-09-05

Small is beautiful. This is especially true for software, as you can
tailor the code exactly to your needs. I tried several times to build
a blog engine, but was always dissatisfied by the
complexity. Deploying is a problem too. I don't want to run a fat
rails process for a small blog like this. The solution was a finally a
tiny script, which renders text files to static web pages. The biggest
advantage is, that I can edit my articles and templates in Emacs and
easily extend the software in Ruby.


#### Directory layout

The layout is as following:

* The blog description, base url and the category list is in a file
  called `blog.yml`

* Posts should be put into folders by date. `my-article.md` is in the
  folder 9 (September) of the year 2008. Posts are distinguished from
  pages by having a date attribute.

* Pages can be put anywhere, like `about.md`

* The output directory is called public. Static files should be put
  into the directories `images`, `stylesheets`, `javascripts`.

* Category pages will be rendered into the `categories` folder. A category
  page will display recent entries for one category.

* An index page will be generated for recent posts.

Following is an example folder tree:

    + posts  
      + blog.yml
      + about.md
      * 2007
        + 2008
          + 9
            + my-article.md
    + public
      + index.html
      + about.html
      + categories
        + emacs.html
        + ruby.html
      + 2007   
      + 2008
        + 9
          + my-article.html
      + images
      + stylesheest
      + javascripts
    

#### Layout and Templates

Layout and templates are rendered by good old ERB. This is the
probably the simplest solution, but ERB templates are still powerful,
they can execute arbitrary ruby code and call helper methods.

There are also some helper methods, which have the same interface like
rails helpers. The template class just needs a few methods to be useful:
    
    # Render stylesheet link tags with fixed url.
    def stylesheet_link_tag(*names)
      names.map { |name|
        tag :link, :href => "#{root}stylesheets/#{name}.css", :rel => 'stylesheet', :media => 'screen'
      }.join("\n")
    end

    # Render javascript tags with fixed url.
    def javascript_tag(*names)
      names.map { |name|
        tag :script, :src => "#{root}javascripts/#{name}.js", :type => 'text/javascript'
      }.join("\n")
    end

    # Render an image tag with fixed url.
    def image_tag(src, options = {})
      tag :img, options.merge(:src => root + 'images/' + src)
    end

    # Render a link with fixed url.
    def link_to(text, path, options = {})
      tag :a, text, options.merge(:href => root + path + '.html')
    end


#### Meta data

Each document has a header section and a body section, which are
separated by a newline. The header section is just a YAML document and
the body section consists of an markdown document. This is also
extremly simple, but just works. An example is probably the best way
to understand this:

<pre>
--- 
category: Ruby
guid: 7ad04f10-5dd6-012b-b53c-001a92975b89
title: BlueCloth, a Markdown library
tags: ruby, bluecloth, markdown
date: 2008-09-05

This is the summary, which is by definition the first paragraph of the
article. The summary shows up in category listings or the index listing.
</pre>  

The blog engine will assign a GUID to the post, the first time it will
be rendered. This GUID should never change, as it will be you used for
identifying posts for comments.


#### RSS Feeds

A blog without feeds is worthless. But a feed is nothing more than an
ERB template file. Some of the variables used here, have been read
from the meta file:

    <?xml version="1.0" encoding="utf-8"?>
    <rss version="2.0"> 
      <channel>
        <title><%= @category ? @blog_title + ' - ' + @category : @blog_title %></title>
        <link><%= @blog_url %></link>
        <description><%= @category ? 'Category ' + @category : @blog_description %></description>
        <language><%= @blog_language %></language>
        <copyright><%= @blog_author %></copyright>
        <pubDate><%= rfc822 Time.now %></pubDate>
        <% for post in @posts %>
          <item>
            <title><%= post.title %></title>
            <description><%= post.text_summary %></description>
            <link><%= post.link %></link>
            <author><%= @blog_author %></author>
            <guid><%= post.guid %></guid>
            <pubDate><%= rfc822 post.date %></pubDate>
          </item>
        <% end %>
      </channel> 
    </rss>


#### JSON store in PHP

As I am not willing to build up a whole Rails stack for a single blog,
I was looking for a simple storage for comments. I really like the
JSON format. It works seamlessly with ExtJS, jQuery and other
Javascript libraries and can be serialized and deserialized from
almost any language.

PHP is not the most elegant language (in fact not even close), but has
the tremendous advantage to be ubiquitous. Next time I will write
something about nifty comment forms in javascript (powered by jQuery)
and a simple JSON store.

**I posted a short description of my commenting system.** [Check it
  out][2].


#### Download

Download the package at my [github repository][1]

[1]: http://github.com/georgi/shinmun/tree/master
[2]: commenting-system-with-lightweight-json-store.html
