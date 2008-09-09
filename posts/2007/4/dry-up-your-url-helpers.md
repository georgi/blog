--- 
guid: 9dba2130-5d6c-012b-b53a-001a92975b89
category: Ruby
tags: [rails, url, helpers]
languages: [ruby]
date: 2007-04-18

DRY Up Your Url Helpers
=======================

This tutorial shows you how to simplify url generation in combination
with _RESTful_ resources by extending the `url_for` helper. This approach
will also work with nested routes and other helpers like `form_tag` and
`link_to`.

One of the concepts of _REST_ is: each resource has its own unique
_URI_. We will enhance the `url_for` helper to generate this unique URI
for an arbitrary record.

### Example Models

First we need a simple example. We have 2 models: `User` and
`Article`. For our url generation to work we have to add following code
to our models:

    class User
      has_many :articles
     
      def to_params
        {:id => permalink}
      end
    end
     
    class Article
      belongs_to :user
     
      def to_params
        {:user_id => user.permalink, :id => permalink}
      end
    end


This is necessary for nested routes to play nicely with our url
generation code. We are now able to find the parameters for each
record to generate an unique URL.


### Pretty URLs

In a previous post I demonstrated the use of meaningful urls. We are
going now the same way.

Users are identified by an URL like:

    /users/matthias-georgi

Each user may write articles, which are located at:

    /users/matthias-georgi/articles

If I want to write a new article, I will use this URL:

    /users/matthias-georgi/articles/new

Editing an existing article would end up on this URL:

    /users/matthias-georgi/articles/my-first-post;edit


For nested resources to get working you define in config/routes.rb:

    map.resources :users do |user|
      user.resources :articles
    end

The traditional way to generate urls is to call the resource helpers:

    article_url(article.user, article)


This is redundant, as the article already knows its user.

### The Resource Helper

Add following module into your lib folder and include the module in
both your application controller and application helper. The most
important bit is the url_for method. It will automatically generate
the right url for your resource.

    module ResourceHelper
     
      def plural_class_name(record)
        singular_class_name(record).pluralize
      end
     
      def singular_class_name(record)
        record.class.name.underscore.tr('/', '_')
      end
     
      def params_for(record)
        if record.respond_to?(:to_params)
          record.to_params
        else
          {:id => record.to_param}
        end
      end
     
      def collection_url(collection, record, options)
        if record
          params = params_for(record)
          params["#{singular_class_name(record)}_id".to_sym] = params.delete(:id)
          url_for options.merge(params).merge(:controller => collection)
        else
          url_for options.merge(:controller => collection)
        end
      end
     
      def member_url(record, options)
        url_for options.merge(params_for(record)).merge(:controller => plural_class_name(record))
      end
     
      def url_for(*args)
        if [String, Hash].any? {|type| args.first.is_a? type }
          super(*args)
        else
          if args[0].is_a?(Symbol)
            collection_url(args[0], args[1], :action => 'index')
          else
            member_url(args.first, :action => 'show')
          end
        end
      end
     
      def new_url_for(collection, record=nil)
        collection_url(collection, record, :action => 'new')
      end
     
      def edit_url_for(record)
        member_url(record, :action => 'edit')
      end
     
      def path_for(*args)
        if args[0].is_a?(Symbol)
          collection_url(args[0], args[1], :action => 'index', :only_path => true)
        else
          member_url(args.first, :action => 'show', :only_path => true)
        end
      end
     
      def new_path_for(collection, record=nil)
        collection_url(collection, record, :action => 'new', :only_path => true)
      end
     
      def edit_path_for(record)
        member_url(record, :action => 'edit', :only_path => true)
      end
     
    end


### Usage

So how can you use this stuff actually?

It is pretty easy: just pass the record instead of the url hash and
the unique url will be generated automatically.

The url of a collection is treated differently. You have to pass the
name of the collection, which is the controller name. For nested
resources you have to pass additionally the record, the collection is
belonging to.

Some examples:

    new_path_for(:users)          # => '/users/new'
    path_for(user)                # => '/users/harald'
    edit_path_for(user)           # => '/users/harald;edit'
    path_for(:articles, user)     # => '/users/harald/articles'
    new_path_for(:articles, user) # => '/users/harald/articles/new'
    path_for(article)             # => '/users/harald/articles/article-1'
    edit_path_for(article)        # => '/users/harald/articles/article-1;edit'
     
    # This works for helpers like url_for, form_tag or link_to. 
     
    link_to article.title, article
    form_tag :articles
    form_tag article, :method => 'put'

If anybody is interested I will release this stuff as plugin. I think
other helpers could benefit as well as you can pass your records
around and each helper may generate the appropriate url.
