--- 
category: Ruby
date: 2007-04-10
languages: ruby
tags: rails, rest, url
Pretty RESTful URLs in Rails
============================

Since Release 1.2 Rails knows to generate _RESTful_ routes. Each
resource is identified by an _URI_, which looks like `/users/123` . It
would be nice to have more readable URLs, which include the name of
the user: `/users/matthias-georgi`. This is a short tutorial on making
your urls pretty while retaining the REST approach.

Assuming that you already have an user model, we add following line to
our `config/routes.rb`:

    @@ruby

    map.resources :users

You may now run the `scaffold_resource` generator in case you don't have
any `UsersController` yet. In order to remember the _permalink_ for each
user, we store it in a column named `permalink` in the users
table. Before we save an user record, we have to infer a url-safe
permalink for the user name. We do this by:

    @@ruby

    def before_save
      self.permalink = name.downcase.gsub(/[^a-z0-9]+/i, '-') if permalink.blank?
    end

Each character, which is not an _alphanumeric_ will be replaced by a
dash. This is only done, if the permalink is not set already. So we
have a way for users to set their permalink manually. To avoid
duplicate permalinks, we _validate_ the uniqeness of the permalink:

    @@ruby

    validates_uniqeness_of :permalink

What happens now, if we browse to a user url like
`/users/matthias-georgi` ? Rails raises an exception, telling us that it
cannot find an record with the id `matthias-georgi`. We'll fix now our
controller to look for the permalink and not the id of the user. Just
replace each call to `User.find(params[:id])` with
`User.find_by_permalink(params[:id])`. Also we have to ensure, that our
user routes will be generated correctly. Therefore we overwrite the
`to_param` method:

    @@ruby

    def to_param
      permalink
    end

The effect of this little change is, that `user_url(a_user)` generates
the right url. _Nested resources_ and _pretty urls_ can get tricky and for
now I won't touch this topic. Remember that _changing_ the permalink may
be problematic as links to the old url will get invalid. Nevertheless
have fun experimenting with pretty urls and the new wonderful world of
REST.