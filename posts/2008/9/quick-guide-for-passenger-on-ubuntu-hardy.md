--- 
category: Ruby
date: 2008-09-13
tags: ruby, passenger, apache, rails, git, phusion, guides
title: Quick Guide for Passenger on Ubuntu Hardy
---






This is a short guide for installing [Phusion Passenger][1] on Ubuntu
Hardy. This includes the installation of Ruby 1.8.6, Apache 2.2.8, 
MySQL 5.0.51a, Git 1.5.4 and Rails 2.1.1.


### Essential Build Tools

First we need to install the compiler toolchain (make, gcc and libc).

    $ apt-get install build-essential


### Git

This guide is based on Git, so we install the git package:
  
    $ apt-get install git-core

If you want to host a git repository on this machine, initialize a new
repository:

    $ mkdir /var/git
    $ mkdir /var/git/myapp
    $ cd /var/git/myapp
    $ git --bare init

Now you can push your application code from your local machine to your
repository:

    $ cd ~/myapp
    $ git remote add origin ssh://myserver.com/var/git/myapp
    $ git push origin master


### Ruby

We are going to install Ruby and all the essential ruby libraries.

    $ apt-get install ruby ruby1.8-dev rubygems irb ri rdoc rake libruby libruby-extras


### Gem Executable Path

Strangely the *rubygems* package does not setup the path for
executables, so we add the following line to `/etc/profile`.

    export PATH=/var/lib/gems/1.8/bin:$PATH

To immediately use the new executable path, we source the profile file:

    $ . /etc/profile


### Apache

This is just a basic Apache install. We need the devlopment files for compiling passenger:

    $ apt-get install apache2 apache2-prefork-dev


### MySQL

I use MySQL, so I needed to install the server and client packages and
the Ruby gem, which compiles a native extension:

    $ apt-get install mysql-server mysql-client
    $ gem install mysql


### Phusion Passenger

This is now the actual Passenger install, which consists of installing
a gem and compiling the Apache module:

    $ gem install passenger
    $ passenger-install-apache2-module


### Apache configuration

The compilation of the Passenger Apache
module finished with an instruction for your httpd.conf. Depending on
you passenger version, you will get something like this, which you add
to your `/etc/apache2/httpd.conf`:

    LoadModule passenger_module /var/lib/gems/1.8/gems/passenger-2.0.3/ext/apache2/mod_passenger.so
    PassengerRoot /var/lib/gems/1.8/gems/passenger-2.0.3
    PassengerRuby /usr/bin/ruby1.8

Additionally you probably want to enable *mod_rewrite*, which is
needed for Rails:

    $ a2enmod rewrite


### Installing your Rails app

We create a app folder in `/var/www` and checkout the source from our
git repository:

    $ cd /var/www
    $ mkdir myapp
    $ cd myapp
    $ git init
    $ git remote add origin /var/git/myapp
    $ git pull origin master


#### Installing Rails

We don't install Rails as Gem, because your application should be
pinned to a specific Rails version. Git submodules allow you to embed
a foreign repository in your source tree. 

We are now going to link the Rails repository to `vendor/rails` and
checking out Version 2.1.1, finally we commit the submodule link to
our repository:

    $ cd /var/www/myapp/
    $ git submodule add git://github.com/rails/rails.git vendor/rails
    $ cd vendor/rails
    $ git checkout v2.1.1
    $ cd ../..
    $ git commit -m 'linked rails as submodule'

Probably you need to setup your database:

    $ mysaladmin create myapp_production
    $ mysaladmin create myapp_development
    $ mysaladmin create myapp_test
    $ rake db:migrate

Now your Rails app should be able to run as a Webrick Server:

    $ ./script/server
   

### Virtual host

Adding a virtual host for your rails application is now super easy
thanks to Passenger. Create a file named
`/etc/apache2/sites-available/myapp`:

    <VirtualHost *:80>
        ServerName myserver.com
        DocumentRoot /var/www/myapp/public
    </VirtualHost>

Now we disable the default site and add our new virtual host:

    $ a2dissite default
    $ a2ensite myapp

After restarting Apache your Rails application should run on Apache:

    $ /etc/init.d/apache2 restart


### User authentication

In case your Rails app is not meant to be seen on public, I recommend
protecting it with HTTP Authentication.

Create a password file:

    htpasswd2 /var/www/myapp/config/auth myusername


And add this to your virtual host configuration (Inside the
VirtualHost section):

    <Location />
        AuthType Basic
        AuthName "My App"
        AuthUserFile /var/www/myapp/config/auth
        Require valid-user
    </Location>


### Conclusion

Phusion Passenger simplifies the Installation of Rails applications
significantly. I don't have to worry about `mod_proxy`,
`mod_proxy_balancer`, `mongrel` and `mongrel_cluster` or even
FastCGI. This is **definitely simpler**.

I have to mention, that Rails is just one option for your Ruby
application. Setting up any other Ruby framework should be possible
through the support of the [Rack][3] interface. 

I really hope, that the specification of using one *rackup file* and
one *public folder* will settle down as a standard for Ruby web
applications, so that hosting companies will focus on supporting this
standard and ruby developers don't need to worry about finding support
for their favorite web frameworks.



[1]: http://www.modrails.com/
[2]: http://www.github.com/
[3]: http://rack.rubyforge.org/
