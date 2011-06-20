--- 
title: Quick Guide for Passenger on Natty Narwhal
date: 2011-06-19
category: Ruby
tags: ruby, passenger, apache, rails, git, phusion, guides
---

This is a short guide for installing [Phusion Passenger][1] and [Ruby
Enterprise Editition][2] on Ubuntu Natty Narwhal. Depending on your machine
this will take 30-60 minutes on a fresh Ubuntu install.


### Installing build tools and libraries

First we need to install the compiler toolchain (make, gcc and libc)
and necessary libraries.

    $ apt-get install build-essential zlib1g-dev libssl-dev libreadline5-dev libmysqlclient-dev


### Ruby Enterprise Edition

We are going to download and compile Ruby Enterprise Edition. The
installer asks for the target directory.  I would recommend to install
into /opt/ruby unless you want to host different versions on this machine.

    $ wget http://rubyenterpriseedition.googlecode.com/files/ruby-enterprise-1.8.7-2011.03.tar.gz
    $ tar xzf ruby-enterprise-1.8.7-2011.03.tar.gz
    $ cd ruby-enterprise-1.8.7-2011.03
    $ ./installer

Now we include the path to the ruby binaries in /etc/environment. It should look like this:

    PATH="/opt/ruby/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games"

After relogin you should be able to type ruby -v and get a response like this:    

    ruby 1.8.7 (2011-02-18 patchlevel 334) [x86_64-linux], MBARI 0x6770, Ruby Enterprise Edition 2011.03


### Apache and Passenger

We need to install Apache and necessary development libraries to compile Phusion Passenger. 

    $ apt-get install libcurl4-openssl-dev apache2-mpm-prefork apache2-prefork-dev libapr1-dev libaprutil1-dev
    $ passenger-install-apache2-module


### Apache configuration

The compilation of the Passenger Apache
module finished with an instruction for your httpd.conf. Depending on
you passenger version, you will get something like this, which you add
to your `/etc/apache2/httpd.conf`:

    LoadModule passenger_module /opt/ruby/lib/ruby/gems/1.8/gems/passenger-3.0.7/ext/apache2/mod_passenger.so
    PassengerRoot /opt/ruby/lib/ruby/gems/1.8/gems/passenger-3.0.7
    PassengerRuby /opt/ruby/bin/ruby

If you browse to your url, you should see the standard apache "It works" page.


### MySQL

The Ruby Enterprise Installer already compiled Ruby's mysql client
library, now we need the server and client.  

    $ apt-get install mysql-server mysql-client


### Virtual host config

Adding a virtual host for your rails application is easy. Assuming
that your application resides in /var/www/myapp create a file named
`/etc/apache2/sites-available/myapp` and fill in :

    <VirtualHost *:80>
        ServerName myserver.com
        DocumentRoot /var/www/myapp/public
    </VirtualHost>

Now we disable the default site and add our new virtual host:

    $ a2dissite default
    $ a2ensite myapp

After restarting Apache your Rails application should run on Apache:

    $ /etc/init.d/apache2 restart


[1]: http://www.modrails.com/
[2]: http://www.rubyenterpriseedition.com/
