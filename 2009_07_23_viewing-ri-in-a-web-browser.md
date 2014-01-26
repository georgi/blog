Viewing RI in a web browser
===========================

I'm a big fan of the Firefox keyword search. For example I have
keywords for [LEO][1], [Wikipedia][2] and [Man pages][3]. Sometimes I
want to look up API documentation in Ruby and typing `ri camelize`
into the address bar and viewing the documentation as web page seems
to be quite natural for me. So I wrote a quick and dirty cgi, which
calls RI and outputs HTML.

### CGI script

I've put the following code in file named
`/usr/lib/cgi-bin/ri.b`. This is the default location for cgi scripts
on my system for Apache.

    #!/usr/bin/env ruby
    
    require 'rdoc/ri/ri_driver'
    require 'rubygems'
    
    print "Content-type: text/html\r\n\r\n"
    
    ARGV << '-f' << 'html'
    
    ri = RiDriver.new
    
    print '<html><body style="width:600px; margin:auto; padding:20px">'
    ri.process_args
    print '</body></html>'

This script does the same thing as if you typed `ri somequery -f
html`. I put some HTML around it to give it some style, but that's it.


### The Keyword Search

So I want to type `ri String.capitalize` and the browser should send a
request to `http://localhost/cgi-bin/ri.rb?String.capitalize`.

Just add a new bookmark and give it the keyword `ri` and use as url
this one:

     http://localhost/cgi-bin/ri.rb?%s

Now we're done. One thing I would like to improve is to add hyperlinks
to the output. For example viewing the documentation of a class brings
up all documented methods. Each of them should be a link to the actual
documentation. Probably some monkey patching on the [HtmlFormatter][4]
class would do the job.


[1]: http://dict.leo.org
[2]: http://en.wikipedia.org
[3]: http://www.nongnu.org/man2html
[4]: http://rdoc.rubyforge.org/classes/RDoc/RI/HtmlFormatter.html
