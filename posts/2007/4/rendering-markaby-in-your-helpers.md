--- 
category: Ruby
date: 2007-04-02
tags: markaby
title: Rendering markaby in your helpers
---




Generating markup in your rails helpers is a general practice in rails
and is used throughout all rails helpers. Normally you use `content_tag`
to generate markup. But often you will encounter situations, where
nested tags force you to write ugly helper code like the following
helper method from the rails library:

    @@ruby

    def options_for_select(container, selected = nil)
     container = container.to_a if Hash === container
     
     options_for_select = container.inject([]) do |options, element|
        if !element.is_a?(String) and element.respond_to?(:first) and element.respond_to?(:last)
         is_selected = ( (selected.respond_to?(:include?) && !selected.is_a?(String) ? selected.include?(element.last) : element.last == selected) )
          if is_selected
           options << "<option value=\"#{html_escape(element.last.to_s)}\" selected=\"selected\">#{html_escape(element.first.to_s)}</option>"
         else
           options << "<option value=\"#{html_escape(element.last.to_s)}\">#{html_escape(element.first.to_s)}</option>"
         end
       else
         is_selected = ( (selected.respond_to?(:include?) && !selected.is_a?(String) ? selected.include?(element) : element == selected) )
         options << ((is_selected) ? "<option value=\"#{html_escape(element.to_s)}\" selected=\"selected\">#{html_escape(element.to_s)}</option>" : "<option value=\"#{html_escape(element.to_s)}\">#{html_escape(element.to_s)}</option>")
       end
      end
     
     options_for_select.join("\n")
    end


### Markaby Helper

We will now rewrite this code with inline markaby. We need therefore the following helper method:

    @@ruby

    def markaby(&proc)
      assigns = {}
      instance_variables.each do |name|
        assigns[ name[1..-1] ] =  instance_variable_get(name)
      end
      Markaby::Builder.new(assigns, self).capture(&proc)
    end

We need to collect the instance variables of the current template and
pass a hash of instance variable names along with their values to the
markaby builder. As second parameter we pass the current template, so
that the builder can access other helper methods. 

### Usage

Ok, let's rewrite the `options_for_select` helper. The method takes an
array of values which should be displayed as options. Alternatively
you may pass an list of pairs like `[['first',1],['second',2]` or an
_Hash_, which maps from option labels to their values. One thing I did
was to refactor the `is_selected test` into a lambda. It is cleaner to
separate the test and probably more efficient. Inside the loop we are
testing, if we have pairs or simple values and generate markup by
sending the option method to the builder, which causes the markaby
builder to generate an option tag. Tag attributes are defined with a
hash, which we pass to the option method. A tag method takes an
optional block, which defines the content of a tag, in our case simply
the text of the option.

    @@ruby

    def options_for_select(container, selected = nil)
      container = container.to_a if Hash === container
     
      if selected.respond_to?(:include?) and !selected.is_a?(String)
        is_selected = lambda { |e| selected.include? e }
      else
        is_selected = lambda { |e| selected == e }
      end
     
      is_pair = lambda {|e| !e.is_a?(String) and e.respond_to?(:first) and e.respond_to?(:last) }
     
      markaby do
        container.each do |element|
          if is_pair[element]
            if is_selected[element.last]
              option(:value => element.last, :selected => 'selected') { h element.first }
            else
               option(:value => element.last) { h element.first }
            end
          else
            if is_selected[element]
              option(:value => element, :selected => 'selected') { h element }
            else
              option(:value => element) { h element }
            end
          end
        end
      end
    end


### Reusable helpers

Our defined markaby method is even more useful, we can accept a block
for our helper method and use it inside the markaby code:

    @@ruby

    def tasks(&block)
      markaby do
        div.tasks {
          ul {
            markaby(&amp;block)
          }
        }
      end
    end

If we have a common pattern like a list of tasks for many templates,
we can generate the common code with the tasks method and put the
actual tasks in the block:

    @@ruby

    tasks {
      task 'Back to articles'.t, articles_url
      task :edit, @article
      task :versions, @article
    }

So, you can see, there is also a task helper, which is defined as follows:

    @@ruby

    def task(text, url_or_resource, html_options={})
      if text.is_a? Symbol
        task "#{text.to_s.humanize}".t, {:action => text, :id => url_or_resource}, html_options
      else
        markaby { li { link_to text, url_or_resource, html_options } }
      end
    end

If the link text is a symbol, we are going to infer the url from the
action name which is the first parameter and the recource, which is
the second parameter in this case. Otherwise we generate a list
element and delegate the arguments to the `link_to helper`. 

By using this simple abstraction, we have hidden the details of task
links. Instead of repeating the same pattern over and over again, we
have a common place to decide, how the tasks should look like. Markaby
makes it really easy to generate nested structures, as it takes
advantage of ruby's block syntax.
