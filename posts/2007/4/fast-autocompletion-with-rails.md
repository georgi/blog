--- 
guid: 9dba67e0-5d6c-012b-b53a-001a92975b89
category: Ruby
tags: ajax, rails, scriptaculous, json
languages: ruby, html, css
date: 2007-04-04

Fast Auto-completion with Rails, Scriptaculous and JSON
=======================================================

Inspired by the excellent [Rails Recipes][1] book , I created an
improved Auto-completion helper, which uses _JSON_ and _AJAX_ instead
of a script tag for loading the completions. What we want to achieve
is a search field, which pops up immediately, showing us a list of
possible completions for our search word. Look at [Google Suggest][2]
to get an idea.

### Rails Autocomplete

Rails already has an `auto_complete_field`, which sends an AJAX
request for each keystroke. This approach is quite slow, but works in
most cases, especially for large datasets `auto_complete_field` is the
better choice. Our idea, stolen from the Rails Recipes book is to
fetch the array of possible completions only once. Each keystroke will
trigger only a local lookup and need no further server interaction.

Scriptaculous already has the right tool for this job:
[Autocompleter.Local][3].  We will just pass a javascript array of
possible completions to the constructor and we're done.


### CSS

OK, let's start. First we need the CSS used by `Autocompleter.Local`,
which styles the choices box:

    div.auto_complete {
      width: 350px;
      background: #fff;
    }
    
    div.auto_complete ul {
      border:1px solid #888;
      margin:0;
      padding:0;
      width:100%;
      list-style-type:none;
    }
    
    div.auto_complete ul li {
      margin:0;
      padding:3px;
    }
    
    div.auto_complete ul li.selected {
      background-color: #ffb;
    }
    
    div.auto_complete ul strong.highlight {
      color: #800;
      margin:0;
      padding:0;
    }
    


### Controller

Rails already has an controller macro for generating a auto completion
action. We will create a similar macro, which will generate an action,
which in turn generates the _JSON_ response. Sounds complex, but the
implementation is quite easy. Just add to your ApplicationController:

    def self.fast_auto_complete_for(object, method, options = {})
     define_method("auto_complete_for_#{object}_#{method}") do
       render :json => object.to_s.camelize.constantize.find(:all).map(&method).to_json
      end
    end
    
The response of the generated action will now contain a list of all
values for the desired attribute. You can use it like in your
controllers: `fast_auto_complete_for :sport, :name`

### Javascript Helper

Now let us get into the tricky part: the javascript macro helper. How
will we get the completion list? _Prototype_ includes the `Ajax.Request`
class, which sends an Ajax Request to our generated action and fetches
the array encoded as _JSON_. Furthermore we have to generate a div which
will hold the popup list for our completion entries. Without going
into detail, I'll just show you the code, which you add to your
`ApplicationHelper`:

    def fast_auto_complete_field(field_id, options={})
      div_id = "#{field_id}_auto_complete"
      url = options.delete(:url) or raise "url required"
      options = options.merge(:tokens => ',', :frequency => 0 )
      script = javascript_tag <<-end
        new Ajax.Request('#{url}', {
          method: 'get',
          onSuccess: function(transport) {
            new Autocompleter.Local('#{field_id}', '#{div_id}', eval(transport.responseText), #{options.to_json});
          }
        });
      end
      content_tag 'div', script, :class => 'auto_complete', :id => div_id
    end
    
Our helper needs the id for the text field we want to enhance. Based
on this id the helper generates the div for presenting the completion
entries. It is also required to pass the url of the json action, which
is in our case `/sports/auto_complete_for_sport_name`. 

### Usage example

    <form>
      <input type="text" name="name" id="sport_name"/>
      <input type="submit" value="Search"/>
    </form>
    
    <%= fast_auto_complete_field :sport_name, :url => '/sports/auto_complete_for_sport_name' %>
    
Well, that's it. Now you may enjoy snappy auto-completion and feel
good about using bleeding edge technology like `AJAX` and `JSON`.


[1]: http://www.pragmaticprogrammer.com/titles/fr_rr/
[2]: http://labs.google.com/suggest/
[3]: http://wiki.script.aculo.us/scriptaculous/show/Autocompleter.Local
