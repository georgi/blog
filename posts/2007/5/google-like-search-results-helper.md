--- 
category: Ruby
date: 2007-05-10
languages: ruby
tags: google, search, railsGoogle-like Search Results Helper
=================================

Representing your search results in a user-friendly way is a common
task among web developers. Google's approach is dead simple but really
effective. The matching text is highlighted and shown with its
context. This can be implemented in less than 20 lines of code which
you can include into you helper:

    @@ruby

    def highlight_text(text, words)
      tokens = strip_tags(text).split
      sections = []
      words.each do |word|
        word = word.downcase
        tokens.each_with_index do |token, i|
          if token.downcase.include? word
            section = tokens[i-10, 20].join(' ')
            words.each do |word|
              section = highlight section, word
            end
            sections << section + ' ... '
            break if sections.size > 3          
          end
        end
      end
      sections.join
    end

First we remove the html tags of the text and split the text into
tokens. Then we iterate over the search words and for each matching
token we generate a section, which contains the highlighted word. This
is done until we hav at least 4 sections, which are finally joined and
ready to be included into the search result template.