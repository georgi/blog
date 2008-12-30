--- 
category: Emacs
date: 2008-09-04
tags: completion, hippie-expand, snippet
title: Emacs Completions with Hippie-Expand and Snippets
---






One of the most important features of a text editor is the completing
of text inside a buffer. There a lots of packages for Emacs, which
provide this feature in many different ways. I will show you, what I
use to improve my life as coder.

<div id="emacs-completion" class="swfobject"></div>

### Multifunctional tab key

Most of the time the tab key in Emacs is bound to the indentation
command, which will indent the current line. So if you want to use the
tab key for other things, you need some kind of multiplexer, which
tries to figure out, what is the right thing to do in each situation.

So I copied the indent-and-complete from the [emacs-rails package][1]:

    @@scheme
   
    (require 'hippie-exp)
    (require 'snippet)
    
    (defun indent-and-complete ()
      "Indent line and complete"
      (interactive)
    
      (cond
       ((and (boundp 'snippet) snippet)
        (snippet-next-field))
    
       ((looking-at "\\\\_>")
        (hippie-expand nil))
    
       ((indent-for-tab-command))))

The function `indent-and-complete` does one of the following actions:

* if a snippet is active, it jumps to the next field
* if we are at a word boundary, it tries to complete with `hippie-expand`
* otherwise it indents the current line


### HTML mode initialization

Well, this function alone will not change your editor behaviour. For
activating our tab function, we need to bind the tab key.

Additionally we want to setup `hippie-expand`, an expansion package,
which will try to expand the word before the cursor in a configurable
way. `hippie-expand-try-functions-list` is a variable, which defines a
list of functions, which should be called for completion.

Let's have a look at my `html-mode` initialization function. It will configure
the completion behaviour of hippie-expand an bind the tab key to `indent-and-complete`.

    @@scheme

    ;; We need a simple wrapper for expand-abbrev
    (defun try-expand-abbrev (old)
      (expand-abbrev))

    ;; ********************************************************************************
    ;; HTML Mode
    ;;
    (add-to-list 'auto-mode-alist '("\\\\.html\\\\'" . html-mode))
    
    (defun html-mode-on-init ()
      (set (make-local-variable 'hippie-expand-try-functions-list)
           '(try-expand-abbrev
    	     try-expand-dabbrev))
      (define-key html-mode-map (kbd "<tab>") 'indent-and-complete))
    
    (add-hook 'html-mode-hook 'html-mode-on-init)    

There a two functions, which will be asked to complete the current word:

* `try-expand-abbrev`: Expands the current word by looking into the
  list of defined abbreviations. So called abbrevs are just shortcuts
  in Emacs. So if you type `li` and hit the tab key it will be
  expanded to `<li></li>`.

* `try-expand-dabbrev`: Dynamic abbreviation is a pragmatic method for
  completing words. Emacs will look for words with the same beginning
  and use them for completion. Hitting multiple times the tab key will
  give you different completions, as you may know from the unix shell.


### Defining your snippets

Now if you want to use snippets for your `html-mode`, you have to
define a abbrev-table with your desired snippets. 

    @@scheme

    (define-abbrev-table 'html-mode-abbrev-table ())
    
    (snippet-with-abbrev-table 'html-mode-abbrev-table 
     ("h1"      . "<h1>$.</h1>")
     ("h2"      . "<h2>$.</h2>")
     ("h3"      . "<h3>$.</h3>")
     ("h4"      . "<h3>$.</h4>")
     ("h5"      . "<h3>$.</h5>")
     ("h6"      . "<h6>$.</h6>")
     ("div"     . "<div>$.</div>")
     ("divc"    . "<div class=\"$${class}\">$.</div>")
     ("span"    . "<span>$.</span>")
     ("spans"   . "<span style=\"$${style}\">$.</span>")
     ("form"    . "<form action=\"$${action}\" method=\"$${post}\">$.</form>")
     ("input"   . "<input type=\"$${text}\" name=\"$${name}\" value=\"$${value}\"/>")
     ("a"       . "<a href=\"$${href}\">$.</a>")
     ("br"      . "<br/>$.")
     ("ul"      . "<ul>$.</ul>")
     ("ol"      . "<ul>$.</ul>")
     ("li"      . "<li>$.</li>")
     ("tab"     . "<table>$.</table>")
     ("tr"      . "<tr>$.</tr>")
     ("td"      . "<td>$.</td>")
     ("th"      . "<th>$.</th>")
     ("str"     . "<strong>$.</strong>")
     ("em"      . "<em>$.</em>")
     ("meta"    . "<meta name=\"$${name}\" content=\"$${content}\"/>")
     ("style"   . "<style type=\"text/css\">$.</style>")
     ("script"  . "<script type=\"text/javascript\">$.</script>")
     ("scripts" . "<script src=\"$${src}\" type=\"text/javascript\">$.</script>")
     ("img"     . "<img src=\"$.\"/>")
     ("link"    . "<link href=\"$${href}\" media=\"screen\" rel=\"stylesheet\" type=\"text/css\"/>"))
	
Great. If you put all the code in your .emacs file, you should be able
to use your tab key for completions. In our case we defined snippets
for the `html-mode` and activated `hippie-expand` to use abbrevs and
dynamic abbreviation. There is much more stuff I will show you next
time, like customizations for other language modes like _Ruby_ and
_Javascript_.

    

[1]: http://dima-exe.ru/rails-on-emacs  "Emacs Rails package"
