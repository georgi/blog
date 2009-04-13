--- 
title: Relax NG Schema for Adobe Flex 3
---

In the past Flex development on Emacs was not as it should be because
of a missing Relax NG Schema, which is needed for nxml-mode to work
properly. To improve the situation I developed a MXML Schema generator
based on the [XSD generator][1] of [Ali Mansuroglu][2]. Now Emacs
knows your Schema and can help you typing and validating MXML files.


### Emacs NXML completion 

Editing MXML files is now a breeze with the superb completion features
of nxml-mode on Emacs:

* completion of tag names

* completion of attribute keys

* completion of attribute values (e.g. enumerations)


### How does it work?

The schema generator reads a manifest file with all classes to be
exported.  Now for each class the generator asks the runtime to
provide the needed information about attributes, events and
styles (the flash runtime has a nice function called `describeType`,
which returns a XML representation of a type).

The tricky part is to convince the *MXML* compiler to include all
classes and metadata into the *SWF* file. Thanks to Ali, I needn't to
figure this out, as he provides an Ant file which already has the
needed compiler flags.


### How does the schema look like?

The schema just lists all class names as elements and lists their
properties, events and styles as attributes. Furthermore as much type
information is preserved as possible using xsd types. So for example
the `dropdownWith` of a `ComboBox` must be a float. XML Editors like
nxml-mode in Emacs can validate this as yout type.

This is just a snippet from the schema file:

    grammar {
      ComboBox = element ComboBox {
        attribute implements { text }?,
        attribute labelFunction { text }?,
        attribute labelField { xsd:string }?,
        attribute dropdownWidth { xsd:float }?
        ....
      }      
    }

I'm using named patterns here, because I need a pattern which matches
a choice of all possible components:

    any = Accordion
    | AddChild
    | AddChildAction
    | AddItemAction
    | AnimateProperty ....


Why do I need this pattern? Well, all container components may have
child components and to enable this in the schema, the conteiner
elements have this pattern inside:

    Application = element Application {
      any?,
      ...
    }


### Problems

There are some known issues, which are not resolved yet. For example
the `Script` tag may have a `CDATA` element inside. Another ugly issue
is the color syntax. Inside the Flex framework a color is just an
integer, but we want to specify colors as hex values. Unfortunately
xsd:integer does not allow hex values, so I have to find a solution
for this some day.


### Download

Download the [Relax NG Compact Schema for Adoble Flex 3][3]


### Github

Watch or fork the [Project on Github][4]


[1]: http://code.google.com/p/xsd4mxml/
[2]: http://flexiglas.blogspot.com/
[3]: http://www.matthias-georgi.de/assets/download/flex3.rnc
[4]: http://github.com/georgi/mxml-rnc
