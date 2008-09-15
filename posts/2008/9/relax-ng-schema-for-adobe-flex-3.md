--- 
category: Actionscript
guid: cb5421c0-654c-012b-f5f5-001a92975b89
date: 2008-09-15

Relax NG Schema for Adobe Flex 3
================================

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


### Download

Download the [Relax NG Compact Schema for Adoble Flex 3][3]


### Github

Watch or fork the [Project on Github][4]



[1]: http://code.google.com/p/xsd4mxml/
[2]: http://flexiglas.blogspot.com/
[3]: ../../download/flex3.rnc
[4]: http://github.com/georgi/mxml-rnc/tree/master
