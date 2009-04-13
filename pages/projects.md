---
title: Projects
---

This blog is mostly about publishing projects and interesting
code. You will find here projects written in all kind of languages,
mostly in Ruby, Javascript, Actionscript and C/C++.


## [RackDAV - Web Authoring for Rack](rack_dav)

RackDAV is Handler for Rack, which allows content authoring over
HTTP. RackDAV brings its own file backend, but other backends are
possible by subclassing RackDAV::Resource.


## [Kontrol - a micro framework](kontrol)

Kontrol is a small web framework written in Ruby, which runs directly
on Rack. Basically you can mount a class as rack handler and attach a
set of named routes, which will be used for route recognition and
generation.


## [GitStore - using Git as versioned data store in Ruby](gitstore)

GitStore implements a versioned data store based on the revision
management system Git. You can store object hierarchies as nested
hashes, which will be mapped on the directory structure of a git
repository. Basically GitStore checks out the repository into a
in-memory representation, which can be modified and finally committed.


## [Shinmun - a git-based blog engine](shinmun)

Shinmun is a small git-based blog engine. Write posts in your favorite
editor, git-push it and serve your blog straight from a repository.


## [Patroon - a javascript template engine](patroon)

Patroon is a template engine written in Javascript in about 130 lines
of code. It takes existing DOM nodes annotated with class names and
expand a data object according to simple rules. Additionally you may
use traditional string interpolation inside attribute values and text
nodes.


## [Relax NG Schema for Adobe Flex 3](mxml-rnc)

In the past Flex development on Emacs was not as it should be because
of a missing Relax NG Schema, which is needed for nxml-mode to work
properly. To improve the situation I developed a MXML Schema generator
based on the XSD generator of Ali Mansuroglu. Now Emacs knows your
Schema and can help you typing and validating MXML files.
