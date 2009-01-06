---
title: Projects
---

This blog is mostly about publishing projects and interesting
code. You will find here projects written in all kind of languages,
mostly in Ruby, Javascript, Actionscript and C/C++.

## [Kontrol - a micro framework](kontrol)

Kontrol is a small web framework written in Ruby, which runs directly
on [Rack][5]. It provides a simple pattern matching algorithm for routing
and uses GitStore as data storage.


## [GitStore](gitstore)

GitStore implements a versioned data store based on the revision
management system [Git][1]. You can store object hierarchies as nested
hashes, which will be mapped on the directory structure of a git
repository. Basically GitStore checks out the repository into a
in-memory representation, which can be modified and finally committed.


## [Shinmun](shinmun)

Shinmun is a small git-based blog engine. Write posts in your favorite
editor, git-push it and serve your blog straight from a
repository.


## [Patroon](patroon)

Patroon is a template engine written in Javascript in about 130 lines
of code. It takes existing DOM nodes annotated with class names and
expand a data object according to simple rules. Additionally you may
use traditional string interpolation inside attribute values and text
nodes.

