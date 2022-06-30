# nbtdb

This is a quick hack to try to make an NBT editor as an excuse to learn
Clojure.

The concept of this is derived from the old Berkeley Unix `fsdb`, which
allowed creating a shell that could work on an unmounted filesystem and
perform key tasks like clearing corrupted inodes, etcetera. The use case
is that sometimes Minecraft gets broken data into an NBT file, like an item
in inventory which causes a crash if you try to render its icon, making
it impossible to fix the problem within the game.

Solution: A way to interact with NBT files and alter them.

## Installation

N/A for now.

## Usage

`lein run` to invoke. Options:

* `-i`: run interactive shell
* `-e`: run individual commands

### Commands

NBT files are conceptualized as working like a filesystem, with lists,
arrays, and compounds viewed as directories. When `nbtdb` reads an NBT
file, it then works like a command shell "in" that file's virtual filesystem.

This roughly follows Unix man page conventions; `[-x]` implies an optional `-x`,
and `[-d <depth>]` implies an optional `-d` that takes a parameter specifying
a depth, rather than the literal word `depth`.

* `cd`: Change to a given node. Double quotes (`"node"`) can be used when a
  node name has spaces and such. Does not gracefully handle node names with
  slashes in them.
* `ls [node]`: display the contents of the node, or the current directory if
  no node is given. This displays values of non-aggregate nodes (such as
  numbers or strings).
* `show [-d <depth>]`: Show the contents of a named thing; this is similar to `ls`,
  but recurses. If no depth is specified, goes until it runs out of data.
* `rm <node>`: Deletes the given node.
* `mv <node> <name>`: Moves the given node. If `name` already exists, and is
  a container, attempts to move the thing into that container. If it already
  exists and is not a container, overwrites it. If it does not already exist,
  creates it. (unimplemented)
* `mkdir <name>`: Create a compound tag with the given name. Only works when
  the current node is a compound tag. (unimplemented)

## Bugs

Unimplemented

## License

Copyright © 2022 Seebs

I haven't picked a license yet. You probably don't want to use this
anyway.
