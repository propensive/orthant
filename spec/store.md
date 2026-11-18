# The Orthant store

This is the format of the `.orthant/` directory, version 1. The store is plain text, is
committed to git, and is written only by the `orthant` tool. It holds the current state of every
version of every document; it keeps no history of its own.

## Layout

```
.orthant/config.tel                 axes, values, parents and settings
.orthant/.gitattributes             `* -text`: git must not alter line endings
.orthant/data/root/<path>           a base record at the root point
.orthant/data/<point>/<path>        a base record at another point
.orthant/data/<point>/<path>.delta  a delta or absent record
.orthant/work/                      view state, pristine copies and the lock; git-ignored
```

`<point>` names the coordinates on which a point is not at its axis's root, as
`axis=value` pairs joined by commas in the configuration's axis order, for example
`language=fr_FR,release=77e0d1a9`. A `/` in a value is written `_`. The point at the root of
every axis is `root`. The pairs are read as a set, so adding an axis renames nothing.

`<path>` is the document's path in the working tree. A document's name does not end in
`.delta`, except at the root point, which has no deltas.

The name of an axis or a value begins with a letter or a digit and continues with letters,
digits, `.`, `+` and `-`; a value may also contain `/`, and neither is `root`.

## Lines

A document is a list of lines. In a file each line ends with a newline; a last line which has
none is a line all the same, and is given one when the file is next written. A file with nothing
in it is a document with no lines.

## Configuration

`config.tel` is a TEL document:

```
format 1
tree docs
axis
  name language
  kind language
  prefer own
  value
    name en
  value
    name fr
    parent en
    translation true
  value
    name fr/CA
    parent fr
```

`format` is the version of this format, and `tree` is the directory of the working tree,
relative to the directory which holds `.orthant/`. The axes are in order of precedence. An
axis's `kind` is `language` or `custom`, and `prefer` is `own` or `parent`: what a stale hunk on
that axis shows; it cannot change once a delta on that axis has a hunk. One value of each axis has no `parent`, and is its root. `translation true`
marks the edge from a value to its parent as a translation; on a `language` axis the tool sets
it where the two values differ before the first `/`.

An axis of kind `git` follows a repository, and may have a `pattern`: the tags it takes as
values, in which `*` stands for any run of characters. Its values may have:

```
  value
    name 77e0d1a9c0de
    parent 5b3c2f109a44
    label v2.4.0 v2.4
    commit 77e0d1a9c0de41f0b6a2c3d4e5f60718293a4b5c
  value
    name main
    parent 77e0d1a9c0de
    branch true
```

`commit` is the hash of the commit a value stands for, and `name` is its first twelve digits,
so a point's directory is named by the commit and not by a tag, which may move. `label` is the
tags which name the commit, separated by spaces; a value may be given by any of them, and is
shown by the first. `branch true` marks the value of a tracked branch, whose name is the
branch's: what is not yet released on it.

## The working tree's state

`.orthant/work/` is not committed. It holds:

```
view.tel            the point which is checked out, and the mode of the view
pristine/<path>     each document as it was checked out
lock                held while a command changes the store or the working tree
staging             a file being written, before it is moved into place
```

`view.tel` has a `mode`, which is `direct`, `indirect` or `change`, and a `coordinate` with an
`axis` and a `value` for each axis which is not at its root. Without it, the root point is
checked out. In the mode `change` it also has `from`, as `axis=value`: the earlier value from
which the change to the reference is taken.

A pristine copy is the document as the store gives it in that mode. A document in the working
tree differs from its pristine copy if it has been edited. A check-in is refused if the store no
longer gives the pristine copy, until `orthant update` has brought the working tree up to it.

## Records

Each (point, document) has at most one record.

- A **base** record is the document's full text, verbatim, in a file with the document's own
  name. It does not depend on any other point.
- A **delta** record is a list of hunks against the point's storage parent, in a `.delta` file.
- An **absent** record says the document does not exist at this point and below. It is a `.delta`
  file whose header ends in `absent`, with no hunks.

A base file and a `.delta` file for the same point and document must not both exist.

## Delta files

The first line is the header:

```
orthant 1 <axis>
orthant 1 <axis> absent
```

`1` is the format version and `<axis>` is the storage axis: the first axis, in configuration
order, on which the point is not at the root. Its presence makes a change of axis order
detectable.

Hunks follow, each preceded by one blank line. Every line of a hunk begins with a sigil; a sigil
is followed by one space and the line's text, or by nothing for an empty line.

| Sigil | Meaning |
|---|---|
| `=` | a context line: present in the storage parent, unchanged, and used only to locate the hunk |
| `!` | a former line: what this hunk replaced before the storage parent changed beneath it |
| `-` | a replaced line of the storage parent |
| `+` | a new line |
| `^` | the start of the document; only as the first line of a hunk, with no text |
| `$` | the end of the document; only as the last anchor line of a hunk, with no text |

Within a hunk the order is: `^` if present, leading `=` lines, `!` lines, `-` lines, trailing
`=` lines, `$` if present, then `+` lines. The block made of the leading context, the replaced
lines and the trailing context is the hunk's **anchor**: it must occur exactly once, as
consecutive lines, in the storage parent's content. A hunk with `!` lines is stale.

A hunk with no `-` lines is an insertion, placed after its leading context; it has a line of
context at least, or `^`. A hunk with neither `-` nor `+` lines inserts nothing: it stops an
insertion which the point would otherwise inherit at that place. A hunk whose `+` lines are its
`-` lines is written only on a translation edge, where it says that those lines are translated
by themselves.

The tool writes the least leading context which makes the anchor occur once, and `^` when every
line before the hunk is not enough. It writes no trailing context and no `$`, but reads both.

Hunks are written in the order of their anchors in the storage parent, but each stands alone
and a reader must not depend on the order. Any line that is neither blank nor begins with a
sigil is an error, so a git conflict marker cannot be read as content.

```
orthant 1 language

- Install the tool.
+ Installez l'outil.

! Run it.
- Run it from a terminal.
+ Lancez-le.

= ## macOS
- Run the installer.
+ Ouvrez le fichier .dmg.

= Last line.
$
+ <!-- traduction : équipe FR -->
```

## Later versions

A later version may encode an anchor as a position, a count and a hash of the replaced lines
instead of their text. The engine therefore treats an anchor abstractly, and the header's
version number says which encoding a file uses.
