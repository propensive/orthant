# Orthant

Orthant is a store for line-based documents that exist in many versions along independent axes,
such as language, release and verbosity. Each version cascades from its parent on every axis and
is stored as the lines that differ. The `orthant` tool checks a version out into a working tree,
optionally beside a reference version, and checks edits back in.

- `design/engine.md`: how content is derived and changed.
- `spec/store.md`: the specification of the `.orthant/` directory, and of a reader of it.

Orthant is built on [Soundness](https://soundness.dev/) alone.

## Installing it

```sh
curl -fsSL https://propensive.dev/orthant | sh
```

In PowerShell, `irm https://propensive.dev/orthant | iex`. Either installs the latest release:
as `orthant` in `~/.local/bin`, or on Windows as `orthant.exe` in
`%LOCALAPPDATA%\Programs\orthant`; or in `ORTHANT_INSTALL_DIR`, if that is set.
`orthant upgrade` replaces it with a newer one.

## Using it

```
orthant init docs                        # a store in .orthant/, a working tree in docs/
orthant axis add language en --language  # an axis and its root value
orthant axis value language fr           # a value beneath the root
orthant axis value language fr/CA fr     # a value beneath another
orthant axis add release v1
orthant axis value release v2
orthant checkout fr v2                   # the documents at those values; other axes stay
orthant status [values…] [--all]         # edits not checked in; what is translated, and what is not
orthant todo [values…] [--checkout]      # the work to do, where best to do it; and go there
orthant grid [axis [axis]]               # a coloured cell of work for each pair of values
orthant checkin [paths…] [--remove] [--dry-run]
orthant checkin --only release           # reach only the points at this release
orthant checkin --since v1               # store the edit at an earlier value
orthant checkin --origin release         # store each change where its lines first appeared
orthant checkout fr --indirect           # each document beside the one it derives from
orthant ref v1[..v2]                     # each document against a change to its reference
orthant ref --parent | --none            # beside its storage parent's; or alone, as plain text
orthant diff [paths…]                    # the edits which are not checked in
orthant update                           # after the store has changed beneath the tree
orthant confirm [paths…]                 # mark stale passages as checked, unchanged
orthant axis prefer language parent      # what a stale passage shows; before content exists
orthant cat guide/intro.md fr/CA         # a document at a point, without checking it out
orthant export site fr v2 [--strict]     # the plain documents at a point; --strict wants no work left
orthant <command> --help                 # how a command is written, and what it does
orthant check                            # verify everything the store holds
orthant install                          # tab-completions for the shell
orthant upgrade                          # replace this executable with the newest release
```

An axis can follow the git repository the store is in:

```
orthant axis add release start --git 'v*'   # values are the tags matching v*
orthant sync [branches…]                    # take up new tags; track the current branch
orthant checkout v2.4.0                     # a value is named by its tag
```

Each tag is a value beneath its nearest tagged ancestor, stored by its commit's hash. The
tracked branch has a value of its own name for what is not yet released. When a new tag is
found on the branch, that value becomes the tag's, with everything checked in at it, and the
branch starts again beneath it.

Commit `.orthant/`; the working tree is added to `.gitignore`. An edit checked in at a point is
seen at every point below it which has not changed the same lines.

In an indirect view every line begins with a sigil: two spaces for a line shared with the
reference, `-` for a line of the reference and `+` for what replaces it, `?` for a line yet to
be translated, and `!` for what a stale passage was written against. Translate a `?` line by
adding `+` lines beneath it; confirm a stale passage by deleting its `!` lines. A `-` line with
no `+` line beneath it is left out of the translation.
`--direct` goes back to plain documents. A view against a change (`orthant ref v1..v2`) shows
only what the change calls for: what the reference was as `!` lines, what it is now as `-`
lines, and the document's own lines there as `+` lines; or, for lines which are new and not yet
translated, `?` lines.

A translator need not know which release or view to ask for:

```
orthant todo fr              # each piece of work, at the earliest point which has it
orthant todo fr --checkout   # check out the first of those points, in the view which shows it
orthant status               # what is left there, counting the edits not yet checked in
orthant checkin              # store them; it says how much less there is to do, and where
orthant grid                 # every language at every release, at a glance
```

A check-in of the source documents likewise says which translations it left with lines to
translate or passages to check.

## Reading a store from another program

Each release has three libraries beside the tool, as jars among its assets on GitHub:
`orthant-engine`, which derives content and does no IO; `orthant-reader`, which opens a store
and reads it; and `orthant-core`, which is the tool. A program which only reads a store depends
on `dev.propensive:orthant-reader`, which brings the engine with it, and of Soundness needs no
more than the filesystem and TEL. A build which resolves Soundness from its releases resolves
Orthant's in the same way.

`orthant-reader` is a reader as §12 of `spec/store.md` defines one. It writes nothing.

```scala
import orthant.*, soundness.*
import filesystemBackends.javaBaseFilesystem

val store = Store.open(home)                        // `home` has `.orthant` in it
val point = store.point(List(t"fr", t"release=v2"))
store.content(t"guide/intro.md", point)             // the lines, or `Unset` if it is not there
```

A store is opened from a value of any type which is `Perusable`: one which has directories
with named entries, and files with bytes. A `Path` of a filesystem is, and an instance for
another type reads a store from wherever that keeps one, such as a commit of a git repository.
A store, once opened, is a value: all it answers is of the one state in which it was read. An
error is a `Store.Error`, raised with the bolt code the specification gives it.

| §12 | In `orthant-reader` |
|---|---|
| `open`, `find` | `Store.open(home)`, `Store.find(directory)` |
| `format`, `axes`, `points` | `store.format`, `store.axes` and `store.entry(axis)`, `store.points` |
| `point` | `store.point(terms, from)`; or a `Point` made directly |
| `documents`, `content`, `text` | `store.documents`, `store.content(document, point)`, `store.text(document, point)` |
| `faults` | `store.faults`; `Store.inspect(home)` reads a store which `open` refuses, to list them |
| `rendition`, `view` | `store.rendition(document, point)`, `store.view(document, point)` |
| `storage-parent`, `origins` | `store.storageParent(point)`, `store.origins(point)` |
| `work`, `own-work` | `store.work(document, point)`, `store.work(point)`, `store.ownWork(document, point)` |
| `export` | `store.publish(point, directory, strict)` |

`checked-out` is the tool's, since what is checked out is no part of the store.

## Building

```
make test                 # compile and run the tests with fume
make test-plain           # the same, without fume
make orthant              # the native executable
```
