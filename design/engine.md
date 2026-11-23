# The Orthant engine

The rules by which content is derived and changed. `spec/store.md` gives the file format. These
rules were first checked on paper; the property tests in `src/test` are their proof.

## Terms

- An **axis** is a named tree of **values** with one root.
- A **point** has one value per axis. Point A is **below** point B if each of A's values is B's
  value or a descendant of it.
- A **document** is a path. Its **content** at a point is a list of lines, or absent.
- A **hunk** replaces some consecutive lines of a content with new lines.
- The **storage axis** of a point is the first axis, in configuration order, on which it is not
  at the root. Its **storage parent** is the same point with that axis moved to the parent value.

Axis order is precedence: translation axes come first, and reordering axes is refused once
content exists.

## Edges

An edge between a value and its parent is a **variation** or a **translation**. On a language
axis the edge is a translation when the language codes differ. Hunk boundaries are kept as the
author wrote them on translation edges, and trimmed to the smallest change on variation edges.

## Content

The content of the root point is its base record. For any other point `p` with storage axis `a`
and storage parent `q`:

1. Let `O` be the points with `p`'s value on `a`, root on the axes before `a`, and `p`'s value
   or an ancestor of it on the axes after `a`. `p` is in `O`.
2. The most specific record among `O` decides existence. A base gives the content outright; an
   absent record gives no content; a delta more specific than either cancels it.
3. Otherwise take `R`, the content of `q`. If `q` has no content, neither has `p`.
4. Place every hunk stored at a point `o` in `O`: locate it in the content of `o`'s storage
   parent, then re-base it onto `R`.
5. Resolve overlaps, and replace the resolved spans of `R`.

### Locating

A hunk's anchor is its context and replaced lines as one block. The store holds how many lines
each is and an imprint of the block, a hash from which the imprint of every run of a content is
quickly had, and not the lines: they are the reference's, where the block is found. One
occurrence in the reference places the hunk. With none, or several, the hunk stored at that point is reported as an
**orphan** or as **ambiguous**, and contributes nothing.

### Re-basing

Given the old and new reference, diff them. A hunk is

- **fresh** if its whole anchor lies within unchanged lines, or if its replaced lines do and
  occur once in each reference;
- an **orphan** if its replaced lines fall wholly within a deletion; it contributes nothing;
- **stale** otherwise. Its span is widened to the whole changed regions it touches, hunks
  sharing a region are merged, and its former lines are the old reference's lines for that span.

An insertion is fresh if the line before it or the line after it is unchanged, or if it is at an
end of the document. A stale hunk which finds again the very lines it was first written against
is fresh again.

Hunks which were apart, and come to insert at the same place because the lines between them are
gone, become one insertion there, of the lines of each in the order they were in. Two hunks at
one place could not both apply, so a delta never holds them. Where stale hunks are merged, their
own lines are likewise taken in that order, an insertion before a replacement which begins where
it is.

A stale hunk shows what its axis prefers: `prefer own` (the default) shows the hunk's own lines,
`prefer parent` shows the reference's. The preference is part of the content, so changing it is
refused once content exists.

### Resolution

Where placed hunks overlap, fresh beats stale; then the hunk from the more specific point wins;
two points neither below the other are ordered by depth on the earliest axis, and the spot is
flagged as a **conflict** if their lines differ. A hunk is accepted if it overlaps nothing
already accepted. Two insertions at the same place overlap, as does an insertion strictly within
the lines another hunk replaces.

## Storing

To make a point's content some new lines, the content as it is rendered is diffed against them.
Each change is widened so that it cuts through no hunk, and one new hunk is stored for it, in
place of every hunk within it; a hunk stored at the point which no change touches is kept as it
is. So a hunk's boundaries last until an edit crosses them.

- A new hunk grows to take in any hunk it would otherwise overlap.
- An inherited insertion which a new hunk replaces, but which lies at an end of the new hunk's
  span, is stopped by a hunk which inserts nothing at that place.
- A hunk without which the content and its issues are the same is not stored.
- A stale hunk which is edited is still stale. **Confirming** a point stores each stale hunk
  there as it is shown, against the lines its storage parent now has, and so clears it.

## Check-in

A check-in has a storing point and a **scope**: the points whose content may change.

1. Record the content of every point below the storing point.
2. Store the new content at the storing point.
3. Visit the other points below it, parents first. Where a storage parent's content changed,
   re-base the hunks stored there; a hunk whose lines were deleted is dropped.
4. **Pin** any point outside the scope whose content changed, by storing its old content there.
5. Verify that the checked-out point's content is the new content, that nothing outside the
   scope changed, and that every stored hunk locates exactly once. If not, refuse.

| Scope | Stored at | May change |
|---|---|---|
| default | the checked-out point | it and every point below it |
| `--only <axis>` | the checked-out point | points below it with its value on that axis |
| `--since <value>` | the checked-out point moved to that ancestor | every point below that |
| `--origin <axis>` | per change, the value that introduced the replaced lines | every point below each |

Nothing is ever merged interactively. What does not commute is refused with a report.

For `--since` and `--origin` the edit is taken as its changes: the runs of lines by which the
checked-out point's content becomes the new content. A run which gives way to as many lines is
a change to each line which differs, so that each can be sent to its own place. Each change is
carried, by re-basing, onto the content of the point it is sent to, where it must be fresh, and
is checked in there with the default scope, the furthest ancestor first.

- `--since <value>` sends every change to the checked-out point moved to that value, which must
  be an ancestor.
- `--origin <axis>` sends a change which replaces or deletes lines to the furthest ancestor
  along that axis which has those lines, as every value between does. A change which only
  inserts lines stays at the checked-out point; one which replaces lines by more lines goes
  whole to where the replaced lines are.

Afterwards the checked-out point's content must be the new content. If a change cannot be
carried (`↯/o.3.4`), or the result is not the new content (`↯/o.3.5`), nothing is stored.

## Indirect views

The indirect view of a document at a point is its storage parent's content, the reference, with
the point's hunks in place. Every line has a sigil, then a space and the text, or the sigil
alone for an empty line; a line with nothing on it is an empty line which the two share.

| Sigil | Meaning |
|---|---|
| two spaces | a line which the reference and the content share |
| `?` | on a translation edge, a line of the reference which no hunk replaces: yet to be translated |
| `-` | a line of the reference which is replaced |
| `+` | a line which replaces it, or is inserted |
| `!` | a line which a stale hunk was written against, before the reference changed |

A run of `!`, then `-`, then `+` lines is a passage. A stale hunk is a passage with its former
lines, the reference's lines now, and its own lines, whatever its axis prefers to show. A
document which is held in full, or whose storage parent has none, is all `+` lines.

A `?` line which `+` lines follow, with nothing between them but other `?` and `-` lines, is
read as a `-` line: adding `+` lines beneath a `?` line translates it.

At a check-in of a view:

1. The shared, `?` and `-` lines, in order, must be the reference exactly.
2. A passage with `!` lines must be a stale hunk as it was shown, though its `+` lines may
   differ. Its `!` lines may be deleted, all of them, and not otherwise changed.
3. Each stale hunk whose `!` lines were deleted is confirmed.
4. The content is the shared and `?` lines with each passage's `+` lines; a stale passage which
   was left wholly alone gives what it showed before. That content is checked in as any other.
5. On a translation edge, a passage whose `+` lines are its `-` lines is stored as a hunk which
   replaces lines by the same lines, so they are pending no longer; in the store it is a kept
   hunk, which has no lines of its own. Such a hunk is kept by later
   check-ins, though the content would be the same without it, and is inherited like any other.
6. A passage whose `-` lines were left out by one hunk, and whose `+` lines another hunk
   inserts beside them, becomes one hunk which replaces them, provided that changes neither
   the content of any point nor what wants attention there.

An edit to a stale hunk leaves it stale only on an axis which prefers its own lines. On one
which prefers the parent's, the hunk showed the parent's lines, so an edit there is new text,
and is stored fresh.

### Views against a change

A view may instead be taken against a change to the reference: what the reference was at an
earlier value of an axis, an ancestor of the point's own value there, on an axis other than its
storage axis. For `(fr, v2.4.0)` and the change `v2.3.0..v2.4.0`, the reference is
`(en, v2.4.0)` and the former reference is `(en, v2.3.0)`.

Lines of the point's content which nothing touches are shared lines; here those are the
point's own lines, not the reference's. A passage is shown for each stale hunk, and for each run
of the reference's lines which changed and which no fresh hunk wholly replaces; it grows to
take in every hunk it touches. It shows what the reference was there as `!` lines, what it is
as `-` lines, and the point's content there as `+` lines. Once a fresh hunk replaces the changed
lines, the passage has been seen to, and is shown no longer. Blank lines at either end of what
changed call for nothing, and are not part of a passage.

On a translation edge, a passage which no hunk touches is lines yet to be translated, and is
shown as `?` lines alone, as it would be beside the reference.

At a check-in the content is the shared lines, the `?` lines which were left alone, and the
`+` lines. The `-` lines which are not blank must be those which were shown, in order, though
one passage may be made into several; the lines of a `?` passage are among them only where they
were translated. A `?` passage every line of which is replaced by itself is stored as
translated by the same lines.
A passage's `!` lines may be deleted, all of them, which confirms whatever is stale in it; they
may not otherwise change. `!` lines never make anything stale.

## The working tree

The working tree holds each document at the point checked out, in one of three modes: plain,
beside its storage parent, or against a change. Beside each is a pristine copy, as the store
gave it.

- **Changing the reference** keeps edits which are not checked in. Each edited document is
  shown, in the new mode, as it would be if its edits were checked in, beside a pristine copy in
  which they are not.
- **Updating**, when the store has changed beneath the working tree, writes each document again
  as the store now has it. An edit which is not checked in is carried onto it as a change is
  carried for `--since`; if any cannot be, nothing is written (`↯/o.4.12`).
- **A hunk with no one place** (`↯/o.1.8`) is found only by deriving, so a command does not look
  for it throughout the store before it starts: `check` does that. A command which reads a
  document at a point refuses it if a hunk stored at that point has no place, and one which
  changes a document refuses it if any of its hunks, at any point, has none. `status` and `todo`
  count such a hunk among what wants doing.

## What is kept between commands

The tool runs as a daemon, and a command is a call to it. A store which a call has read is
kept (`Kept`), with a stamp of its configuration and of each of its record files: when each
was written, its size, and what the filesystem knows it by.

- **A store whose files are as they were is not read again.** The next call is given the same
  store, and with it everything which was derived in it: a rendition is derived once in the
  life of the daemon, and not once in each call.
- **A store of which a file has changed is read again**, and takes from the one before the
  dossier of each document whose records are the same, so what was derived of those documents
  is kept. A check-in keeps the store as it leaves it.
- **Calls share what is kept**, so what a dossier and a store remember of what they derived
  may be read and written by several tasks at once. Two tasks which derive the same thing
  derive the same value, and either is kept.
- **A store is kept softly**: the memory it takes is given up when it is wanted for anything
  else, and the store is then read again.

The library is unchanged by this: `Workspace.open` reads a store, and keeps nothing.

`check` derives every document at every point, which is more than can be held at once where
the points are many: it derives a few documents at a time, each in a task of its own, and gives
up what it derived of each when its points have been looked at (`Workspace.survey`).

## What wants doing

The work in a document at a point is its lines yet to be translated, its stale passages, and
its hunks which conflict or have no place. A point inherits the hunks of the points with its
value on its storage axis and an ancestor's value elsewhere, so work which one of those has too
is best done there: only the rest is the point's own.

- `orthant status` counts the edits which are not checked in as if they were. Given values, or
  `--all`, it lists each point's own work, and counts what it shares with the points above.
- `orthant todo` lists each point's own work, and with `--checkout` checks out the first such
  point: against the change from the value before it on an axis, if nothing wants doing at
  that value, and otherwise beside its storage parent.
- `orthant grid` shows a cell for each pair of values of two axes, with the other axes at the
  values checked out: the share of lines translated, then the counts of lines to translate
  (`?`), stale passages (`!`) and faulty hunks (`x`), coloured by the worst of them.
- A check-in reports, for each point whose own work it changed, by how much.
- A strict export is refused while any document at the point has work.
- An export along an axis writes the documents at every value of that axis, the other axes as
  at the point, each value's in a directory of its name. One store serves them all, so what the
  values inherit from each other is derived once: a chain of values costs what its last value
  does, and not that for each of them. A strict one writes nothing if any of them has work.

A stale passage is reported by its line in the working tree, in whatever mode that is shown,
at the point checked out, and by its line in the plain document elsewhere.

## Following a git repository

An axis of kind `git` takes its values from a repository. `sync` reads the tags which the
axis's pattern takes. Each tag's commit becomes a value: its name is the first twelve digits of
the commit's hash, or the whole hash where a value of the store has those twelve for its name
already, and the tag is its label, by which it is shown and may be named. Its parent
is the nearest value whose commit is an ancestor of it, or the axis's root. If two such values
are nearest, neither an ancestor of the other, the sync is refused (`↯/o.7.2`), and the tag is
given its parent by hand. Where the root has a commit, a tag of a commit which is not descended
from it is older than what the axis follows, and is not taken.

A tracked branch has a value of its own name, beneath the nearest tagged ancestor of its tip:
what is not yet released there. When a new tag is found between that parent and the tip, the
branch's value becomes the tag's, with everything stored at it, and a new, empty value of the
branch's name is put beneath it. So what was checked in on the branch since the last release is
what was released; anything checked in after the tag was made, and before the sync, is part of
it too, and the sync says so.

A sync adds values and renames one; by invariant 6 it changes no content. The renamed value's
directory is renamed wherever it is: within the directory of each point, on the earlier axes,
at which something is stored at the value. A branch whose name a value of another axis has is
not tracked (`↯/o.2.6`), since no two values of a store have one name.

## A sourced axis

An axis may have a source (`spec/store.md` §7.4): its values are commits of a git repository,
and the content at a commit, where every other axis is at its root, is what the commit's tree
has. The store holds nothing at those source points, and every other point derives as it
always did, its storage parent and its origins being found from the earlier axes. So a patch
checked in at one commit is hunks against the source there, and is re-based onto the source at
each later commit as any hunk is onto a storage parent which has changed.

- **The engine reads nothing.** A `Dossier` is given a `Source`, which answers for a value of
  the sourced axis with the document's lines. The reader makes one for each document from an
  `Upstream`, which is the repository as far as a reader needs it; the tool's is
  `Sourcing.Repository`, over the git library.
- **The axis is a tree, and the history is not.** A value's parent is one of the values whose
  commits are its commit's ancestors. `check` refuses a parent which is not (`↯/o.7.6`), and
  the tool gives a new value the nearest of them, refusing a commit which has two, neither an
  ancestor of the other (`↯/o.7.2`). Lineage over the true history is not attempted.
- **A commit need not be a value.** Wherever a value is named, a revision of the source may
  be: the commit is put beneath its nearest ancestor among the values, and above those of that
  value's children which descend from it, in memory. By invariant 6 that changes no content,
  and the configuration is not written. The commit must descend from the axis's root
  (`↯/o.7.8`), and where the repository's history is shallow and does not reach it, that is
  said (`↯/o.7.9`).
- **The history is read once.** Whether one commit is an ancestor of another is asked of
  every value when a commit is placed, and of every value by `check`. The repository gives
  each commit it reads a generation, one more than the greatest of its parents', and keeps it
  with the commit's parents for as long as the process runs, as a commit never changes. No
  commit descends from one whose generation is not less than its own, so a question is
  answered from the commits between the two, and the first one asked is what reads the history.
- **A check-in at such a commit makes it a value**, with the configuration written then. Its
  children among the values, which were its parent's, inherit what was checked in, as any
  point below a check-in does.
- **A branch is not a value.** A name of a branch is a revision like any other, and is the
  commit it names when the command is run. `sync` takes up the tags of the source's
  repository, and tracks no branch (`↯/o.7.10`).
- **The working tree is the source's**, with the store's documents written over it: a
  checkout writes only the documents which have a record somewhere. A file which is in the
  tree alone is the source's, and is not a change unless a check-in names it; it is then read
  as plain content, whatever the mode of the view.
- **Two issues are the source's doing** (`spec/store.md` §9.9): hunks whose document the
  source no longer has are baseless, and a base record for a document which the source has
  come to have is an overlay. Neither is an error, and `status` reports both.

## Invariants

1. After a check-in, the checked-out point's content is the new content.
2. Content outside the scope is unchanged.
3. Checking in unchanged content changes nothing in the store; writing a parsed file reproduces
   it.
4. Every stored hunk locates exactly once in its storage parent.
5. Content does not depend on the order of hunks in a file.
6. Inserting an empty value, or renaming a value, changes no content.
7. Re-basing onto the same content changes nothing.
8. A fresh hunk stored over a stale spot clears that flag and changes nothing else.
9. With one axis, a child's content is its parent's with the child's hunks applied.

All nine are tested over runs of random check-ins (`Trials` and `Solo` in `src/test`), on a
five-line alphabet so that lines repeat. Invariants 1 to 4 are checked after every step of a run
on three axes, with and without `--only`; 5 to 8 after every fifth step of the same runs; and 9
on runs over a single axis, where a point's content is worked out without the engine, from its
parent's content and the hunks it holds. `orthant.sweep` runs all of them over more seeds, and
`orthant.probe` shows the store around a failing step.

Two details of the invariants as tested:

- For 6, the empty value is put between a value and its parent, on the first axis and on the
  second, and must itself have its parent's content.
- For 8, the point's content and its other hunks are unchanged, as is every point which is not
  below it. A point below it may change, because it inherits the new hunk.

## Not yet done

- Finding a new place for a hunk whose lines were deleted and written again elsewhere: it is
  dropped, and the lines show as the storage parent has them.
- Keeping an author's hunk boundaries from an indirect check-in; boundaries come from the diff,
  which pairs paragraphs where blank lines align.
- Views against a point other than the storage parent.
- Renames of documents; a git merge driver.
- Re-parenting a value when a tag appears between two values which a sync has already made.
- On a sourced axis: lineage over the true history of commits, where a commit has two parents;
  and following a document which the source renames.
- Upgrading the tool in place, as `tel upgrade` does: Orthant has no releases to upgrade to.

## Bolt codes

Every error Orthant raises has a bolt code with the letter `o`.

| Code | Meaning |
|---|---|
| `↯/o.1.1` | a delta file's header is missing or malformed |
| `↯/o.1.2` | a line of a delta file has no sigil, or its anchor line is not written as one |
| `↯/o.1.3` | the lines of a hunk are out of order, or it has not exactly one anchor line |
| `↯/o.1.4` | a delta file is of another format version |
| `↯/o.1.5` | the store holds both a base and a delta for a document at a point |
| `↯/o.1.6` | a delta's header names an axis which is not its point's storage axis |
| `↯/o.1.7` | a directory of the store's data does not name a point |
| `↯/o.1.8` | a stored hunk does not have its lines exactly once in its storage parent |
| `↯/o.1.9` | the store holds a record at a point whose content is the source's |
| `↯/o.2.1` | a value is not on the axis |
| `↯/o.2.2` | an axis has no root, or more than one |
| `↯/o.2.3` | a value's parents form a cycle |
| `↯/o.2.4` | a name is not valid for an axis or a value |
| `↯/o.2.5` | the axis already exists |
| `↯/o.2.6` | an axis of the store already has the value |
| `↯/o.2.7` | a value given alone is on more than one axis |
| `↯/o.2.8` | no axis has the value, or there is no such axis |
| `↯/o.2.9` | hunks are stored on the axis, so what it prefers cannot change |
| `↯/o.3.1` | the checked-out content would not equal the working tree |
| `↯/o.3.2` | a point outside the scope would change |
| `↯/o.3.3` | a stored hunk would not locate exactly once |
| `↯/o.3.4` | a change could not be carried to the point it was sent to |
| `↯/o.3.5` | the changes, stored where they were sent, would not give the checked-in content |
| `↯/o.3.6` | the check-in is at a point whose content is the source's |
| `↯/o.4.1` | there is no store in the directory or above it |
| `↯/o.4.2` | the working tree has edits which are not checked in |
| `↯/o.4.3` | the store's content changed since the document was checked out |
| `↯/o.4.4` | a new document's name ends in `.delta` |
| `↯/o.4.5` | a file could not be read or written |
| `↯/o.4.6` | the document does not exist at the point |
| `↯/o.4.7` | a line of a view has no sigil |
| `↯/o.4.8` | the reference's lines in a view are not those which were checked out |
| `↯/o.4.9` | the `!` lines of a view were changed |
| `↯/o.4.10` | the point has no storage parent for an indirect view |
| `↯/o.4.11` | a strict export found stale, conflicting or untranslated lines |
| `↯/o.4.12` | an update could not carry an edit onto the store's content |
| `↯/o.4.13` | there is no view against that change at the point checked out |
| `↯/o.4.14` | a view against a change was asked for from the value checked out |
| `↯/o.5.1` | the configuration, or the record of the view, could not be read, the store is of another format, or a name is there twice |
| `↯/o.5.2` | there is already a store in the directory |
| `↯/o.6.1` | unrecognised command |
| `↯/o.6.2` | a command was given the wrong operands |
| `↯/o.7.1` | the git repository could not be read |
| `↯/o.7.2` | a tag has more than one nearest ancestor among the axis's values |
| `↯/o.7.3` | the repository has no such branch |
| `↯/o.7.4` | the axis does not follow a git repository |
| `↯/o.7.5` | the source of an axis could not be read, or none was given |
| `↯/o.7.6` | on a sourced axis, a value's commit does not descend from its parent's |
| `↯/o.7.7` | the source's repository has no commit which the revision names |
| `↯/o.7.8` | the commit does not descend from the root of the sourced axis |
| `↯/o.7.9` | the source's history is shallow, and does not reach from the commit to a value |
| `↯/o.7.10` | a branch was to be tracked on a sourced axis |
| `↯/o.8.1` | the launcher did not apply an upgrade which was staged |
| `↯/o.8.2` | the executable has no key, so it cannot be upgraded in place |
| `↯/o.8.3` | the newest release has no executable for the platform |
| `↯/o.8.4` | the release's executable could not be downloaded, or its digest was wrong |
| `↯/o.8.5` | the downloaded executable could not be staged |

Subdivisions: 1 the store format; 2 axes and points; 3 check-in; 4 the working tree and views;
5 the configuration; 6 the command line; 7 git: following a repository, and the source of an
axis; 8 upgrading the tool.
