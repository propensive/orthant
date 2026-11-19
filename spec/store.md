# Orthant Store Specification Draft

## Abstract

An Orthant **store** holds line-based documents which exist in many versions along independent
**axes**, such as language, release and edition. Each axis is a tree of **values**; a **point**
takes one value from each axis; and every document has, at each point, a **content** — a list of
lines — or is absent there. The store does not hold each content in full: a point's content
cascades from the contents of the points above it, and only the lines which differ are stored.

This document specifies version 1 of the store: the files which make it up, the rules by which
the content of a document at a point is derived from them, and the operations which a program
reading a store provides. It is written so that a reader may be implemented from this document
alone, and so that two readers given the same store give the same answer to every query.

The design of the store is motivated by the following goals:

- **Plain text.** Every file is text which a person can read, and which a line-based version
  control system can store, compare and merge. The store is committed to git, and keeps no
  history of its own.
- **Records which stand alone.** A stored change is located by the lines it replaces and not by
  a line number, so that it survives changes made elsewhere in the document it is written
  against.
- **Derivation without judgement.** The content at a point is a function of the store. Where
  stored changes no longer fit the lines they were written against, or disagree, the rules still
  give one content, and report what wants attention beside it.
- **Reading without the tool.** A store is written only by the `orthant` tool, but may be read
  by anything. Reading needs no state outside the store, and changes nothing in it.

## 1. Status

This document is a draft specification of version 1 of the Orthant store format.

It specifies reading. How a store is changed — how an edit is checked in, how stored changes
follow the content beneath them, how an axis follows a git repository — is described in
[`design/engine.md`](../design/engine.md), and is not specified here (§17).

## 2. Conformance Language

The key words **MUST**, **MUST NOT**, **REQUIRED**, **SHALL**, **SHALL NOT**, **SHOULD**, **SHOULD
NOT**, **RECOMMENDED**, **MAY**, and **OPTIONAL** in this document are to be interpreted as
described in RFC 2119 and RFC 8174 when, and only when, they appear in all capitals.

Two things conform to this specification:

- A **store** is **valid** if none of the conditions which §13 lists as making a store invalid
  holds of it.
- A **reader** is a program which reads a store. A reader conforms if it provides the
  operations of §12.2 and, for every valid store, each operation it provides gives the result
  this document defines.

Requirements on what a store contains are requirements on whatever writes it. A reader MUST NOT
write to a store.

Sections marked *(Informative)* are not normative.

## 3. Overview

A store is a directory named `.orthant`. It holds a configuration, which names the axes and
their values, and a set of **records**, each of which is one file and says something about one
document at one point:

- a **base** record gives the document's whole content;
- a **delta** record gives a list of **hunks**, each of which replaces some consecutive lines
  of another point's content with other lines;
- an **absent** record says that the document does not exist.

Most points have no record for a document. The content of a document at a point is derived
(§9): from the content at the point's **storage parent**, which is one step nearer the root on
one axis; and from the hunks stored at the point and at certain points above it, its
**origins**. A hunk which is inherited from an origin was written against a different content,
and is carried onto the storage parent's content by **re-basing** (§9.6), which compares the
two contents with a prescribed difference algorithm (§10). A hunk whose lines have changed
beneath it is **stale**; it is still applied, and is reported.

A reader therefore does three things: it reads the configuration and the records (§5–§8); it
derives contents on demand (§9–§10); and it answers queries about them (§11–§12).

## 4. Terms

A **line** is a sequence of Unicode code points which contains no `U+000A` LINE FEED. Two lines
are **equal** if they are the same sequence of code points; no normalization is applied, and
`U+000D` CARRIAGE RETURN is a code point like any other. An **empty line** has no code points.

A **content** is a finite list of lines, possibly empty. Lines of a content are numbered from
0. A **span** `[s, e)` of a content, with `0 ≤ s ≤ e ≤` its length, is the lines numbered from
`s` up to but not including `e`; a span with `s = e` is **empty**, and names the gap before
line `s`.

An **axis** has a name, and a set of **values** which form a tree: each value has a name, and
each but one has a **parent**, which is another value of the axis. The value with no parent is
the axis's **root**. The **lineage** of a value is the value, its parent, its parent's parent,
and so on, ending with the root. The **depth** of a value is the number of values in its
lineage, less one. A value `v` is **below** a value `w` of the same axis if `w` is in the
lineage of `v`; a value is below itself.

The edge between a value and its parent is either a **translation** or a **variation**.

The axes of a store are ordered. The first axis has the greatest **precedence**.

A **point** is a choice of one value from each axis. The **root point** takes the root of every
axis. A point `p` is **below** a point `q` if, on every axis, `p`'s value is below `q`'s. Two
points are **comparable** if either is below the other.

Point `p` is **more specific** than point `q` if, on the first axis on which their values have
different depths, `p`'s value is the deeper. For any two different points whose values on each
axis are in a single lineage, one is more specific than the other.

The **storage axis** of a point other than the root point is the first axis on which its value
is not the root. Its **storage parent** is the point which differs from it only on that axis,
where it takes the parent of the point's value. The root point has neither.

A **document** is named by its **path**: one or more **components**, joined by `/`.

A **record** is what the store holds for one document at one point (§8). A document is **in
the store** if the store holds a record for it at any point.

## 5. Files and Encoding

A store is defined over a tree of directories and files. A reader MAY take that tree from a
filesystem, from a tree object of a git repository, or from anywhere else which presents
directories with named entries and files with bytes.

Every file of a store MUST be well-formed UTF-8, with no byte order mark. A file which a reader
cannot read, or which is not well-formed UTF-8, is **↯/o.4.5**.

The **lines of a file** are found by cutting its text at every `U+000A`; if the last piece is
empty, it is discarded. So each line of a file ends with a line feed; a file whose last line has
none has that line all the same; and a file of no bytes has no lines. A file of a store MUST end
with a line feed unless it is empty. No other code point ends a line: a `U+000D` before a line
feed is the last code point of its line.

The **text of a content** is each of its lines followed by `U+000A`, in order. It is how a
content is given as bytes.

An entry of a directory whose name begins with `.` is **hidden**. Within `.orthant/data/`
(§6), a reader MUST ignore hidden entries, at every depth, and MUST ignore entries which are
neither directories nor regular files. A store SHOULD NOT contain symbolic links.

## 6. Layout

### 6.1 Directory Structure

```
.orthant/config.tel                   the configuration (§7)
.orthant/data/<point>/<path>          a base record (§8.1)
.orthant/data/<point>/<path>.delta    a delta or absent record (§8.2)
.orthant/.gitattributes               `* -text`: git is not to alter line endings
.orthant/.gitignore                   `/work/`
.orthant/work/                        the state of a working tree (§15); not part of the store
```

The directory which contains `.orthant` is the store's **home**. A directory has a store if it
contains `.orthant/config.tel`.

`.orthant/data/` contains one directory for each point at which the store holds a record, and
MAY be missing if the store holds none. Each such directory is a **point directory**. A reader
MUST ignore an entry of `.orthant/data/` which is not a directory, MUST ignore `.orthant/work/`
when deriving content, and MUST ignore every other entry of `.orthant/` which this section does
not name.

### 6.2 Names

The name of an axis and the name of a value are given by this grammar:

```ebnf
axis-name   ::= initial , { letter | digit | "." | "+" | "-" } ;
value-name  ::= initial , { letter | digit | "." | "+" | "-" | "/" } ;
initial     ::= letter | digit ;
letter      ::= "A" | … | "Z" | "a" | … | "z" ;
digit       ::= "0" | … | "9" ;
```

Neither may be `root`. The names of the axes of a store MUST differ from one another, as MUST
the names of the values of one axis. Names are compared as sequences of code points: `FR` and
`fr` are different values.

### 6.3 Point Directories

The name of a point directory is either `root`, which names the root point, or one or more
**pairs** joined by `,`:

```ebnf
point-name  ::= "root" | pair , { "," , pair } ;
pair        ::= axis-name , "=" , folded-value ;
```

where `folded-value` is the name of a value with every `/` written as `_`. Each pair gives the
point's value on one axis; on every axis which no pair names, the point is at the root.

A point directory's name MUST name each axis at most once, MUST name only axes and values which
the configuration has, and MUST NOT give the root of an axis. A name which does otherwise does
not name a point (**↯/o.1.7**). It follows that the root point is named only by `root`.

The pairs SHOULD be in the order of the axes. A reader MUST read them as a set, in whatever
order they come: adding an axis to a store renames no directory.

> For axes `language` and `release`, in that order, the point at `fr/CA` and `v2` is held in
> `language=fr_CA,release=v2`; the point at `en`, the root of `language`, and `v2` is held in
> `release=v2`.

### 6.4 Record Files

Every file beneath a point directory, at any depth, is a record. Its **relative path** is the
names of the directories between the point directory and the file, and then the file's name,
joined by `/`.

- In the directory of a point other than the root point, a file whose relative path ends in
  `.delta` is a delta or absent record (§8.2) of the document whose path is the relative path
  without those six characters.
- Every other file is a base record (§8.1) of the document whose path is its relative path. In
  `root`, that is every file: there, a path ending in `.delta` is a document's path like any
  other.

So the path of a document which has a record anywhere but at the root point does not end in
`.delta`, and no component of a document's path begins with `.`.

The store MUST hold at most one record for a document at a point. A base record and a `.delta`
record of the same document in the same point directory, or records of one document in two
directories which name the same point, are **↯/o.1.5**.

## 7. Configuration

### 7.1 Structure

`.orthant/config.tel` is a [TEL](https://github.com/propensive/tel) document. A store whose
configuration is missing is no store (**↯/o.4.1**); one whose configuration is not a TEL
document, or does not have the structure below, cannot be read (**↯/o.5.1**).

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
axis
  name release
  kind custom
  prefer own
  value
    name v1
  value
    name v2
    parent v1
```

The document has these members, in which *once* means exactly one occurrence, *optional* at
most one, and *any* any number:

| Keyword | Within | Occurs | Value |
|---|---|---|---|
| `format` | the document | once | the version of this format: `1` |
| `tree` | the document | once | the directory of the working tree, relative to the store's home (§15) |
| `axis` | the document | any | an axis; the order of occurrence is the order of the axes |
| `name` | `axis` | once | an `axis-name` (§6.2) |
| `kind` | `axis` | once | `language`, `custom` or `git` |
| `prefer` | `axis` | once | `own` or `parent` (§9.5) |
| `pattern` | `axis` | optional | on an axis of kind `git`, the tags it follows (§7.3) |
| `value` | `axis` | any | a value of the axis |
| `name` | `value` | once | a `value-name` (§6.2) |
| `parent` | `value` | optional | the name of the value's parent; absent for the root |
| `translation` | `value` | optional | `true` or `false`; if `true`, the edge to the parent is a translation |
| `label` | `value` | optional | names by which the value is also known, separated by spaces (§7.3) |
| `commit` | `value` | optional | a commit's hash (§7.3) |
| `branch` | `value` | optional | `true` or `false` (§7.3) |

Version 1 defines no other member. A reader MUST refuse a store whose `format` is not `1`
(**↯/o.5.1**). A reader SHOULD ignore a member which this table does not have: one may be added
without a new version, provided that it has no bearing on content.

### 7.2 Axes and Values

A store has no axes, or any number. With none, it has one point, the root point.

The values of each axis MUST form a tree:

- Exactly one value has no `parent` (**↯/o.2.2** otherwise); it is the axis's root.
- Every `parent` names a value of the same axis (**↯/o.2.1**).
- No value is in the lineage of its own parent (**↯/o.2.3**).

A value may come before its parent in the configuration.

An edge is a translation if and only if the value beneath it has `translation true`. A reader
MUST NOT infer it from the names of the values or from the axis's `kind`.

An axis's `kind` says how its values come about, and has no bearing on what the store holds. A
reader MUST give it when asked (§12.2) and MUST NOT otherwise depend on it.

### 7.3 Axes Which Follow a Repository *(Informative in part)*

An axis of kind `git` takes its values from the tags and branches of the git repository which
the store is in. This section's keywords tell a reader what each value stands for; they do not
affect content.

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

- `commit` is the full hash of the commit which the value stands for. The value's `name` is the
  first twelve hexadecimal digits of it, so that a point directory is named by the commit, and
  not by a tag which may move.
- `label` is the tags which name that commit. Normatively: the **labels** of a value are the
  pieces of `label`, cut at every `U+0020`, which are not empty; a value with no `label` has
  none. The **title** of a value is its first label or, if it has none, its name.
- `branch true` marks the value which holds what is not yet released on the branch of the
  value's name.
- `pattern` is the tags which the axis takes as values; `*` stands for any run of characters,
  and an axis with no pattern takes every tag.

Labels are not names: they do not appear in point directories, and are used only to resolve a
term (§12.2) and to show a value.

### 7.4 Written Form *(Informative)*

The tool writes the configuration with no pragma, comment, tabulation or literal atom, and with
two spaces of indentation for each level. In that form every line is some number of pairs of
spaces, a keyword, and then either nothing, for `axis` and `value`, or one space and a value
which runs to the end of the line.

A reader SHOULD read the configuration with a conforming TEL parser. One which reads only this
form MUST refuse whatever it does not recognise (**↯/o.5.1**), and MUST NOT guess.

## 8. Records

### 8.1 Base Records

A base record is the document's content, verbatim: the lines of the file (§5) are the lines of
the content. Nothing in it is a header, a sigil or an escape.

### 8.2 Delta Files

A `.delta` file holds a **delta record** or an **absent record**. Its first line is its
**header**:

```ebnf
header  ::= "orthant" , " " , version , " " , axis-name , [ " " , "absent" ] ;
```

Cut at every `U+0020`, the header MUST have three pieces, or four of which the last is
`absent`, and the first MUST be `orthant` (**↯/o.1.1**). A file with no lines has no header
(**↯/o.1.1**). The second piece MUST be `1`; otherwise the file is of a version this
specification does not define (**↯/o.1.4**). The third MUST be the name of the storage axis of
the point whose directory the file is in (**↯/o.1.6**): a change to the order of the axes,
which would change the meaning of every delta, cannot then go unnoticed.

With `absent`, the record is an absent record. Its header MUST be its only line; a reader
ignores any which follow.

Without it, the record is a delta record, and the lines after the header are its hunks. They
are read in order:

- An empty line ends the hunk in progress, if there is one.
- Any other line is a line of the hunk in progress; if there is none, it begins one.

A valid store has exactly one empty line before each hunk, and none elsewhere. A reader MUST
accept more, and MUST accept a hunk which follows the header directly. A delta record MAY have
no hunks.

### 8.3 Hunks

The first code point of each line of a hunk is its **sigil**. A line which is the sigil alone
has the empty line as its **text**. Otherwise the second code point MUST be `U+0020`, and the
text is whatever follows it, which may be empty. A line which begins with no sigil, or whose
sigil is followed by anything but a space, is **↯/o.1.2**; so a line which consists of spaces,
or a conflict marker left by a merge, is never read as content.

| Sigil | Its line is | Meaning |
|---|---|---|
| `^` | the start mark | the anchor begins at the start of the content; the sigil stands alone |
| `=` | a context line | a line of the storage parent's content which the hunk leaves as it is |
| `!` | a former line | a line which the hunk replaced before the storage parent's content changed |
| `-` | a replaced line | a line of the storage parent's content which the hunk replaces |
| `$` | the end mark | the anchor ends at the end of the content; the sigil stands alone |
| `+` | a new line | a line which the hunk puts in place of the replaced lines |

The lines of a hunk MUST come in this order: the start mark; **leading** context lines; former
lines; replaced lines; **trailing** context lines; the end mark; new lines. Any of these may be
missing. A context line is leading if no former or replaced line comes before it in the hunk,
and trailing otherwise. A line which comes after one that should follow it is **↯/o.1.3**,
except a context line after the end mark or a new line, which is **↯/o.1.2**. A `^` or `$`
followed by any other code point is **↯/o.1.2**.

A hunk therefore has:

| Part | |
|---|---|
| `start` | whether it has a start mark |
| `before` | the texts of its leading context lines |
| `former` | the texts of its former lines |
| `replaced` | the texts of its replaced lines |
| `after` | the texts of its trailing context lines |
| `end` | whether it has an end mark |
| `lines` | the texts of its new lines: its **own lines** |

The **anchor** of a hunk is `before`, then `replaced`, then `after`, as one run of lines,
together with `start` and `end`. A hunk whose `replaced` is empty is an **insertion**. A hunk
whose `former` is not empty is **stale as stored**.

In a valid store:

- Every hunk's anchor occurs exactly once (§9.4) in the content of the storage parent of the
  point it is stored at, if the document has a content there (**↯/o.1.8**).
- No two hunks of one delta record are placed (§9.5) on spans which overlap (§9.7).

These are properties of the whole store and not of one file. A reader finds the first while
deriving content, and MUST report it (§9.9, §12.2).

*(Informative.)* The tool writes the hunks of a record in the order of their anchors; the
fewest leading context lines with which the anchor occurs once, and a start mark where every
line before the hunk is not enough; and no trailing context and no end mark. A hunk with
neither replaced nor new lines stops an insertion which the point would otherwise inherit at
that place. A hunk whose new lines equal its replaced lines is written only on a translation
edge, where it says that those lines are their own translation (§11).

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

## 9. Content

This section defines, for a document `d` and a point `p` of a valid store, the **rendition** of
`d` at `p`, which is either **absent** or has:

- the **content** of `d` at `p`;
- a **reference**: the content from which that content was derived, or nothing;
- **placements**: the hunks which were applied to the reference, each with the span it replaces
  (§9.5);
- **issues**: what wants attention (§9.9).

The definition is recursive. The rendition at `p` depends on the content at `p`'s storage
parent and at the storage parents of `p`'s origins, each of which is nearer the root point than
`p` is; so the recursion ends. A reader SHOULD keep each rendition it derives, since each is
needed many times over.

### 9.1 The Root Point

At the root point, if the store holds a base record for `d`, the content is that record's, the
reference is empty, and there are no placements and no issues. Otherwise the rendition is
absent.

### 9.2 Origins

Let `p` be a point other than the root point, with storage axis `a` and storage parent `q`.

The **origins** of `p` are the points which

- have `p`'s value on `a`;
- are at the root on every axis before `a`, as `p` is; and
- on every axis after `a`, have a value in the lineage of `p`'s value.

`p` is one of its own origins, and is more specific than every other. The origins are taken
from the **most specific** (§4) to the least, which is a total order of them.

> With axes `language`, `release`, where `v3` is beneath `v2` beneath the root `v1`, the origins
> of (`fr`, `v3`) are (`fr`, `v3`), (`fr`, `v2`) and (`fr`, `v1`), in that order. The origins
> of (`en`, `v3`), whose storage axis is `release`, are itself alone.

### 9.3 Which Records Count

Take the origins of `p` in order, most specific first, keeping a list `Δ` of origins, which is
empty to begin with. At each origin `o`:

1. If the store holds no record for `d` at `o`, go on to the next origin.
2. If it holds a delta record, add `o` to the end of `Δ`, and go on to the next origin.
3. If it holds a base record and `Δ` is empty, the rendition has that record's content, an
   empty reference, no placements and no issues. Stop.
4. If it holds an absent record and `Δ` is empty, the rendition is absent. Stop.
5. Otherwise — a base or an absent record, and `Δ` is not empty — take no further origins.

If that did not stop, let `R` be the content of `d` at `q`. If `d` is absent at `q`, the
rendition at `p` is absent. Otherwise `R` is the rendition's reference, and its content,
placements and issues are given by §9.4–§9.9 from `R` and the delta records at the origins in
`Δ`.

So a base or absent record decides the matter for its own point and for every point which has
it as an origin, until a delta record at a more specific origin overrules it; and a document
which is absent at a point's storage parent is absent at the point, unless a base record says
otherwise.

### 9.4 Locating

A hunk **occurs at** line `i` of a content `C`, where `n` is the number of lines of its anchor
(`before`, `replaced`, `after`), if

- `0 ≤ i` and `i + n ≤` the length of `C`;
- the lines of `C` in `[i, i + n)` are equal, one by one, to the lines of the anchor;
- if the hunk has `start`, then `i = 0`; and
- if the hunk has `end`, then `i + n` is the length of `C`.

A hunk which occurs at exactly one line `i` of `C` is **located** in `C`, at the **position**
`i +` the length of `before`: the line at which its replaced lines begin. A hunk which occurs
nowhere in `C` is an **orphan** there; one which occurs at more than one line is **ambiguous**
there. Neither is located, and neither contributes to any content.

An anchor of no lines, with neither mark, occurs at every `i` from 0 to the length of `C`: it
is located only in a content of no lines.

### 9.5 Placing

A **placement** is a hunk in place on the reference `R`. It has:

| Part | |
|---|---|
| `span` | the span `[start, end)` of `R` which it replaces; empty for an insertion |
| `origin` | the origin whose delta record it came from |
| `own` | the lines which the hunk gives |
| `former` | the lines it was written against, where they have since changed; else none |
| `fresh` | false if the placement is **stale**: the lines it was written against have changed |
| `shown` | the lines which take the place of the span in the content |

For each origin `o` in `Δ`, in order, let `S` be `R` if `o` is `p`, and otherwise the content
of `d` at the storage parent of `o`. If `d` is absent there, `o` contributes nothing. Otherwise
each hunk of the delta record at `o`, in the order of the file, is located in `S` (§9.4); those
which are not located contribute no placement. Then:

- If `o` is `p`, each located hunk, at position `s`, is a placement with span
  `[s, s + |replaced|)`, with the hunk's `lines` as `own` and its `former` as `former`; it is
  fresh unless the hunk is stale as stored.
- Otherwise the located hunks are re-based from `S` onto `R` (§9.6), and each outcome which
  that gives is a placement, with the outcome's span, `own`, `former` and freshness.

In either case `origin` is `o`. The `shown` lines of a placement are:

- its `own` lines, if it is fresh, or if `prefer` is `own` on `a`, the storage axis of `p`;
- the lines of `R` in its span, if it is stale and `prefer` is `parent` on `a`.

So a stale hunk on an axis which prefers the parent changes nothing in the content, and is
still reported.

The placements so made, in the order they were made — origin by origin, and within an origin
in the order of the file or the order which §9.6 gives — are the **candidates**.

### 9.6 Re-basing

Re-basing carries hunks which are located in a content `S`, the **old** content, onto a content
`R`, the **new** one. Its input is the located hunks, each with its position, in the order of
the file. Its output is a list of **outcomes**, each with a span of `R`, `own` lines, `former`
lines, and whether it is stale.

Let the **regions** by which `S` becomes `R` be as §10 gives them. Each region has a span
`[from, to)` of `S` and a span `[from₂, to₂)` of `R`, and is **unchanged**, in which case the
two spans hold equal lines, or **changed**. A changed region whose span of `R` is empty is a
**pure deletion**. For a line or gap `i` of `S` within an unchanged region `U`, let
`carry(i, U) = U.from₂ + (i − U.from)`.

The **rank** of a hunk at position `s` is `2s`, if it is an insertion, and `2s + 1` otherwise:
hunks in the order of their ranks are in the order of their positions, an insertion coming
before a replacement which begins where it is.

**Step 1: each hunk alone.** For a hunk `h` at position `s`, let `e = s + |replaced|`,
`a₀ = s − |before|` and `a₁ = e + |after|`, so that its anchor is the span `[a₀, a₁)` of
`S`. Where a rule below speaks of *the* unchanged region which satisfies a condition, it means the
first such region. Take the first of these rules which applies:

1. **The anchor is untouched.** If an unchanged region `U` has `U.from ≤ a₀` and
   `a₁ ≤ U.to`, then `h` is **carried** to `carry(s, U)`.
2. **An insertion beside an unchanged line.** If `h` is an insertion, and an unchanged region
   `U` has `U.from < s ≤ U.to`, or failing that an unchanged region `U` has
   `U.from ≤ s < U.to`, then `h` is carried to `carry(s, U)`.
3. **An insertion at an end.** If `h` is an insertion and `s = 0`, then `h` is carried to 0. If
   `h` is an insertion and `s` is the length of `S`, then `h` is carried to the length of `R`.
4. **An insertion within a change.** If `h` is an insertion, then `h` is **disturbed**, and
   **touches** every region with `from ≤ s ≤ to`.
5. **Replaced lines untouched, context not.** If an unchanged region `U` has `U.from ≤ s` and
   `e ≤ U.to`, then: if the lines `replaced` occur as consecutive lines exactly once in `S` and
   exactly once in `R`, `h` is carried to `carry(s, U)`; and otherwise `h` is disturbed, with
   the hull `[s, e)` of `S` and `[carry(s, U), carry(e, U))` of `R`.
6. **Replaced lines changed.** Otherwise `h` touches every region with `from < e` and
   `s < to`, and every region with `from = to` and `s < from < e`. If every region it touches
   is a pure deletion, `h` is **lost**: it has no outcome. Otherwise `h` is disturbed.

A hunk carried to `c` has the outcome with span `[c, c + |replaced|)`, the hunk's `lines` as
`own`, and the hunk's `former` as `former`; it is stale if and only if the hunk is stale as
stored.

A hunk which is disturbed and touches regions, of which `F` is the first and `L` the last, has
the **hull** whose span of `S` is `[from, to)` and whose span of `R` is `[from₂, to₂)`, where:

- `from` is `F.from` and `from₂` is `F.from₂`, if `F` is changed; otherwise `from` is `s` and
  `from₂` is `carry(s, F)`;
- `to` is `L.to` and `to₂` is `L.to₂`, if `L` is changed; otherwise `to` is `e` and `to₂` is
  `carry(e, L)`.

So the hull of a disturbed hunk reaches to the far side of every changed region it touches.

**Step 2: disturbed hunks together.** Take the hulls in the order of their `from`, those with
the same `from` in the order of the file, and form them into **groups**: a hull joins the group
before it if its `from` is less than the greatest `to` in that group, or if its span of `S` is
empty and is that group's whole span; otherwise it begins a new group. For each group, let
`[from, to)` reach from the least `from` to the greatest `to` of its hulls, and `[from₂, to₂)`
from the least `from₂` to the greatest `to₂`. The group has one outcome:

- Its span is `[from₂, to₂)`.
- Its `own` lines are the lines of `S` in `[from, to)`, in which each hunk of the group has
  taken effect: taking the hunks in the order of their ranks, the lines of `S` from `from` to
  the first hunk's position, then that hunk's `lines`; then the lines of `S` from the end of
  that hunk's replaced lines to the next hunk's position, then that hunk's `lines`; and so on,
  ending with the lines of `S` from the end of the last hunk's replaced lines to `to`.
- Let `F` be the `former` lines of the group's one hunk, if the group has one hunk and it is
  stale as stored; and otherwise the lines of `S` in `[from, to)`. If `F` is empty, or is equal
  to the lines of `R` in `[from₂, to₂)`, the outcome is not stale, and has no `former` lines: a
  hunk which finds again the very lines it was written against is stale no longer. Otherwise
  the outcome is stale, and `F` is its `former` lines.

The rank of a group's outcome is the least of the ranks of its hunks.

**Step 3: insertions which meet.** List the outcomes of the carried hunks, in the order of the
file, and then the outcomes of the groups, in order. Outcomes in that list whose spans are the
same empty span have come to insert at one place. Wherever there are two or more such, they are
replaced by one outcome with that span, whose `own` lines are theirs, one after another in the
order of their ranks — those of equal rank in the order of the list — and whose `former` lines
are theirs likewise; it is stale if those `former` lines are not empty.

**The output** is the outcomes of that list whose spans are not empty, in the order of the
list; and then the outcomes whose spans are empty, in the order in which their spans first come
in the list.

### 9.7 Resolution

Two placements **overlap** if

- both spans are empty, and are the same; or
- one span `[s, s)` is empty, the other `[t, u)` is not, and `t < s < u`; or
- neither span is empty, and they have a line in common.

An insertion at either end of the lines which another placement replaces does not overlap it.

Order the candidates by these keys, in turn:

1. fresh placements before stale ones;
2. the placement whose `origin` is more specific (§4) first;
3. the lesser `start` first;
4. the earlier candidate (§9.5) first.

Take them in that order. A candidate which overlaps no placement yet accepted is **accepted**.
A candidate which overlaps one is **rejected**. A rejected candidate is in **conflict** with
its **rival** — of the accepted placements it overlaps, the one accepted last — if their
origins are not comparable (§4), they are both fresh or both stale, and their `shown` lines are
not equal.

### 9.8 Rendering

The accepted placements, in the order of their `start`, an empty span before another which
begins where it is, are the rendition's **placements**. No two overlap.

The **content** is the lines of `R` in which the span of each placement has given way to its
`shown` lines: the lines of `R` before the first placement's span, the first placement's
`shown` lines, the lines of `R` between its span and the next, and so on, ending with the lines
of `R` after the last placement's span.

Each placement also has a **place** `[from, to)` in the content: the span which its `shown`
lines occupy there.

### 9.9 Issues

The issues of the rendition are, in order:

1. for each hunk of the delta record at `p` itself, in the order of the file, which is not
   located in `R`: that it is an **orphan**, or **ambiguous** (§9.4);
2. for each placement which is not fresh, in order: that it is **stale**;
3. for each conflict, in the order they were found: the **conflict**, with the placement which
   is shown and the candidate which is not.

A hunk of another origin which is not located is an issue at that origin, and not at `p`.

A rendition which §9.1 or §9.3 gives outright, from a base record, has no issues.

In a valid store no rendition has an orphan or an ambiguous hunk (**↯/o.1.8**). Stale
placements and conflicts are not errors: they are the ordinary condition of a store in which
one version has moved and another has yet to follow.

## 10. Regions

The regions by which a content `A`, of `N` lines, becomes a content `B`, of `M` lines, are
given by the shortest edit script which the greedy algorithm of Myers finds¹, with the choices
it leaves open made as below. A reader MUST find these regions and no others: two shortest
scripts for the same contents may give different regions, and so different content.

¹ E. W. Myers, "An O(ND) Difference Algorithm and Its Variations", *Algorithmica* 1 (1986).

Lines are compared for equality as §4 defines. `V` is an array of integers indexed by
**diagonal** `k`, the number of deletions less the number of insertions; `V[k]` is the furthest
line of `A` reached on that diagonal.

```
V[1] ← 0
for D from 0 upward:
    saved[D] ← a copy of V
    for k from −D to D in steps of 2:
        if k = −D or (k ≠ D and V[k − 1] < V[k + 1]):
            x ← V[k + 1]                      -- an insertion, from diagonal k + 1
        else:
            x ← V[k − 1] + 1                  -- a deletion, from diagonal k − 1
        y ← x − k
        while x < N and y < M and A[x] = B[y]:
            x ← x + 1;  y ← y + 1             -- a match
        V[k] ← x
        if x = N and y = M: go to the second part, with this D
```

The script is then read backwards from the end of both contents:

```
x ← N;  y ← M;  script ← empty
for d from D down to 0:
    V ← saved[d];  k ← x − y
    if d = 0:
        put x matches at the front of script
    else if k = −d or (k ≠ d and V[k − 1] < V[k + 1]):
        put (x − V[k + 1]) matches at the front of script, and an insertion before them
        x ← V[k + 1];  y ← x − (k + 1)
    else:
        put (x − V[k − 1] − 1) matches at the front of script, and a deletion before them
        x ← V[k − 1];  y ← x − (k − 1)
```

So where a deletion and an insertion reach equally far, the insertion is taken.

The script is a list of matches, deletions and insertions. Each maximal run of matches is an
**unchanged** region, and each maximal run of deletions and insertions, in whatever order they
come, is a **changed** region. A region's span of `A` has one line for each match or deletion
in its run, and its span of `B` one line for each match or insertion; the regions' spans follow
one another in both contents, and cover both. Unchanged and changed regions alternate. Two
contents of no lines have no regions.

> From `a b c d` to `a x c d` there are three regions: unchanged `[0, 1)` and `[0, 1)`; changed
> `[1, 2)` and `[1, 2)`; unchanged `[2, 4)` and `[2, 4)`.

## 11. Work

A point is **a translation** if it is not the root point and the edge between its value on its
storage axis and that value's parent is a translation (§7.2).

The **pending lines** of a document at a point are the lines of its storage parent's content
which have yet to be translated. If the point is not a translation, or the rendition is absent,
there are none. Otherwise they are the lines of the rendition's reference which are not empty,
and which are in the span of no placement. A line which consists of spaces is not empty.

A rendition given outright by a base record has no reference, and so no pending lines.

The **work** in a document at a point is:

| Part | |
|---|---|
| `total` | at a translation, the number of lines of the reference which are not empty; elsewhere 0 |
| `pending` | the pending lines |
| `stale` | the placements which are stale |
| `faults` | the number of issues which are not stale placements: conflicts, orphans, ambiguous hunks |

A document **wants attention** at a point if it has a pending line, a stale placement or a
fault there. The work of an absent rendition is nothing. The work **at a point** is the sum,
over every document in the store, of `total`, of the number of pending lines, of the number of
stale placements and of `faults`.

## 12. Reading a Store

This section defines the operations of a reader. They are given as functions with names,
arguments and results; a reader MAY name and shape them as suits its language, provided that
each can be asked for and gives the result defined here. *Fails* means that the operation gives
no result, and reports the error code named.

### 12.1 General Requirements

- **A reader does not write.** No operation changes anything beneath `.orthant/`. A reader MUST
  NOT take the lock of §15, and MUST NOT create `.orthant/work/`.
- **Results are functions of the store.** Every operation's result depends only on the files of
  the store, and — for `checked-out`, `find` and `export` alone — on what else its definition
  names. A reader MUST NOT consult the working tree, the repository's history or the
  environment to answer any other.
- **One snapshot.** The tool replaces each file whole, but a change to the store may replace
  several. A reader SHOULD answer every query of one session from one state of the store, and
  SHOULD read from a git commit where it can. A reader which reads a filesystem while the store
  is being written MAY find it invalid, and SHOULD then read it again.
- **Errors are not content.** A reader MUST NOT give a content which this specification does
  not define. Where a store is invalid, the reader fails, or says what it left out (§12.2,
  `faults`).
- **Lines are exact.** A reader MUST give lines as they are stored: it MUST NOT normalize
  Unicode, change line endings, strip a `U+000D` or trim spaces.

### 12.2 Required Operations

A reader MUST provide each of these.

**`open(home)` → a store.** Reads the configuration and finds the records of the store whose
home is given. It fails if there is no store there (**↯/o.4.1**), and if the store is invalid in
any way which can be found without deriving content: every condition which makes a store
invalid (§13) but **↯/o.1.8**. It MAY read the records' files then or when
they are first needed, provided that an operation which needs a malformed record fails as
`open` would have.

**`format(store)` → an integer.** The store's `format`.

**`axes(store)` → a list of axes.** The axes, in order. For each: its name, its `kind`, its
`prefer`, its `pattern` if it has one, its root, and its values in the order of the
configuration. For each value: its name; its parent, if it has one; whether its edge is a
translation; its labels and its title (§7.3); its `commit`, if it has one; and whether it has
`branch true`. A reader MUST also give, for a value, its lineage, its depth and its children,
or the means to find them.

**`points(store)` → a list of points.** Every point: each choice of one value from each axis.
A point comes after every other point which it is below. The RECOMMENDED order is by the sum of
the depths of a point's values, and among points with the same sum, with the last axis's values
varying fastest, in the order of the configuration. A reader MAY give the points lazily; a
store has as many as the product of the numbers of values of its axes.

**`point(store, from, terms)` → a point.** The point which `from` becomes when it is moved to
each of the values which the terms name, in turn; `from` is the root point unless another is
given. A **term** is:

- `axis=value`, which names a value of the axis named. The value is named by its name or, if no
  value of that axis has that name, by one of its labels. It fails if the store has no such
  axis (**↯/o.2.8**) or the axis no such value (**↯/o.2.1**).
- `value`, with no `=`, which names a value of whichever axis has one so named, by name or
  failing that by label. It fails if no axis has one (**↯/o.2.8**), or more than one axis has
  (**↯/o.2.7**).

A reader MUST also let a point be given as a value for each axis, with no terms to resolve.

**`documents(store)` → a list of paths.** The path of every document in the store (§4), each
once, in the order of their code points. A document is listed though it be absent at every
point.

**`content(store, document, point)` → a content, or absent.** The content of the document at
the point, as §9 defines it. A document which is not in the store is absent at every point;
that is no error. In a store which is invalid, a reader which has not already failed MUST NOT
give a content derived from a rendition which has an orphan or an ambiguous hunk among its
issues (**↯/o.1.8**), unless it gives those issues with it (`rendition`, §12.3).

**`text(store, document, point)` → bytes, or absent.** The text of that content (§5), in
UTF-8.

**`faults(store)` → a list of errors.** Everything which makes the store invalid (§13), each
with the file, document or point it concerns. For **↯/o.1.8** it derives the rendition of
every document at every point, and gives an error for each point and document at which a hunk
of the point's own delta record is an orphan or ambiguous. The store is valid if the list is empty. Where a record
cannot be read, a reader MAY go on as if the store did not hold it, to find what else is wrong;
it MUST NOT give content from a store so read without saying so.

### 12.3 Recommended Operations

A reader SHOULD provide each of these.

**`find(directory)` → a home.** The nearest directory, at or above the one given, which has a
store. It fails if none has (**↯/o.4.1**).

**`rendition(store, document, point)` → a rendition, or absent.** All that §9 defines: the
content; the reference; the placements, in order, each with its span `[start, end)` of the
reference, its place `[from, to)` in the content, its origin, whether it is fresh, its `shown`,
`own` and `former` lines; and the issues, each with its kind and the hunk or placements it
concerns. From it a caller can say where every line of a content came from: a line in a
placement's place came from the delta record at its origin, and every other line from the
storage parent's content.

**`storage-parent(store, point)` → a point, or none**, and **`origins(store, point)` → a list
of points**, as §4 and §9.2 define them.

**`work(store, document, point)` → work**, and **`work(store, point)` → a sum**, as §11 defines
them.

**`export(store, point, directory, strict)` → a count.** Writes the text of each document which
has a content at the point to the file of its path beneath the directory given, which is
outside the store, and gives their number. If `strict` is asked for and any document in the
store wants attention at the point (§11), it writes nothing, and fails (**↯/o.4.11**).

### 12.4 Optional Operations

A reader MAY provide each of these.

**`view(store, document, point)` → a list of lines, or absent.** The document beside its
reference: the reference, with the placements in place, every line marked with a sigil. A
**marked** line is the sigil alone, if the line is empty, and otherwise the sigil, a space, and
the line.

- If the rendition has no reference lines and no placements, the view is each line of the
  content, marked `+`.
- Otherwise it is, for each placement in order: the lines of the reference between the last
  placement's span and this one's, as **shared** lines; the placement's `former` lines, marked
  `!`, if it is stale; the lines of the reference in its span, marked `-`; and its `own` lines,
  marked `+`. Then the remaining lines of the reference, as shared lines.
- A shared line which is empty is an empty line. One which is not is marked `?`, if the point
  is a translation (§11), and otherwise is two spaces and then the line.

A stale placement's `own` lines are given whatever its axis prefers. The view is absent where
the rendition is.

**`own-work(store, document, point)` → work.** The work which is best done at this point: the
work of §11, less each pending line which is equal to a pending line of the document at one of
the point's other origins, and less each stale placement which has the same origin and the same
`own` lines as a stale placement of the document's rendition at one of them.

**`checked-out(store)` → a point.** The point which the working tree shows (§15), which is not
part of the store. A reader which provides it MUST give the root point where there is no
`view.tel`.

## 13. Errors

Every error is named by a **bolt code** whose letter is `o`. This table gives the codes which
this specification defines; the tool has others, for operations which are not specified here.

| Code | Condition | Defined in |
|---|---|---|
| **↯/o.1.1** | a delta file's header is missing or malformed | §8.2 |
| **↯/o.1.2** | a line of a hunk has no sigil, or a sigil which is not followed by a space | §8.3 |
| **↯/o.1.3** | the lines of a hunk are out of order | §8.3 |
| **↯/o.1.4** | a delta file is of a version other than 1 | §8.2 |
| **↯/o.1.5** | the store holds more than one record for a document at a point | §6.4 |
| **↯/o.1.6** | a delta file's header names an axis which is not its point's storage axis | §8.2 |
| **↯/o.1.7** | a directory of `.orthant/data/` does not name a point | §6.3 |
| **↯/o.1.8** | a stored hunk is not located in its storage parent's content | §8.3, §9.4 |
| **↯/o.2.1** | a value is not on the axis | §7.2, §12.2 |
| **↯/o.2.2** | an axis has no root, or more than one | §7.2 |
| **↯/o.2.3** | a value's parents form a cycle | §7.2 |
| **↯/o.2.7** | a value given alone is on more than one axis | §12.2 |
| **↯/o.2.8** | no axis has the value, or there is no such axis | §12.2 |
| **↯/o.4.1** | there is no store | §7.1, §12 |
| **↯/o.4.5** | a file could not be read, or is not UTF-8 | §5 |
| **↯/o.4.11** | a strict export found a document which wants attention | §12.3 |
| **↯/o.5.1** | the configuration could not be read, or is of a format other than 1 | §7 |

A store is **invalid** if any of **↯/o.1.1**–**↯/o.1.8**, **↯/o.2.1**–**↯/o.2.3** or
**↯/o.5.1** holds of it, or if a file of it is **↯/o.4.5**. The conditions **↯/o.2.7**,
**↯/o.2.8**, **↯/o.4.1** and **↯/o.4.11**, and **↯/o.2.1** where a term names a value which
its axis does not have, are failures of an operation, and say nothing about the store.

A reader SHOULD report an error with its code, and with the file and line, or the document and
point, which it concerns.

## 14. Example

A store has the configuration of §7.1, and three records of one document, `guide/intro.md`.

`.orthant/data/root/guide/intro.md`, a base record at (`en`, `v1`):

```
Title

Install the tool.
Run it.

Last line.
```

`.orthant/data/release=v2/guide/intro.md.delta`, a delta record at (`en`, `v2`):

```
orthant 1 release

- Run it.
+ Run it from a terminal.
+ New para.
```

`.orthant/data/language=fr/guide/intro.md.delta`, a delta record at (`fr`, `v1`):

```
orthant 1 language

- Title
+ Titre

- Install the tool.
- Run it.
+ Installez l'outil.
+ Lancez-le.
```

**At (`en`, `v2`).** The storage axis is `release`, the storage parent is (`en`, `v1`), and the
point is its own only origin. Its hunk is located at position 3 of the root's content, and is a
fresh placement on `[3, 4)`. The content is:

```
Title

Install the tool.
Run it from a terminal.
New para.

Last line.
```

**At (`fr`, `v1`).** The storage axis is `language` and the storage parent is (`en`, `v1`). The
two hunks are located at positions 0 and 2, and are fresh placements on `[0, 1)` and `[2, 4)`.
The content is `Titre`, an empty line, `Installez l'outil.`, `Lancez-le.`, an empty line and
`Last line.`. The point is a translation; of the four lines of its reference which are not
empty, `Last line.` is in no placement's span, and is pending.

**At (`fr`, `v2`).** The storage parent is (`en`, `v2`), whose content, above, is the reference
`R`. The origins are (`fr`, `v2`), which has no record, and (`fr`, `v1`), which has a delta
record; so `Δ` is (`fr`, `v1`) alone. Its hunks are located in `S`, the content at its own
storage parent (`en`, `v1`), at positions 0 and 2, and are re-based from `S` onto `R`. The
regions are: unchanged, `[0, 3)` of `S` and `[0, 3)` of `R`; changed, `[3, 4)` and `[3, 5)`;
unchanged, `[4, 6)` and `[5, 7)`.

- The first hunk's anchor, `[0, 1)`, is within the first region. It is carried to 0 (rule 1).
- The second hunk's replaced lines, `[2, 4)`, lie across the first and second regions; it
  touches both (rule 6), and is disturbed. Its hull begins at its own position, 2, since the
  first region it touches is unchanged, and ends where the changed region does: `[2, 4)` of `S`
  and `[2, 5)` of `R`. It is alone in its group. Its `own` lines are its two new lines; its
  `former` lines are the lines of `S` in `[2, 4)`, which are not the lines of `R` in `[2, 5)`;
  so it is stale.

The placements are a fresh one on `[0, 1)` and a stale one on `[2, 5)`. `language` prefers
`own`, so the stale placement shows its own lines, and the content is that of (`fr`, `v1`). Its
issues are one stale placement. Its work is: `total` 5, one pending line (`Last line.`), one
stale placement, no faults. Its view (§12.4) is:

```
- Title
+ Titre

! Install the tool.
! Run it.
- Install the tool.
- Run it from a terminal.
- New para.
+ Installez l'outil.
+ Lancez-le.

? Last line.
```

Had `language` preferred `parent`, the stale placement would have shown lines 2 to 4 of `R`,
and the content would have been `Titre` followed by the last six lines of the content at
(`en`, `v2`).

## 15. The Working Tree *(Informative)*

The tool checks one point out into a **working tree**, the directory which the configuration's
`tree` names, relative to the store's home. Neither it nor `.orthant/work/` is part of the
store, and neither is committed. `.orthant/work/` holds:

```
view.tel            the point which is checked out, and the mode of the view
pristine/<path>     each document as it was checked out
lock                held while the tool changes the store or the working tree
staging             a file being written, before it is moved into place
```

`view.tel` is a TEL document with a `mode`, which is `direct`, `indirect` or `change`, and one
`coordinate` for each axis on which the point is not at the root, with an `axis` and a `value`:

```
mode direct
coordinate
  axis release
  value v2
```

Without the file, the root point is checked out. In the mode `change` it also has `from`, as
`axis=value`: the earlier value from which a change to the reference is shown.

The tool writes each file of the store to `staging` and moves it into place, so that no reader
sees part of a file.

## 16. Later Versions

A change to this format which a reader of version 1 would misread is a new version. The
configuration's `format` gives the version of the store, and each delta file's header gives the
version of that file; a reader MUST refuse either if it is of a version the reader does not
know (**↯/o.5.1**, **↯/o.1.4**), and MUST NOT read what it can of it.

A later version may give a hunk's anchor as a position, a count and a hash of the replaced
lines instead of their text. A reader SHOULD therefore keep locating (§9.4) apart from what
follows it.

## 17. Completeness of this Specification

This specification is complete for reading version 1 of the store: the layout (§6), the
configuration (§7), the records (§8), the content of every document at every point with its
placements and issues (§9, §10), the work which remains (§11) and the operations of a reader
(§12). Its error codes are tabulated once, in §13, and each is referenced where its condition
is defined.

It does not specify:

- how a store is changed: check-in and its scopes, the storing of hunks, confirming a stale
  hunk, and the invariants which a change keeps;
- the views of a working tree other than the one of §12.4, and how an edited view is read back;
- how an axis of kind `git` is synchronized with a repository;
- the command line of the `orthant` tool.

These are described in [`design/engine.md`](../design/engine.md).
