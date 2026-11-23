                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃    Orthant, version 0.1.0.                                                                       ┃
┃    © Copyright 2026 Jon Pretty, Propensive OÜ.                                                   ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package orthant

import proscenium.List
import soundness.*

// The configuration of a store, as `.orthant/config.tel` holds it: the format version, the
// directory of the working tree relative to the store's parent, and the axes in order of
// precedence. The field names are the keywords of the file.
//
// On an axis which follows a git repository, a value is a commit: its name is the start of the
// commit's hash, `commit` is the whole of it, and `label` is the tags which name it, separated
// by spaces. A value with `branch` set is what is not yet released on the branch of its name.
//
// A value with `working` set is the one whose documents the live tree holds: what is being
// written, and is not yet released. At most one value of a store is.
case class ValueEntry
  ( name:        Text,
    parent:      Optional[Text]    = Unset,
    translation: Optional[Boolean] = Unset,
    label:       Optional[Text]    = Unset,
    commit:      Optional[Text]    = Unset,
    branch:      Optional[Boolean] = Unset,
    working:     Optional[Boolean] = Unset ):

  def labels: List[Text] = label.lay(Nil)(_.cut(t" ").filter(_ != t""))

  // The name by which the value is shown: its first tag, if it has one.
  def title: Text = labels.head.or(name)

// Where a sourced axis's content comes from: `repository` is the path of a git repository,
// relative to the directory the store is in, or that directory's own repository if it is not
// given; `prefix` is the directory of the repository's tree which the documents are in, or the
// whole tree.
case class SourceEntry(repository: Optional[Text] = Unset, prefix: Optional[Text] = Unset)

// `pattern` is the tags which an axis following a git repository takes as values; `*` stands
// for any run of characters, and no pattern takes every tag. An axis with a `source` is
// sourced: each of its values is a commit of the source, and has a `commit`.
case class AxisEntry
  ( name:    Text,
    kind:    Text,
    prefer:  Text,
    value:   List[ValueEntry],
    pattern: Optional[Text]        = Unset,
    source:  Optional[SourceEntry] = Unset )

object Settings:
  // The version of the store format, and the one before it, in which a point's records were in
  // a directory named for every one of its values at once: a store of that version is read only
  // to be made one of this.
  final val format = 2
  final val legacy = 1

  def preference(name: Text): Preference =
    if name == t"parent" then Preference.Parent else Preference.Own

  // The language of a value of a language axis: `fr` for `fr/CA`. An edge between two values
  // with different languages is a translation.
  def language(value: Text): Text = value.cut(t"/").head

// `live` is the directory of the live tree, relative to the store's parent: the documents as
// they are being written, which are edited there freely and stored by `orthant tag`. Neither
// it nor a value's `working` has any bearing on content.
case class Settings(format: Int, tree: Text, axis: List[AxisEntry], live: Optional[Text] = Unset):
  // The axis and the value which the live tree holds: the value marked as working or, where
  // none is, the one unreleased value of an axis which follows a repository, if there is one.
  def working: Optional[(AxisEntry, ValueEntry)] =
    def among(chosen: ValueEntry => Boolean): List[(AxisEntry, ValueEntry)] =
      axis.filter(_.source.absent).bind: entry => entry.value.filter(chosen).map((entry, _))

    among(_.working.or(false)) match
      case Nil => among(_.branch.or(false)) match
        case found :: Nil => found
        case _            => Unset

      case found :: _ => found

  def space: Space raises Axis.Error = Space:
    axis.map: entry =>
      val values = entry.value.map: value =>
        Value(value.name, value.parent, value.translation.or(false))

      Axis(entry.name, values, Settings.preference(entry.prefer), entry.source.present)
