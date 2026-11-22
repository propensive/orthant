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

// Probably has an `Axis` of its own, for spreading a test over values; here the name is Orthant's.
import orthant.Axis
import proscenium.{List, Nil}
import soundness.*
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.sortingAlgorithms.timsort
import soundness.strategies.throwUnsafely

// Orthant's suite. Run with `make test` (fume) or `make test-plain`.
object Tests extends Suite(m"Orthant tests"):
  def run(): Unit =
    suite(m"Bolt codes"):
      test(m"Orthant's errors are named by the letter o"):
        Orthant.bolt(6, 1).text

      . assert(_ == t"↯/o.6.1")

    suite(m"Axes and points"):
      val language =
        Axis
          ( t"language",
            List
              ( Value(t"en/GB"),
                Value(t"fr/FR", t"en/GB", translation = true),
                Value(t"fr/CA", t"fr/FR") ) )

      val release =
        Axis(t"release", List(Value(t"v1"), Value(t"v2", t"v1"), Value(t"v3", t"v2")))

      val space = Space(List(language, release))

      test(m"A value's lineage ends in the root"):
        language.lineage(t"fr/CA")

      . assert(_ == List(t"fr/CA", t"fr/FR", t"en/GB"))

      test(m"An axis with two roots is refused"):
        safely(Axis(t"language", List(Value(t"en/GB"), Value(t"fr/FR")))).absent

      . assert(_ == true)

      test(m"A cycle of parents is refused"):
        safely(Axis(t"a", List(Value(t"r"), Value(t"x", t"y"), Value(t"y", t"x")))).absent

      . assert(_ == true)

      test(m"The storage parent moves the first non-root axis"):
        space.storageParent(space.point(t"language" -> t"fr/CA", t"release" -> t"v2"))

      . assert(_ == space.point(t"language" -> t"fr/FR", t"release" -> t"v2"))

      test(m"At the language root the storage axis is the release"):
        space.storageParent(space.point(t"release" -> t"v2"))

      . assert(_ == space.point(t"release" -> t"v1"))

      test(m"A point inherits from its value at earlier releases"):
        space.origins(space.point(t"language" -> t"fr/CA", t"release" -> t"v3")).map(_(release))

      . assert(_ == List(t"v3", t"v2", t"v1"))

      test(m"Every point comes after the points it is below"):
        val points = space.points
        points.size == 9 && points.head == space.root

      . assert(_ == true)

    suite(m"Delta files"):
      val text: List[Text] =
        List
          ( t"orthant 1 language",
            t"",
            t"- Install the tool.",
            t"+ Installez l'outil.",
            t"",
            t"! Run it.",
            t"- Run it from a terminal.",
            t"+ Lancez-le.",
            t"",
            t"^",
            t"= ## macOS",
            t"=",
            t"- Run the installer.",
            t"= Last line.",
            t"$$",
            t"+ Ouvrez le fichier .dmg.",
            t"+" )

      test(m"A delta file is read as its hunks"):
        Delta.parse(text) match
          case Folio.Delta(axis, hunks) => (axis, hunks.size, hunks.map(_.stale))
          case _                        => (t"", 0, Nil)

      . assert(_ == (t"language", 3, List(false, true, false)))

      test(m"Context, document ends and empty lines are read"):
        Delta.parse(text) match
          case Folio.Delta(_, hunks) => hunks.last
          case _                     => Unset

      . assert: hunk =>
          hunk ==
            Hunk
              ( List(t"Run the installer."), List(t"Ouvrez le fichier .dmg.", t""),
                List(t"## macOS", t""), List(t"Last line."), Nil, true, true )

      test(m"Writing a parsed delta file reproduces it"):
        Delta.serialize(Delta.parse(text))

      . assert(_ == text)

      test(m"An absent record is one header line"):
        Delta.serialize(Delta.parse(List(t"orthant 1 release absent")))

      . assert(_ == List(t"orthant 1 release absent"))

      test(m"A conflict marker is not read as content"):
        safely(Delta.parse(List(t"orthant 1 language", t"", t"<<<<<<< HEAD"))).absent

      . assert(_ == true)

      test(m"A replaced line after a new line is out of order"):
        safely(Delta.parse(List(t"orthant 1 language", t"", t"+ a", t"- b"))).absent

      . assert(_ == true)

      test(m"A later format version is refused"):
        safely(Delta.parse(List(t"orthant 2 language"))).absent

      . assert(_ == true)

    suite(m"The cascade"):
      val language =
        Axis
          ( t"language",
            List
              ( Value(t"en/GB"),
                Value(t"fr/FR", t"en/GB", translation = true),
                Value(t"fr/CA", t"fr/FR") ) )

      val release =
        Axis(t"release", List(Value(t"v1"), Value(t"v2", t"v1"), Value(t"v3", t"v2")))

      val space = Space(List(language, release))

      def at(language: Text, release: Text): Point =
        space.point(t"language" -> language, t"release" -> release)

      def stale(dossier: Dossier, point: Point): Int =
        dossier.rendition(point).let(_.issues).or(Nil).count:
          case Issue.Stale(_) => true
          case _              => false

      val be = t"To be, or not to be,"
      val question = t"that is the question."
      val query = t"that is the query."
      val exit = t"Exit."
      val etre = t"Être, ou ne pas être,"
      val telle = t"telle est la question."
      val requete = t"telle est la requête."
      val sort = t"Il sort."
      val english = List(be, t"", question, t"", exit)
      val english2 = List(be, t"", query, t"", exit)
      val french = List(etre, t"", telle, t"", sort)
      val french2 = List(etre, t"", requete, t"", sort)
      val quebec = List(etre, t"", t"c'est ça la question.", t"", sort)

      val first = Dossier(space).checkin(at(t"en/GB", t"v1"), english)
      val second = first.checkin(at(t"en/GB", t"v2"), english2)
      val third = second.checkin(at(t"fr/FR", t"v1"), french)
      val fourth = third.checkin(at(t"fr/CA", t"v1"), quebec)
      val fifth = fourth.checkin(at(t"fr/FR", t"v2"), french2)
      val sixth = fifth.confirm(at(t"fr/FR", t"v2"))

      test(m"The root holds the document whole"):
        first.folios.at(at(t"en/GB", t"v1"))

      . assert(_ == Folio.Base(english))

      test(m"A release stores only the line it changes"):
        second.folios.at(at(t"en/GB", t"v2"))

      . assert: folio =>
          folio ==
            Folio.Delta
              ( t"release",
                List(Hunk(List(t"that is the question."), List(t"that is the query."))) )

      test(m"A later release inherits the change"):
        second.content(at(t"en/GB", t"v3"))

      . assert(_ == english2)

      test(m"A translation is one hunk for each paragraph"):
        third.folios.at(at(t"fr/FR", t"v1")) match
          case Folio.Delta(axis, hunks) => (axis, hunks.map(_.replaced))
          case _                        => (t"", Nil)

      . assert(_ == (t"language", List(List(be), List(question), List(exit))))

      test(m"A translation keeps its paragraph when the source changes"):
        third.content(at(t"fr/FR", t"v2"))

      . assert(_ == french)

      test(m"The paragraph whose source changed is stale"):
        stale(third, at(t"fr/FR", t"v2"))

      . assert(_ == 1)

      test(m"Nothing is stale where the source has not changed"):
        stale(third, at(t"fr/FR", t"v1"))

      . assert(_ == 0)

      test(m"A dialect is the language with its own lines"):
        fourth.content(at(t"fr/CA", t"v1"))

      . assert(_ == quebec)

      test(m"A dialect stores only the line it alters"):
        fourth.folios.at(at(t"fr/CA", t"v1")) match
          case Folio.Delta(axis, hunks) => hunks.map(_.replaced)
          case _                        => Nil

      . assert(_ == List(List(telle)))

      test(m"A dialect's line is inherited by the next release"):
        fourth.content(at(t"fr/CA", t"v2"))

      . assert(_ == quebec)

      test(m"A stale paragraph which is translated anew stays stale until it is checked"):
        (fifth.content(at(t"fr/FR", t"v2")), stale(fifth, at(t"fr/FR", t"v2")))

      . assert(_ == (french2, 1))

      test(m"Confirming a stale paragraph changes no content and clears its staleness"):
        val confirmed = fifth.confirm(at(t"fr/FR", t"v2"))
        (confirmed.content(at(t"fr/FR", t"v2")), stale(confirmed, at(t"fr/FR", t"v2")))

      . assert(_ == (french2, 0))

      test(m"A stale paragraph may be confirmed as it stands"):
        val confirmed = third.confirm(at(t"fr/FR", t"v2"))
        (confirmed.content(at(t"fr/FR", t"v2")), stale(confirmed, at(t"fr/FR", t"v2")))

      . assert(_ == (french, 0))

      test(m"The new translation is stored as one hunk at the release"):
        fifth.folios.at(at(t"fr/FR", t"v2")) match
          case Folio.Delta(axis, hunks) =>
            hunks.map: hunk => (hunk.former, hunk.replaced, hunk.lines)

          case _ =>
            Nil

      . assert(_ == List((List(question), List(query), List(requete))))

      test(m"The earlier release keeps the earlier translation"):
        fifth.content(at(t"fr/FR", t"v1"))

      . assert(_ == french)

      test(m"The dialect keeps its own line, now stale, at the new release"):
        (fifth.content(at(t"fr/CA", t"v2")), stale(fifth, at(t"fr/CA", t"v2")))

      . assert(_ == (quebec, 1))

      test(m"The new translation is inherited by the release after"):
        fifth.content(at(t"fr/FR", t"v3"))

      . assert(_ == french2)

      test(m"Checking in unchanged content changes nothing"):
        sixth.checkin(at(t"fr/FR", t"v2"), french2).folios

      . assert(_ == sixth.folios)

      val exeunt = List(be, t"", question, t"", t"Exeunt.")

      val only =
        sixth.checkin
          ( at(t"en/GB", t"v1"), exeunt, point => point(release) == t"v1" )

      test(m"An edit to an old release may be kept to that release"):
        (only.content(at(t"en/GB", t"v1")), only.content(at(t"en/GB", t"v2")))

      . assert(_ == (exeunt, english2))

      test(m"A translation of the edited release is stale there"):
        (only.content(at(t"fr/FR", t"v1")), stale(only, at(t"fr/FR", t"v1")))

      . assert(_ == (french, 1))

      test(m"A translation of the later release is untouched"):
        (only.content(at(t"fr/FR", t"v2")), stale(only, at(t"fr/FR", t"v2")))

      . assert(_ == (french2, 0))

      val cascaded = sixth.checkin(at(t"en/GB", t"v1"), exeunt)

      test(m"By default an edit to an old release reaches the later ones"):
        cascaded.content(at(t"en/GB", t"v3"))

      . assert(_ == List(be, t"", query, t"", t"Exeunt."))

      val shorter = List(be, t"", exit)
      val cut = sixth.checkin(at(t"en/GB", t"v1"), shorter)

      test(m"A translation of a deleted paragraph goes with it"):
        cut.content(at(t"fr/FR", t"v1"))

      . assert(_ == List(etre, t"", sort))

      val gone = sixth.checkin(at(t"en/GB", t"v2"), Unset)

      test(m"A document deleted at a release is absent from then on"):
        ( gone.content(at(t"en/GB", t"v1")).present, gone.content(at(t"en/GB", t"v3")).present,
          gone.content(at(t"fr/FR", t"v2")).present )

      . assert(_ == (true, false, false))

      suite(m"Indirect views"):
        val fr1 = at(t"fr/FR", t"v1")
        val fr2 = at(t"fr/FR", t"v2")
        val partial = first.checkin(fr1, List(etre, t"", question, t"", exit))

        def code(block: => Any): Text =
          try
            block
            t""
          catch case error: Indirect.Error => error.bolt.text

        test(m"A translation shows each paragraph beside the one it replaces"):
          Indirect.view(third, fr1)

        . assert: view =>
            view ==
              List
                ( t"- $be", t"+ $etre", t"", t"- $question", t"+ $telle", t"", t"- $exit",
                  t"+ $sort" )

        test(m"A stale passage shows the lines it was written against"):
          Indirect.view(third, fr2)

        . assert: view =>
            view ==
              List
                ( t"- $be", t"+ $etre", t"", t"! $question", t"- $query", t"+ $telle", t"",
                  t"- $exit", t"+ $sort" )

        test(m"A variation shows the lines it shares without a mark of doubt"):
          Indirect.view(second, at(t"en/GB", t"v2"))

        . assert(_ == List(t"  $be", t"", t"- $question", t"+ $query", t"", t"  $exit"))

        test(m"A document held in full is all new lines"):
          Indirect.view(first, at(t"en/GB", t"v1")).let(_.keep(1))

        . assert(_ == List(t"+ $be"))

        test(m"Lines yet to be translated are pending, and blank ones are not"):
          (Indirect.view(partial, fr1), partial.pending(fr1))

        . assert: result =>
            result == (
              List(t"- $be", t"+ $etre", t"", t"? $question", t"", t"? $exit"), List(2, 4) )

        test(m"A view checked in as it was changes nothing"):
          def same(dossier: Dossier, point: Point): Boolean =
            val view = Indirect.view(dossier, point).or(Nil)
            dossier.folios == Indirect.checkin(dossier, point, view).folios

          List(same(third, fr1), same(third, fr2), same(partial, fr1))

        . assert(_ == List(true, true, true))

        test(m"A pending line is translated by replacing it"):
          val view = List(t"- $be", t"+ $etre", t"", t"- $question", t"+ $telle", t"", t"? $exit")
          val translated = Indirect.checkin(partial, fr1, view)
          (translated.content(fr1), translated.pending(fr1))

        . assert(_ == (List(etre, t"", telle, t"", exit), List(4)))

        val affirmed =
          Indirect.checkin
            ( partial, fr1,
              List(t"- $be", t"+ $etre", t"", t"? $question", t"", t"- $exit", t"+ $exit") )

        test(m"A line translated by itself is pending no longer"):
          (affirmed.content(fr1), affirmed.pending(fr1))

        . assert(_ == (List(etre, t"", question, t"", exit), List(2)))

        test(m"It stays so through a later check-in, and at a later release"):
          val later = affirmed.checkin(fr1, List(etre, t"", telle, t"", exit))
          (later.pending(fr1), affirmed.pending(fr2))

        . assert(_ == (Nil, List(2)))

        test(m"A replaced line with nothing in its place is left out"):
          val view = List(t"- $be", t"+ $etre", t"", t"- $question", t"", t"? $exit")
          Indirect.checkin(partial, fr1, view).content(fr1)

        . assert(_ == List(etre, t"", t"", exit))

        test(m"A pending line with lines added beneath it is translated by them"):
          val view = List(t"- $be", t"+ $etre", t"", t"? $question", t"+ $telle", t"", t"? $exit")
          val translated = Indirect.checkin(partial, fr1, view)
          (translated.content(fr1), translated.pending(fr1))

        . assert(_ == (List(etre, t"", telle, t"", exit), List(4)))

        test(m"A line left out, and later translated, is held as one hunk"):
          val omitted = List(t"- $be", t"+ $etre", t"", t"- $question", t"", t"? $exit")
          val view = List(t"- $be", t"+ $etre", t"", t"- $question", t"+ $telle", t"", t"? $exit")
          val filled = Indirect.checkin(Indirect.checkin(partial, fr1, omitted), fr1, view)
          val shown = Indirect.view(filled, fr1) == view
          (filled.content(fr1), filled.rendition(fr1).let(_.placements.size), shown)

        . assert(_ == (List(etre, t"", telle, t"", exit), 2, true))

        test(m"A stale passage left alone is still stale"):
          stale(Indirect.checkin(third, fr2, Indirect.view(third, fr2).or(Nil)), fr2)

        . assert(_ == 1)

        test(m"Deleting its former lines confirms it"):
          val view = Indirect.view(third, fr2).or(Nil).filter(_ != t"! $question")
          val checked = Indirect.checkin(third, fr2, view)
          (checked.content(fr2), stale(checked, fr2), stale(checked, at(t"fr/FR", t"v3")))

        . assert(_ == (french, 0, 0))

        test(m"Revising it and deleting its former lines stores the revision, fresh"):
          val view =
            List
              ( t"- $be", t"+ $etre", t"", t"- $query", t"+ $requete", t"", t"- $exit", t"+ $sort" )

          val checked = Indirect.checkin(third, fr2, view)
          (checked.content(fr2), stale(checked, fr2), checked.content(fr1))

        . assert(_ == (french2, 0, french))

        test(m"Revising it and keeping its former lines leaves it stale"):
          val view =
            List
              ( t"- $be", t"+ $etre", t"", t"! $question", t"- $query", t"+ $requete", t"",
                t"- $exit", t"+ $sort" )

          val checked = Indirect.checkin(third, fr2, view)
          (checked.content(fr2), stale(checked, fr2))

        . assert(_ == (french2, 1))

        test(m"Former lines may not be edited"):
          val view = Indirect.view(third, fr2).or(Nil).map: line =>
            if line == t"! $question" then t"! something else" else line

          code(Indirect.checkin(third, fr2, view))

        . assert(_ == t"↯/o.4.9")

        test(m"The reference's lines may not be edited"):
          val view = Indirect.view(third, fr1).or(Nil).map: line =>
            if line == t"- $exit" then t"- Exeunt." else line

          code(Indirect.checkin(third, fr1, view))

        . assert(_ == t"↯/o.4.8")

        test(m"Every line of a view has a sigil"):
          code(Indirect.checkin(third, fr1, List(t"- $be", t"Être")))

        . assert(_ == t"↯/o.4.7")

        test(m"A bare sigil is an empty line of its kind"):
          val view =
            List(t"- $be", t"+ $etre", t"+", t"+ encore", t"", t"? $question", t"", t"? $exit")

          Indirect.checkin(partial, fr1, view).content(fr1)

        . assert(_ == List(etre, t"", t"encore", t"", question, t"", exit))

        test(m"A view against a change shows what the reference was where it changed"):
          Indirect.alterationView(third, fr2, english)

        . assert: view =>
            view ==
              List
                ( t"  $etre", t"", t"! $question", t"- $query", t"+ $telle", t"", t"  $sort" )

        test(m"Where the change touched nothing the view is the content alone"):
          Indirect.alterationView(third, fr1, english)

        . assert(_ == List(t"  $etre", t"", t"  $telle", t"", t"  $sort"))

        test(m"A changed line which is not translated is shown as yet to be translated"):
          Indirect.alterationView(partial.checkin(at(t"en/GB", t"v2"), english2), fr2, english)

        . assert: view => view == List(t"  $etre", t"", t"? $query", t"", t"  $exit")

        test(m"Left as it is, such a line is still to be translated"):
          val changed = partial.checkin(at(t"en/GB", t"v2"), english2)
          val view = Indirect.alterationView(changed, fr2, english).or(Nil)
          val request = Indirect.alterationRead(changed, fr2, english, view)
          val pending = changed.checkin(fr2, request.lines).pending(fr2).size
          (request.lines == changed.content(fr2), pending)

        . assert(_ == (true, 2))

        test(m"It is translated, in such a view, by the lines added beneath it"):
          val changed = partial.checkin(at(t"en/GB", t"v2"), english2)
          val view = List(t"  $etre", t"", t"? $query", t"+ $requete", t"", t"  $exit")
          val request = Indirect.alterationRead(changed, fr2, english, view)
          val checked = changed.checkin(fr2, request.lines)
          (checked.content(fr2), checked.pending(fr2).size)

        . assert(_ == (List(etre, t"", requete, t"", exit), 1))

        test(m"Deleting the former lines of such a view, and revising, stores it fresh"):
          val view = List(t"  $etre", t"", t"- $query", t"+ $requete", t"", t"  $sort")
          val request = Indirect.alterationRead(third, fr2, english, view)
          val checked = Indirect.confirm(third, fr2, request).checkin(fr2, request.lines)
          (request.checked.size, checked.content(fr2), stale(checked, fr2))

        . assert(_ == (1, french2, 0))

        test(m"Leaving its former lines leaves it stale, though its other lines may be edited"):
          val edited = t"  Être ou ne pas être,"
          val view = List(edited, t"", t"! $question", t"- $query", t"+ $telle", t"", t"  $sort")

          val request = Indirect.alterationRead(third, fr2, english, view)

          val checked = Indirect.confirm(third, fr2, request).checkin(fr2, request.lines)
          (request.checked.size, checked.content(fr2).let(_.keep(1)), stale(checked, fr2))

        . assert(_ == (0, List(t"Être ou ne pas être,"), 1))

        test(m"A passage of such a view may be made into several"):
          val longer =
            second.checkin(at(t"en/GB", t"v2"), List(be, t"", query, t"More.", t"", exit))

          val both = longer.checkin(fr1, french)

          val shown = Indirect.alterationView(both, fr2, english)

          val view =
            List
              ( t"  $etre", t"", t"- $query", t"+ $requete", t"- More.", t"+ Encore.", t"",
                t"  $sort" )

          val request = Indirect.alterationRead(both, fr2, english, view)
          val checked = Indirect.confirm(both, fr2, request).checkin(fr2, request.lines)
          (shown.let(_.excerpt(2, 6)), checked.content(fr2), stale(checked, fr2))

        . assert: result =>
            result == (
              List(t"! $question", t"- $query", t"- More.", t"+ $telle"),
              List(etre, t"", requete, t"Encore.", t"", sort), 0 )

        test(m"The lines the reference has now may not be edited in it"):
          val view =
            List(t"  $etre", t"", t"! $question", t"- something else", t"+ $telle", t"", t"  $sort")

          code(Indirect.alterationRead(third, fr2, english, view))

        . assert(_ == t"↯/o.4.8")

        test(m"Nor may its former lines, except to delete them all"):
          val view =
            List(t"  $etre", t"", t"! something else", t"- $query", t"+ $telle", t"", t"  $sort")

          code(Indirect.alterationRead(third, fr2, english, view))

        . assert(_ == t"↯/o.4.9")

    suite(m"Repeated lines and preferences"):
      val release = Axis(t"release", List(Value(t"v1"), Value(t"v2", t"v1")))

      def language(preference: Preference): Axis =
        Axis
          ( t"language", List(Value(t"en"), Value(t"fr", t"en", translation = true)),
            preference )

      def stale(dossier: Dossier, point: Point): Int = dossier.issues(point).count:
        case Issue.Stale(_) => true
        case _              => false

      val linux = t"## Linux"
      val macos = t"## macOS"
      val run = t"Run the installer."
      val lancez = t"Lancez l'installateur."
      val ouvrez = t"Ouvrez le fichier .dmg."

      val space = Space(List(language(Preference.Own), release))
      val fr1 = space.point(t"language" -> t"fr")
      val fr2 = space.point(t"language" -> t"fr", t"release" -> t"v2")

      val installers =
        Dossier(space).checkin(space.root, List(linux, run, macos, run))
        . checkin(fr1, List(linux, lancez, macos, ouvrez))

      test(m"A hunk on a repeated line is anchored by the line before it"):
        installers.folios.at(fr1) match
          case Folio.Delta(_, hunks) => hunks.map(_.before)
          case _                     => Nil

      . assert(_ == List(List(linux), List(macos)))

      val swapped =
        installers.checkin
          ( space.point(t"release" -> t"v2"), List(macos, run, linux, run) )

      // The translation under the heading which kept its place follows it. The other's lines were
      // deleted and written again elsewhere, so it is dropped, and its line shows untranslated:
      // finding it a new place is left for later. What must never happen is a translation under
      // the wrong heading with nothing to say so.
      test(m"Swapping sections with the same line never misplaces a translation"):
        swapped.content(fr2)

      . assert(_ == List(macos, ouvrez, linux, run))

      val space2 = Space(List(language(Preference.Parent), release))
      val exit = t"Exit."
      val exeunt = t"Exeunt."
      val sort = t"Il sort."

      val preferring =
        Dossier(space2).checkin(space2.root, List(exit))
        . checkin(space2.point(t"language" -> t"fr"), List(sort))
        . checkin(space2.point(t"release" -> t"v2"), List(exeunt))

      test(m"An axis which prefers the parent shows its lines where a hunk is stale"):
        val point = space2.point(t"language" -> t"fr", t"release" -> t"v2")
        (preferring.content(point), stale(preferring, point))

      . assert(_ == (List(exeunt), 1))

      test(m"Confirming it keeps the parent's lines and clears the staleness"):
        val point = space2.point(t"language" -> t"fr", t"release" -> t"v2")
        val confirmed = preferring.confirm(point)
        (confirmed.content(point), stale(confirmed, point))

      . assert(_ == (List(exeunt), 0))

      test(m"Its view shows the hunk's own lines, and leaving it alone keeps the parent's"):
        val point = space2.point(t"language" -> t"fr", t"release" -> t"v2")
        val view = Indirect.view(preferring, point).or(Nil)
        (view, Indirect.checkin(preferring, point, view).content(point))

      . assert(_ == (List(t"! $exit", t"- $exeunt", t"+ $sort"), List(exeunt)))

      test(m"Deleting the former lines in its view brings its own lines back"):
        val point = space2.point(t"language" -> t"fr", t"release" -> t"v2")
        val checked = Indirect.checkin(preferring, point, List(t"- $exeunt", t"+ $sort"))
        (checked.content(point), stale(checked, point))

      . assert(_ == (List(sort), 0))

      test(m"An edit where the parent's lines show is stored, and is not stale"):
        val point = space2.point(t"language" -> t"fr", t"release" -> t"v2")
        val edited = preferring.checkin(point, List(t"Ils sortent."))
        (edited.content(point), stale(edited, point))

      . assert(_ == (List(t"Ils sortent."), 0))

    suite(m"Edits sent elsewhere"):
      val release = Axis(t"release", List(Value(t"v1"), Value(t"v2", t"v1"), Value(t"v3", t"v2")))
      val space = Space(List(release))
      val v1 = space.root
      val v2 = space.point(t"release" -> t"v2")
      val v3 = space.point(t"release" -> t"v3")

      val base =
        Dossier(space).checkin(v1, List(t"a", t"b", t"c")).checkin(v2, List(t"a", t"B", t"c"))
        . checkin(v3, List(t"a", t"B", t"C"))

      def code(block: => Any): Text =
        try
          block
          t""
        catch case error: Checkin.Error => error.bolt.text

      test(m"An edit stored since an earlier value reaches it, and not the values before it"):
        val sent = Retro.since(base, v3, List(t"A", t"B", t"C"), v2)
        (sent.content(v1), sent.content(v2), sent.content(v3))

      . assert(_ == (List(t"a", t"b", t"c"), List(t"A", t"B", t"c"), List(t"A", t"B", t"C")))

      test(m"It is stored at that value, and the value it was made at holds nothing new"):
        Retro.since(base, v3, List(t"A", t"B", t"C"), v2).folios.at(v3) == base.folios.at(v3)

      . assert(_ == true)

      test(m"An edit to lines which the earlier value does not have is refused"):
        code(Retro.since(base, v3, List(t"a", t"B", t"D"), v1))

      . assert(_ == t"↯/o.3.4")

      test(m"An edit cannot be stored since a value which is not an ancestor"):
        code(Retro.since(base, v2, List(t"a", t"B", t"x"), v3))

      . assert(_ == t"↯/o.3.2")

      test(m"Each change goes to the value which first had its lines, and insertions stay"):
        val sent = Retro.origin(base, v3, List(t"A", t"B2", t"C", t"new"), release)
        (sent.content(v1), sent.content(v2), sent.content(v3))

      . assert: contents =>
          contents == (
            List(t"A", t"b", t"c"), List(t"A", t"B2", t"c"), List(t"A", t"B2", t"C", t"new") )

      test(m"A change to lines of the value it is made at stays there"):
        val sent = Retro.origin(base, v3, List(t"a", t"B", t"C2"), release)
        (sent.content(v2), sent.content(v3))

      . assert(_ == (List(t"a", t"B", t"c"), List(t"a", t"B", t"C2")))

    suite(m"Changes which meet"):
      val release = Axis(t"release", List(Value(t"v1"), Value(t"v2", t"v1")))
      val space = Space(List(release))
      val second = space.point(t"release" -> t"v2")

      val apart =
        Dossier(space).checkin(space.root, List(t"a", t"x", t"b"))
        . checkin(second, List(t"a", t"p", t"x", t"q", t"b"))

      test(m"Two insertions are two hunks while a line lies between them"):
        apart.folios.at(second) match
          case Folio.Delta(_, hunks) => hunks.map(_.lines)
          case _                     => Nil

      . assert(_ == List(List(t"p"), List(t"q")))

      val met = apart.checkin(space.root, List(t"a", t"b"))

      test(m"When that line is deleted both insertions remain, in their order"):
        met.content(second)

      . assert(_ == List(t"a", t"p", t"q", t"b"))

      test(m"They are stored as one hunk, since two at one place could not both apply"):
        met.folios.at(second) match
          case Folio.Delta(_, hunks) => hunks.map(_.lines)
          case _                     => Nil

      . assert(_ == List(List(t"p", t"q")))

    suite(m"The store on disk"):
      def fixture(): Workspace =
        val root = java.nio.file.Files.createTempDirectory("orthant").nn.toString.nn.tt

        Workspace.init(root, t"docs").axis(t"language", t"en", t"language")
        . value(t"language", t"fr", Unset).value(t"language", t"fr/CA", t"fr")
        . axis(t"release", t"v1", t"custom").value(t"release", t"v2", Unset)

      def edit(store: Workspace, document: Text, content: Text): Unit =
        Disk.write(t"${store.tree}/$document", content, store.staging)

      def code(block: => Any): Text =
        try
          block
          t""
        catch
          case error: Store.Error     => error.bolt.text
          case error: Workspace.Error => error.bolt.text

      // A store with a document at the root, its translation, and a change at the second release.
      def populated(): Workspace =
        val store = fixture()
        edit(store, t"guide/intro.md", t"Install it.\n\nRun it.\n")
        Worktree.checkin(store, Nil, false, false)
        val store2 = Workspace.load(store.root)
        Worktree.checkout(store2, store2.locate(store2.space.root, List(t"fr")), false)
        edit(store2, t"guide/intro.md", t"Installez-le.\n\nLancez-le.\n")
        Worktree.checkin(store2, Nil, false, false)
        val store3 = Workspace.load(store.root)
        Worktree.checkout(store3, store3.locate(store3.space.root, List(t"v2")), false)
        edit(store3, t"guide/intro.md", t"Install it.\n\nRun it now.\n")
        Worktree.checkin(store3, Nil, false, false)
        Workspace.load(store.root)

      test(m"A last line without a newline is a line all the same"):
        (Store.lines(t"one\ntwo\n"), Store.lines(t"one\ntwo"), Store.lines(t""))

      . assert(_ == (List(t"one", t"two"), List(t"one", t"two"), Nil))

      test(m"A point's directory names its values which are not roots"):
        val store = fixture()
        val point = store.locate(store.space.root, List(t"fr/CA", t"release=v2"))
        (store.name(point), Store.point(store.space, store.name(point)) == point)

      . assert(_ == (t"language=fr_CA,release=v2", true))

      test(m"The root point's directory is root"):
        val store = fixture()
        store.name(store.space.root)

      . assert(_ == t"root")

      test(m"A value on two axes must name its axis"):
        val store = fixture().axis(t"edition", t"v2", t"custom")
        code(store.locate(store.space.root, List(t"v2")))

      . assert(_ == t"↯/o.2.7")

      test(m"The configuration is read back as it was written"):
        val store = fixture()
        Workspace.load(store.root).settings

      . assert(_ == fixture().settings)

      test(m"Checked-in documents are stored as a base and its deltas"):
        val store = populated()
        Disk.files(store.data)

      . assert: files =>
          files ==
            List
              ( t"language=fr/guide/intro.md.delta",
                t"release=v2/guide/intro.md.delta",
                t"root/guide/intro.md" )

      test(m"A reloaded store gives each point its content"):
        val store = populated()
        val dossier = store.dossier(t"guide/intro.md")

        def at(values: Text*): Optional[List[Text]] =
          dossier.content(store.locate(store.space.root, values.to(scala.List).pipe(List.from(_))))

        (at(t"v2"), at(t"fr/CA"), at(t"fr", t"v2"))

      . assert: contents =>
          contents == (
            List(t"Install it.", t"", t"Run it now."),
            List(t"Installez-le.", t"", t"Lancez-le."),
            List(t"Installez-le.", t"", t"Lancez-le.") )

      test(m"A populated store is sound, and its stale translation is not a fault"):
        Workspace.scan(populated().root)(1).size

      . assert(_ == 0)

      test(m"A check-in which changes nothing writes nothing"):
        val store = populated()
        Worktree.checkin(store, Nil, false, false).stored.size

      . assert(_ == 0)

      test(m"A check-out is refused over edits which are not checked in"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Edited.\n")
        code(Worktree.checkout(store, store.space.root, false))

      . assert(_ == t"↯/o.4.2")

      // As in a fresh clone of a repository which tracks some of the working tree's files: none
      // was checked out, so all are new, but they hold what a check-out would write.
      def unrecorded(store: Workspace): Unit =
        val pristine = t"${store.work}/pristine"
        Disk.files(pristine).each: file => Disk.remove(t"$pristine/$file", pristine)

      test(m"A check-out over files which hold what it writes is not refused"):
        val store = populated()
        unrecorded(store)
        Worktree.checkout(store, Worktree.view(store), false)

      . assert(_ == 1)

      test(m"A check-out over a new file which it would change is refused"):
        val store = populated()
        unrecorded(store)
        edit(store, t"guide/intro.md", t"Edited.\n")
        code(Worktree.checkout(store, Worktree.view(store), false))

      . assert(_ == t"↯/o.4.2")

      test(m"A forced check-out discards the edits"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Edited.\n")
        Worktree.checkout(store, store.space.root, true)
        Disk.read(t"${store.tree}/guide/intro.md")

      . assert(_ == t"Install it.\n\nRun it.\n")

      test(m"A check-out removes a document which the point does not have"):
        val store = populated()
        edit(store, t"extra.md", t"Only at the second release.\n")
        Worktree.checkin(store, Nil, false, false)
        val store2 = Workspace.load(store.root)
        Worktree.checkout(store2, store2.space.root, false)
        Disk.files(store2.tree)

      . assert(_ == List(t"guide/intro.md"))

      test(m"A missing document is passed over unless it is to be removed"):
        val store = populated()
        Disk.remove(t"${store.tree}/guide/intro.md", store.tree)
        val tally = Worktree.checkin(store, Nil, false, false)
        (tally.stored.size, tally.skipped.size)

      . assert(_ == (0, 1))

      test(m"Removing a document makes it absent at that point and present above"):
        val store = populated()
        Disk.remove(t"${store.tree}/guide/intro.md", store.tree)
        Worktree.checkin(store, Nil, true, false)
        val store2 = Workspace.load(store.root)
        val dossier = store2.dossier(t"guide/intro.md")
        val second = store2.locate(store2.space.root, List(t"v2"))
        (dossier.content(second).absent, dossier.content(store2.space.root).present)

      . assert(_ == (true, true))

      test(m"A dry run stores nothing"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Edited.\n")
        Worktree.checkin(store, Nil, false, true)
        val reloaded = Workspace.load(store.root).dossier(t"guide/intro.md").folios
        reloaded == store.dossier(t"guide/intro.md").folios

      . assert(_ == true)

      test(m"A check-in is refused if the store changed beneath the working tree"):
        val store = populated()
        val stale = Workspace.load(store.root)
        edit(store, t"guide/intro.md", t"Install it.\n\nRun it at once.\n")
        Worktree.checkin(store, Nil, false, false)
        val pristine = t"${store.work}/pristine/guide/intro.md"
        Disk.write(pristine, t"Install it.\n\nRun it now.\n", store.staging)
        edit(store, t"guide/intro.md", t"Another edit.\n")
        code(Worktree.checkin(Workspace.load(stale.root), Nil, false, false))

      . assert(_ == t"↯/o.4.3")

      test(m"A check-in of some paths leaves the others"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Edited.\n")
        edit(store, t"other/notes.md", t"Notes.\n")
        Worktree.checkin(store, List(t"other"), false, false).stored.map(_.document)

      . assert(_ == List(t"other/notes.md"))

      test(m"A conflict marker in a delta is a fault with the delta format's code"):
        val store = populated()
        val file = t"${store.data}/release=v2/guide/intro.md.delta"
        Disk.write(file, t"${Disk.read(file).or(t"")}<<<<<<< HEAD\n", store.staging)
        Workspace.scan(store.root)(1).map(_.bolt.text)

      . assert(_ == List(t"↯/o.1.2"))

      test(m"A base and a delta for one document at one point is a fault"):
        val store = populated()
        Disk.write(t"${store.data}/release=v2/guide/intro.md", t"Both.\n", store.staging)
        Workspace.scan(store.root)(1).map(_.bolt.text)

      . assert(_ == List(t"↯/o.1.5"))

      test(m"A directory which names no point is a fault"):
        val store = populated()
        Disk.write(t"${store.data}/language=de/guide/intro.md", t"Hallo.\n", store.staging)
        Workspace.scan(store.root)(1).map(_.bolt.text)

      . assert(_ == List(t"↯/o.1.7"))

      test(m"A directory which names an axis twice is a fault"):
        val store = populated()
        val folder = t"${store.data}/language=fr,language=fr"
        Disk.write(t"$folder/guide/other.md", t"Bonjour.\n", store.staging)
        Workspace.scan(store.root)(1).map(_.bolt.text)

      . assert(_ == List(t"↯/o.1.7"))

      test(m"A store of a later format is refused"):
        val store = populated()
        val file = t"${store.folder}/config.tel"
        val later = Store.lines(Disk.read(file).or(t"")).map: line =>
          if line == t"format 1" then t"format 2" else line

        Disk.write(file, Store.text(later), store.staging)
        code(Workspace.scan(store.root))

      . assert(_ == t"↯/o.5.1")

      test(m"Two axes of one name are refused"):
        val store = populated()
        val file = t"${store.folder}/config.tel"
        val lines = Store.lines(Disk.read(file).or(t""))
        val twice = lines + List(t"axis", t"  name release", t"  kind custom", t"  prefer own")
        Disk.write(file, Store.text(twice + List(t"  value", t"    name v1")), store.staging)
        code(Workspace.scan(store.root))

      . assert(_ == t"↯/o.5.1")

      test(m"Two values of one name on an axis are refused"):
        val store = populated()
        val file = t"${store.folder}/config.tel"
        val lines = Store.lines(Disk.read(file).or(t""))
        val twice = lines + List(t"  value", t"    name v2", t"    parent v1")
        Disk.write(file, Store.text(twice), store.staging)
        code(Workspace.scan(store.root))

      . assert(_ == t"↯/o.5.1")

      test(m"A delta whose lines are not in its parent is a fault"):
        val store = populated()
        Disk.write(t"${store.data}/root/guide/intro.md", t"Something else.\n", store.staging)
        Workspace.scan(store.root)(1).map(_.bolt.text).distinct

      . assert(_ == List(t"↯/o.1.8"))

      test(m"A hunk with no place is found where its document is read or changed, not before"):
        val made = populated()
        Disk.write(t"${made.data}/root/guide/intro.md", t"Something else.\n", made.staging)
        val store = Workspace.open(made.root)
        val root = store.space.root
        val french = store.locate(root, List(t"fr"))
        val plain = store.content(t"guide/intro.md", root)
        Worktree.checkout(store, root, true)
        edit(store, t"guide/intro.md", t"Something else again.\n")

        ( plain, code(Workspace.load(made.root)), code(store.content(t"guide/intro.md", french)),
          code(Worktree.checkout(store, french, true)),
          code(Worktree.checkin(store, Nil, false, false)) )

      . assert(_ == (List(t"Something else."), t"↯/o.1.8", t"↯/o.1.8", t"↯/o.1.8", t"↯/o.1.8"))

      test(m"Documents derived in tasks of their own are as they are when derived in turn"):
        import threads.virtualThreads
        val store = populated()
        val ready = Workspace.inspect(store.root)
        supervise(ready.prepare(ready.points))
        def all(store: Workspace) = store.points.map(store.content(t"guide/intro.md", _))
        (all(ready) == all(store), ready.faults.size, all(ready).size)

      . assert(_ == (true, 0, 6))

      test(m"An indirect check-out writes views, and a check-in of one stores its content"):
        val store = populated()
        val french = store.locate(store.space.root, List(t"fr", t"v2"))
        Worktree.checkout(store, french, false, Worktree.Mode.Beside)
        val view = Disk.read(t"${store.tree}/guide/intro.md").or(t"")
        val revised = t"- Install it.\n+ Installez-le.\n\n- Run it now.\n+ Lancez-le maintenant.\n"
        edit(store, t"guide/intro.md", revised)
        Worktree.checkin(store, Nil, false, false)
        val after = Workspace.load(store.root)

        ( view, after.dossier(t"guide/intro.md").content(french),
          Worktree.changes(after).size, after.dossier(t"guide/intro.md").issues(french).size )

      . assert: result =>
          result == (
            t"- Install it.\n+ Installez-le.\n\n! Run it.\n- Run it now.\n+ Lancez-le.\n",
            List(t"Installez-le.", t"", t"Lancez-le maintenant."), 0, 0 )

      test(m"An indirect check-out is refused where there is no storage parent"):
        val store = populated()
        code(Worktree.checkout(store, store.space.root, false, Worktree.Mode.Beside))

      . assert(_ == t"↯/o.4.10")

      test(m"A view without sigils is refused with the view's code"):
        val store = populated()
        val target = store.locate(store.space.root, List(t"fr"))
        Worktree.checkout(store, target, false, Worktree.Mode.Beside)
        edit(store, t"guide/intro.md", t"Installez-le.\n")
        code(Worktree.checkin(store, Nil, false, false))

      . assert(_ == t"↯/o.4.7")

      test(m"Confirming a document clears its stale passages and writes its view again"):
        val store = populated()
        val french = store.locate(store.space.root, List(t"fr", t"v2"))
        Worktree.checkout(store, french, false, Worktree.Mode.Beside)
        val found = Worktree.confirm(store, Nil)
        val after = Workspace.load(store.root)

        ( found, after.dossier(t"guide/intro.md").issues(french).size, Worktree.changes(after).size,
          Disk.read(t"${store.tree}/guide/intro.md") )

      . assert: result =>
          result == (
            List((t"guide/intro.md", 1)), 0, 0,
            t"- Install it.\n+ Installez-le.\n\n- Run it now.\n+ Lancez-le.\n" )

      test(m"A strict export is refused while a document is stale"):
        val store = populated()
        val french = store.locate(store.space.root, List(t"fr", t"v2"))
        code(Worktree.publish(store, french, t"${store.root}/out", true))

      . assert(_ == t"↯/o.4.11")

      test(m"An export writes the plain documents"):
        val store = populated()
        val french = store.locate(store.space.root, List(t"fr", t"v2"))
        Worktree.publish(store, french, t"${store.root}/out", false)
        Disk.read(t"${store.root}/out/guide/intro.md")

      . assert(_ == t"Installez-le.\n\nLancez-le.\n")

      test(m"What an axis prefers cannot change once hunks are stored on it"):
        val store = populated()
        (code(store.prefer(t"language", true)), code(fixture().prefer(t"language", true)))

      . assert(_ == (t"↯/o.2.9", t""))

      test(m"A check-in since an earlier value is stored there"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Install it first.\n\nRun it now.\n")
        Worktree.checkin(store, Nil, false, false, Worktree.Reach.Since(t"v1"))
        val after = Workspace.load(store.root)
        (after.dossier(t"guide/intro.md").content(after.space.root), Worktree.changes(after).size)

      . assert(_ == (List(t"Install it first.", t"", t"Run it."), 0))

      test(m"A check-in by origin sends each change to the release which had its lines"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Install it first.\n\nRun it at once.\n")
        Worktree.checkin(store, Nil, false, false, Worktree.Reach.Origin(t"release"))
        val dossier = Workspace.load(store.root).dossier(t"guide/intro.md")

        ( dossier.content(store.space.root),
          dossier.content(store.locate(store.space.root, List(t"v2"))) )

      . assert: contents =>
          contents == (
            List(t"Install it first.", t"", t"Run it."),
            List(t"Install it first.", t"", t"Run it at once.") )

      test(m"A check-in kept to one release leaves the other alone"):
        val store = populated()
        Worktree.checkout(store, store.space.root, false)
        edit(store, t"guide/intro.md", t"Install it first.\n\nRun it.\n")
        Worktree.checkin(store, Nil, false, false, Worktree.Reach.Only(t"release"))
        val after = Workspace.load(store.root)
        after.dossier(t"guide/intro.md").content(after.locate(after.space.root, List(t"v2")))

      . assert(_ == List(t"Install it.", t"", t"Run it now."))

      test(m"A refused check-in names the document, the point and the engine's code"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Install it.\n\nRun it soon.\n")
        code(Worktree.checkin(store, Nil, false, false, Worktree.Reach.Since(t"v1")))

      . assert(_ == t"↯/o.3.4")

      test(m"A view against a change is checked out, edited and checked in"):
        val store = populated()
        val french = store.locate(store.space.root, List(t"fr", t"v2"))
        Worktree.checkout(store, french, false, Worktree.Mode.Against(t"release", t"v1"))
        val view = Disk.read(t"${store.tree}/guide/intro.md")
        val revised = t"  Installez-le.\n\n- Run it now.\n+ Lancez-le maintenant.\n"
        edit(store, t"guide/intro.md", revised)
        Worktree.checkin(store, Nil, false, false)
        val after = Workspace.load(store.root)

        ( view, after.dossier(t"guide/intro.md").content(french),
          after.dossier(t"guide/intro.md").issues(french).size, Worktree.changes(after).size,
          Disk.read(t"${store.tree}/guide/intro.md") )

      . assert: result =>
          result == (
            t"  Installez-le.\n\n! Run it.\n- Run it now.\n+ Lancez-le.\n",
            List(t"Installez-le.", t"", t"Lancez-le maintenant."), 0, 0,
            t"  Installez-le.\n\n  Lancez-le maintenant.\n" )

      test(m"A view against a change must be from an ancestor on another axis"):
        val store = populated()
        val french = store.locate(store.space.root, List(t"fr", t"v2"))

        ( code(Worktree.checkout(store, french, false, Worktree.Mode.Against(t"language", t"en"))),
          code(Worktree.checkout(store, french, false, Worktree.Mode.Against(t"release", t"v2"))) )

      . assert(_ == (t"↯/o.4.13", t""))

      test(m"Changing the reference keeps edits which are not checked in"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Install it please.\n\nRun it now.\n")
        Worktree.refer(store, Worktree.Mode.Beside)
        val view = Disk.read(t"${store.tree}/guide/intro.md")
        val pending = Worktree.changes(store).size
        Worktree.checkin(store, Nil, false, false)
        val after = Workspace.load(store.root)

        ( view, pending,
          after.dossier(t"guide/intro.md").content(after.locate(after.space.root, List(t"v2"))) )

      . assert: result =>
          result == (
            t"- Install it.\n+ Install it please.\n\n- Run it.\n+ Run it now.\n", 1,
            List(t"Install it please.", t"", t"Run it now.") )

      test(m"And going back to plain documents keeps them too"):
        val store = populated()
        Worktree.refer(store, Worktree.Mode.Beside)
        val revised = t"- Install it.\n+ Install it please.\n\n- Run it.\n+ Run it now.\n"
        edit(store, t"guide/intro.md", revised)
        Worktree.refer(store, Worktree.Mode.Plain)
        (Disk.read(t"${store.tree}/guide/intro.md"), Worktree.changes(store).size)

      . assert(_ == (t"Install it please.\n\nRun it now.\n", 1))

      test(m"An update carries edits onto a store which changed beneath the working tree"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Install it please.\n\nRun it now.\n")
        val held = t"${store.data}/root/guide/intro.md"
        Disk.write(held, t"Install it.\n\nRun it.\n\nMore.\n", store.staging)
        val moved = Workspace.load(store.root)
        val refused = code(Worktree.checkin(moved, Nil, false, false))
        val written = Worktree.update(moved)
        val tree = Disk.read(t"${store.tree}/guide/intro.md")
        Worktree.checkin(moved, Nil, false, false)

        ( refused, written, tree,
          Workspace.load(store.root).dossier(t"guide/intro.md").content(moved.space.root) )

      . assert: result =>
          result == (
            t"↯/o.4.3", List(t"guide/intro.md"), t"Install it please.\n\nRun it now.\n\nMore.\n",
            List(t"Install it.", t"", t"Run it.", t"", t"More.") )

      test(m"An update is refused where the store changed the lines which were edited"):
        val store = fixture()
        edit(store, t"notes.md", t"Install it.\n\nRun it.\n")
        Worktree.checkin(store, Nil, false, false)
        edit(store, t"notes.md", t"Install it please.\n\nRun it.\n")
        Disk.write(t"${store.data}/root/notes.md", t"Install them.\n\nRun it.\n", store.staging)
        code(Worktree.update(Workspace.load(store.root)))

      . assert(_ == t"↯/o.4.12")

      test(m"The edits which are not checked in are listed by the line they begin at"):
        val store = populated()
        edit(store, t"guide/intro.md", t"Install it.\n\nRun it today.\n")
        Worktree.differences(store, Nil).map(_(1))

      . assert(_ == List(List(t"@ 3", t"- Run it now.", t"+ Run it today.")))

      test(m"The work at a point is its lines to translate and its stale passages"):
        val store = populated()
        val point = store.locate(store.space.root, List(t"fr", t"v2"))
        val work = Survey.work(store, t"guide/intro.md", point)
        (work.total, work.pending.size, work.stale.size, Survey.sum(store, point).percent)

      . assert(_ == (2, 0, 1, 100))

      test(m"Work which a point above has too is not the point's own"):
        val store = fixture()
        edit(store, t"notes.md", t"Install it.\n\nRun it.\n")
        Worktree.checkin(store, Nil, false, false)
        val loaded = Workspace.load(store.root)
        val first = loaded.locate(loaded.space.root, List(t"fr"))
        val second = loaded.locate(first, List(t"v2"))

        ( Survey.own(loaded, t"notes.md", first).pending.size,
          Survey.work(loaded, t"notes.md", second).pending.size,
          Survey.own(loaded, t"notes.md", second).pending.size,
          Worktree.reach(loaded, t"notes.md", first) )

      . assert(_ == (2, 2, 0, 1))

      test(m"Work which a change brought is best seen against that change"):
        val store = populated()
        val point = store.locate(store.space.root, List(t"fr", t"v2"))
        val first = store.locate(store.space.root, List(t"fr"))
        (Worktree.approach(store, point), Worktree.approach(store, first))

      . assert(_ == (Worktree.Mode.Against(t"release", t"v1"), Worktree.Mode.Beside))

      test(m"A check-in tells what it does to the work at the points it reaches"):
        val store = fixture()
        edit(store, t"notes.md", t"Install it.\n\nRun it.\n")
        val tally = Worktree.checkin(store, Nil, false, true)

        tally.effects.filter(_.pending != 0).map: effect =>
          (store.name(effect.point), effect.pending)

      . assert(_ == List((t"language=fr", 2)))

      test(m"The edits which are not checked in can be counted as if they were"):
        val store = fixture()
        edit(store, t"notes.md", t"Install it.\n\nRun it.\n")
        Worktree.checkin(store, Nil, false, false)
        val loaded = Workspace.load(store.root)
        val point = loaded.locate(loaded.space.root, List(t"fr"))
        Worktree.checkout(loaded, point, false, Worktree.Mode.Beside)
        edit(loaded, t"notes.md", t"? Install it.\n+ Installez-le.\n\n? Run it.\n")
        val (seen, failed) = Worktree.prospect(loaded)
        (Survey.sum(loaded, point).pending, Survey.sum(seen, point).pending, failed)

      . assert(_ == (2, 1, Nil))

      test(m"A strict export is refused while a line is yet to be translated"):
        val store = fixture()
        edit(store, t"notes.md", t"Install it.\n")
        Worktree.checkin(store, Nil, false, false)
        val loaded = Workspace.load(store.root)
        val french = loaded.locate(loaded.space.root, List(t"fr"))
        code(Worktree.publish(loaded, french, t"${store.root}/out", true))

      . assert(_ == t"↯/o.4.11")

      test(m"A stale passage is found by its line in the view which is checked out"):
        val store = populated()
        val point = store.locate(store.space.root, List(t"fr", t"v2"))
        Worktree.checkout(store, point, false, Worktree.Mode.Beside)
        val stale = Survey.work(store, t"guide/intro.md", point).stale
        (stale.map(_.from + 1), stale.map(Worktree.line(store, t"guide/intro.md", _)))

      . assert(_ == (List(3), List(4)))

      test(m"A stale passage is found by its line in a view against a change"):
        val store = populated()
        val point = store.locate(store.space.root, List(t"fr", t"v2"))
        Worktree.checkout(store, point, false, Worktree.Mode.Against(t"release", t"v1"))
        val stale = Survey.work(store, t"guide/intro.md", point).stale
        val shown = Store.lines(Disk.read(t"${store.tree}/guide/intro.md").or(t""))
        val found = stale.map(Worktree.line(store, t"guide/intro.md", _))
        (found, found.map { line => shown.excerpt(line - 1, line).join })

      . assert(_ == (List(3), List(t"! Run it.")))

      test(m"The points at or below a value are every one which has it or a descendant of it"):
        val store = fixture()
        val both = store.within(List(t"fr", t"v2")).size
        (store.within(List(t"fr")).size, both, store.within(Nil).size)

      . assert(_ == (4, 2, 6))

      test(m"There is no store above a directory without one"):
        code(Workspace.find(java.nio.file.Files.createTempDirectory("orthant").nn.toString.nn.tt))

      . assert(_ == t"↯/o.4.1")

    // The store of the specification's example (spec/store.md §14), read as §12 says a reader
    // reads one: from memory, and from a disk.
    suite(m"Reading a store"):
      import soundness.codepages.utf8Codepage
      import soundness.filesystemBackends.javaBaseFilesystem
      import soundness.systems.javaBaseSystem

      def bytes(text: Text): Data = utf8Codepage.encoded(text)

      def config(prefer: Text = t"own", kind: Text = t"custom", second: Text = t"release"): Text =
        Store.text:
          List
            ( t"format 1", t"tree docs", t"axis", t"  name language", t"  kind language",
              t"  prefer $prefer", t"  value", t"    name en", t"  value", t"    name fr",
              t"    parent en", t"    translation true", t"  value", t"    name fr/CA",
              t"    parent fr", t"axis", t"  name $second", t"  kind $kind", t"  prefer own",
              t"  value", t"    name v1", t"  value", t"    name v2", t"    parent v1" )

      val intro = t"guide/intro.md"
      val base = List(t"Title", t"", t"Install the tool.", t"Run it.", t"", t"Last line.")

      val later =
        List(t"orthant 1 release", t"", t"- Run it.", t"+ Run it from a terminal.", t"+ New para.")

      val french =
        List
          ( t"orthant 1 language", t"", t"- Title", t"+ Titre", t"", t"- Install the tool.",
            t"- Run it.", t"+ Installez l'outil.", t"+ Lancez-le." )

      val second =
        List
          ( t"Title", t"", t"Install the tool.", t"Run it from a terminal.", t"New para.", t"",
            t"Last line." )

      val translated =
        List(t"Titre", t"", t"Installez l'outil.", t"Lancez-le.", t"", t"Last line.")

      def texts(settings: Text, delta: List[Text]): List[(Text, Text)] =
        List
          ( t".orthant/config.tel"                             -> settings,
            t".orthant/data/root/guide/intro.md"               -> Store.text(base),
            t".orthant/data/release=v2/guide/intro.md.delta"   -> Store.text(delta),
            t".orthant/data/language=fr/guide/intro.md.delta"  -> Store.text(french) )

      // A file given in `more` takes the place of the example's own of that path.
      def example
        ( settings: Text                = config(),
          more:     List[(Text, Data)]  = Nil,
          delta:    List[Text]          = later )
      :   Archive =

        Archive(more + texts(settings, delta).map { (file, text) => file -> bytes(text) })

      def disk(): Text =
        val root = java.nio.file.Files.createTempDirectory("orthant").nn.toString.nn.tt

        texts(config(), later).each: (file, text) =>
          Disk.write(t"$root/$file", text, t"$root/staging")

        root

      def code(block: => Any): Text =
        try
          block
          t""
        catch case error: Store.Error => error.bolt.text

      test(m"A store is opened from memory, and is of format 1"):
        Store.open(example()).format

      . assert(_ == 1)

      test(m"The axes are given in order, with what the configuration says of each"):
        val store = Store.open(example())
        val language = store.entry(t"language")

        ( store.axes.map(_.name), store.axes.map(_.root), language.let(_.kind),
          language.let(_.prefer), language.let(_.value.map(_.name)) )

      . assert: found =>
          found ==
            ( List(t"language", t"release"), List(t"en", t"v1"), t"language", t"own",
              List(t"en", t"fr", t"fr/CA") )

      test(m"A value has a lineage, a depth and children"):
        Store.open(example()).axes.filter(_.name == t"language").map: language =>
          (language.lineage(t"fr/CA"), language.depth(t"fr/CA"), language.children(t"en"))

      . assert(_ == List((List(t"fr/CA", t"fr", t"en"), 2, List(t"fr"))))

      test(m"Every point is given, each after the points it is below"):
        val store = Store.open(example())
        store.points.map(store.name(_))

      . assert: names =>
          names ==
            List
              ( t"root", t"release=v2", t"language=fr", t"language=fr,release=v2",
                t"language=fr_CA", t"language=fr_CA,release=v2" )

      test(m"A point is named by terms, each moving the last"):
        val store = Store.open(example())
        val first = store.point(List(t"fr", t"release=v2"))
        (store.name(first), store.name(store.point(List(t"language=en"), first)))

      . assert(_ == (t"language=fr,release=v2", t"release=v2"))

      test(m"A term which names no axis, or no value, fails"):
        val store = Store.open(example())

        ( code(store.point(List(t"edition=v2"))), code(store.point(List(t"release=v9"))),
          code(store.point(List(t"v9"))) )

      . assert(_ == (t"↯/o.2.8", t"↯/o.2.1", t"↯/o.2.8"))

      test(m"The documents are listed, each once"):
        Store.open(example()).documents

      . assert(_ == List(intro))

      test(m"The contents of the example are as the specification gives them"):
        val store = Store.open(example())

        ( store.content(intro, store.point(Nil)), store.content(intro, store.point(List(t"v2"))),
          store.content(intro, store.point(List(t"fr"))),
          store.content(intro, store.point(List(t"fr", t"v2"))) )

      . assert(_ == (base, second, translated, translated))

      test(m"An axis which prefers its parent shows the parent's lines where a hunk is stale"):
        val store = Store.open(example(config(prefer = t"parent")))
        store.content(intro, store.point(List(t"fr", t"v2")))

      . assert(_ == t"Titre" :: second.excerpt(1, 7))

      test(m"The text of a content is its bytes"):
        val store = Store.open(example())
        store.text(intro, store.point(List(t"v2"))).let(_.to[List])

      . assert(_ == bytes(Store.text(second)).to[List])

      test(m"A document which is not in the store is absent, and that is no error"):
        val store = Store.open(example())
        store.content(t"guide/other.md", store.point(List(t"fr")))

      . assert(_ == Unset)

      test(m"The example has no faults"):
        Store.open(example()).faults.map(_.bolt.text)

      . assert(_ == Nil)

      test(m"A rendition has each placement, and the issues"):
        val store = Store.open(example())

        store.rendition(intro, store.point(List(t"fr", t"v2"))).let: rendition =>
          val stale = rendition.issues.map:
            case Issue.Stale(placement) => (placement.start, placement.end)
            case _                      => (-1, -1)

          ( rendition.reference, rendition.placements.map { place => (place.start, place.end) },
            rendition.placements.map(_.fresh), stale )

      . assert(_ == (second, List((0, 1), (2, 5)), List(true, false), List((2, 5))))

      test(m"A point has a storage parent, and origins from the most specific"):
        val store = Store.open(example())
        val point = store.point(List(t"fr", t"v2"))

        ( store.storageParent(point).let(store.name(_)), store.origins(point).map(store.name(_)),
          store.storageParent(store.point(Nil)), store.origins(store.point(List(t"v2"))).size )

      . assert(_ == (t"release=v2", List(t"language=fr,release=v2", t"language=fr"), Unset, 1))

      test(m"The work of the example is as the specification gives it"):
        val store = Store.open(example())
        val point = store.point(List(t"fr", t"v2"))
        val work = store.work(intro, point)
        (work.total, work.pending, work.stale.size, work.faults, store.work(point))

      . assert(_ == (5, List(t"Last line."), 1, 0, Survey.Sum(5, 1, 1, 0)))

      test(m"Work which a point above has too is not a point's own"):
        val store = Store.open(example())
        val own = store.ownWork(intro, store.point(List(t"fr", t"v2")))
        (own.pending, own.stale.size)

      . assert(_ == (Nil, 1))

      test(m"The view of the example is as the specification gives it"):
        val store = Store.open(example())
        store.view(intro, store.point(List(t"fr", t"v2")))

      . assert: view =>
          view ==
            List
              ( t"- Title", t"+ Titre", t"", t"! Install the tool.", t"! Run it.",
                t"- Install the tool.", t"- Run it from a terminal.", t"- New para.",
                t"+ Installez l'outil.", t"+ Lancez-le.", t"", t"? Last line." )

      test(m"The regions of the example are as the specification gives them"):
        Region.between(base, second)

      . assert: regions =>
          regions ==
            List(Region(false, 0, 3, 0, 3), Region(true, 3, 4, 3, 5), Region(false, 4, 6, 5, 7))

      test(m"A file which begins with a byte order mark is not read"):
        val marked = bytes(t"\ufeff${Store.text(base)}")
        code(Store.open(example(more = List(t".orthant/data/root/guide/intro.md" -> marked))))

      . assert(_ == t"↯/o.4.5")

      test(m"A file which is not UTF-8 is not read"):
        val broken: Data = Array.unsafeFrozen(scala.Array[Byte](72, -1, -2, 10))
        code(Store.open(example(more = List(t".orthant/data/root/guide/intro.md" -> broken))))

      . assert(_ == t"↯/o.4.5")

      test(m"A configuration which is not UTF-8 is not read"):
        val broken: Data = Array.unsafeFrozen(scala.Array[Byte](72, -1, -2, 10))
        code(Store.open(example(more = List(t".orthant/config.tel" -> broken))))

      . assert(_ == t"↯/o.4.5")

      test(m"A name which the grammar does not allow is refused"):
        ( code(Store.open(example(config(second = t"re/lease")))),
          code(Store.open(example(config(second = t"root")))),
          code(Store.open(example(config(second = t"-release")))) )

      . assert(_ == (t"↯/o.5.1", t"↯/o.5.1", t"↯/o.5.1"))

      test(m"An axis of an unknown kind, or which prefers neither, is refused"):
        ( code(Store.open(example(config(kind = t"other")))),
          code(Store.open(example(config(prefer = t"neither")))) )

      . assert(_ == (t"↯/o.5.1", t"↯/o.5.1"))

      test(m"There is no store where there is no configuration"):
        code(Store.open(Archive(Nil)))

      . assert(_ == t"↯/o.4.1")

      test(m"Hidden entries are passed over at every depth"):
        val hidden =
          List
            ( t".orthant/data/root/.draft.md"       -> bytes(t"Draft.\n"),
              t".orthant/data/root/.cache/notes.md" -> bytes(t"Notes.\n"),
              t".orthant/data/.git/HEAD"            -> bytes(t"ref\n") )

        val store = Store.open(example(more = hidden))
        (store.documents, store.faults.size)

      . assert(_ == (List(intro), 0))

      test(m"A store with a directory which names no point is not opened, but is inspected"):
        val stray = List(t".orthant/data/edition=v2/guide/intro.md" -> bytes(t"Stray.\n"))
        val store = Store.inspect(example(more = stray))

        ( code(Store.open(example(more = stray))), store.faults.map(_.bolt.text),
          store.documents )

      . assert(_ == (t"↯/o.1.7", List(t"↯/o.1.7"), List(intro)))

      test(m"A store with a hunk which has no place is opened, and the hunk is a fault"):
        val lost = List(t"orthant 1 release", t"", t"- Not there.", t"+ Nor here.")
        val store = Store.open(example(delta = lost))
        val point = store.point(List(t"v2"))

        ( store.faults.map(_.bolt.text), code(store.content(intro, point)),
          store.rendition(intro, point).let(_.issues.size), store.content(intro, store.point(Nil)) )

      . assert(_ == (List(t"↯/o.1.8"), t"↯/o.1.8", 1, base))

      test(m"A store is opened from a directory, and reads as it does from memory"):
        val store = Store.open(Disk.path(disk()))
        (store.documents, store.content(intro, store.point(List(t"fr", t"v2"))), store.faults.size)

      . assert(_ == (List(intro), translated, 0))

      test(m"A symbolic link in a store is passed over"):
        val root = disk()
        val link = java.nio.file.Path.of(t"$root/.orthant/data/root/guide/link.md".s).nn
        val target = java.nio.file.Path.of(t"$root/.orthant/data/root/guide/intro.md".s).nn
        java.nio.file.Files.createSymbolicLink(link, target)
        Store.open(Disk.path(root)).documents

      . assert(_ == List(intro))

      test(m"A store is found from a directory beneath its home"):
        val root = disk()
        Disk.directory(t"$root/docs/guide")
        Store.find(Disk.path(t"$root/docs/guide")).encode == root

      . assert(_ == true)

      test(m"No store is found where there is none above"):
        val nowhere = java.nio.file.Files.createTempDirectory("orthant").nn.toString.nn.tt
        code(Store.find(Disk.path(nowhere)))

      . assert(_ == t"↯/o.4.1")

      test(m"An export writes each document, and a strict one refuses work which is wanting"):
        val root = disk()
        val store = Store.open(Disk.path(root))
        val count = store.publish(store.point(List(t"v2")), Disk.path(t"$root/out"))
        val french = store.point(List(t"fr", t"v2"))
        val strict = code(store.publish(french, Disk.path(t"$root/no"), true))
        (count, Disk.read(t"$root/out/guide/intro.md"), strict, Disk.exists(t"$root/no"))

      . assert(_ == (1, Store.text(second), t"↯/o.4.11", false))

      test(m"Reading a store writes nothing beneath it"):
        val root = disk()
        val before = Disk.files(t"$root/.orthant")
        val store = Store.open(Disk.path(root))
        store.faults
        store.publish(store.point(List(t"fr")), Disk.path(t"$root/out"))
        (Disk.files(t"$root/.orthant") == before, Disk.exists(t"$root/.orthant/work"))

      . assert(_ == (true, false))

    suite(m"Upgrading"):
      val platforms =
        t"macos-arm64\thttps://example.com/orthant-macos-arm64\tDEADBEEF\n" +
          t"linux-x64\thttps://example.com/orthant-linux-x64\t0123\n"

      val manifest: Text = t"version\t0.3.0\nbuild\t3000\nsigned-by\tabc123\n$platforms"

      test(m"A manifest's version and build are read"):
        UpgradeManifest.parse(manifest).let: release => t"${release.version} ${release.build}"

      . assert(_ == t"0.3.0 3000")

      test(m"A manifest's executable for a platform is read"):
        UpgradeManifest.parse(manifest).let(_.executable(t"macos-arm64")).let(_.sha256)

      . assert(_ == t"DEADBEEF")

      test(m"A platform which the manifest does not name has no executable"):
        UpgradeManifest.parse(manifest).let(_.executable(t"windows-x64")).absent

      . assert(_ == true)

      test(m"An empty signed-by is no signing key"):
        UpgradeManifest.parse(t"version\t0.3.0\nbuild\t3000\nsigned-by\t\n").let(_.signedBy).absent

      . assert(_ == true)

      test(m"A manifest without a build is not a release"):
        UpgradeManifest.parse(t"version\t0.3.0\n").absent

      . assert(_ == true)

      test(m"A manifest whose build is not a number is not a release"):
        UpgradeManifest.parse(t"version\t0.3.0\nbuild\tthree\n").absent

      . assert(_ == true)

      test(m"A build id stands for its version"):
        List(SelfUpgrade.version(3000), SelfUpgrade.version(1002003))

      . assert(_ == List(t"0.3.0", t"1.2.3"))

    suite(m"Following a git repository"):
      // A history: one, two and three in a line; side, beside two; and merge, of two and side.
      val one = t"1111111111111111111111111111111111111111"
      val two = t"2222222222222222222222222222222222222222"
      val three = t"3333333333333333333333333333333333333333"
      val side = t"5555555555555555555555555555555555555555"
      val merge = t"9999999999999999999999999999999999999999"

      def parents(commit: Text): List[Text] =
        if commit == two || commit == side then List(one)
        else if commit == three then List(two)
        else if commit == merge then List(two, side)
        else Nil

      def ancestor(upper: Text, lower: Text): Boolean =
        parents(lower).exists: parent => parent == upper || ancestor(upper, parent)

      def refs(tags: List[(Text, Text)], tip: Text): Sync.Refs =
        Sync.Refs(tags, List((t"main", tip)), t"main", ancestor)

      def code(block: => Any): Text =
        try
          block
          t""
        catch
          case error: Store.Error     => error.bolt.text
          case error: Workspace.Error => error.bolt.text

      def fixture(): Workspace =
        val root = java.nio.file.Files.createTempDirectory("orthant").nn.toString.nn.tt
        Workspace.init(root, t"docs").axis(t"release", t"start", t"git", t"v*")

      def values(store: Workspace): List[(Text, Text, Text)] =
        store.entry(t"release").lay(Nil)(_.value).map: value =>
          val label = value.label.or(if value.branch.or(false) then t"(branch)" else t"-")
          (value.name, value.parent.or(t"-"), label)

      val early = refs(List((t"v1", one), (t"v2", two), (t"nightly", two)), three)

      test(m"A pattern takes the tags it matches"):
        List(Sync.takes(t"v*", t"v1.0"), Sync.takes(t"v*", t"nightly"), Sync.takes(t"*", t"x"),
             Sync.takes(t"v*.0", t"v1.0"), Sync.takes(t"v*.0", t"v1.1"), Sync.takes(t"v1", t"v1"))

      . assert(_ == List(true, false, true, true, false, true))

      test(m"Tags become values beneath their nearest tagged ancestors, and the branch is tracked"):
        values(Sync(fixture(), t"release", early, Nil)(0))

      . assert: found =>
          found ==
            List
              ( (t"start", t"-", t"-"), (t"111111111111", t"start", t"v1"),
                (t"222222222222", t"111111111111", t"v2"), (t"main", t"222222222222", t"(branch)") )

      test(m"A value is found by its tag, and shown by it"):
        val store = Sync(fixture(), t"release", early, Nil)(0)
        val point = store.locate(store.space.root, List(t"v2"))
        (store.name(point), store.title(t"release", t"222222222222"))

      . assert(_ == (t"release=222222222222", t"v2"))

      test(m"A second sync with nothing new does nothing"):
        val store = Sync(fixture(), t"release", early, Nil)(0)
        Sync(store, t"release", early, Nil)(1)

      . assert(_ == Nil)

      test(m"A new tag on the branch takes what was checked in there, and the branch starts again"):
        val store = Sync(fixture(), t"release", early, Nil)(0)
        Disk.write(t"${store.tree}/notes.md", t"First.\n", store.staging)
        Worktree.checkin(store, Nil, false, false)
        val based = Workspace.load(store.root)
        Worktree.checkout(based, based.locate(based.space.root, List(t"main")), false)
        Disk.write(t"${store.tree}/notes.md", t"First.\nSecond.\n", store.staging)
        Worktree.checkin(based, Nil, false, false)
        val later = refs(List((t"v1", one), (t"v2", two), (t"v3", three)), three)
        val (synced, notes) = Sync(Workspace.load(store.root), t"release", later, Nil)
        val dossier = synced.dossier(t"notes.md")

        def at(value: Text): Optional[List[Text]] =
          dossier.content(synced.locate(synced.space.root, List(value)))

        val folders = Disk.directories(synced.data)

        ( values(synced).excerpt(3, 5), at(t"v2"), at(t"v3"), at(t"main"), folders, notes.size,
          Workspace.scan(synced.root)(1).size, Worktree.changes(synced).size )

      . assert: result =>
          result == (
            List
              ( (t"333333333333", t"222222222222", t"v3"),
                (t"main", t"333333333333", t"(branch)") ),
            List(t"First."), List(t"First.", t"Second."), List(t"First.", t"Second."),
            List(t"release=333333333333", t"root"), 2, 0, 0 )

      test(m"A sync which labels, adds and releases writes what it leaves in memory"):
        val store = Sync(fixture(), t"release", refs(List((t"v1", one)), two), Nil)(0)
        val later = refs(List((t"v1", one), (t"v1.0", one), (t"v2", two), (t"v3", three)), three)
        val (synced, notes) = Sync(store, t"release", later, Nil)
        (values(Workspace.load(store.root)) == values(synced), values(synced), notes.size)

      . assert: result =>
          result == (
            true,
            List
              ( (t"start", t"-", t"-"), (t"111111111111", t"start", t"v1 v1.0"),
                (t"222222222222", t"111111111111", t"v2"),
                (t"333333333333", t"222222222222", t"v3"),
                (t"main", t"333333333333", t"(branch)") ),
            3 )

      test(m"A tag with two nearest ancestors is refused, and may be given its parent"):
        val tags = List((t"v1", one), (t"v2", two), (t"v2-side", side))
        val store = Sync(fixture(), t"release", refs(tags, three), Nil)(0)
        val merged = refs(tags + List((t"v4", merge)), three)

        ( code(Sync(store, t"release", merged, Nil)),
          values(Sync.pin(store, t"release", t"v4", t"v2", merged)).last )

      . assert(_ == (t"↯/o.7.2", (t"999999999999", t"222222222222", t"v4")))

      test(m"Only an axis which follows the repository can be synchronized"):
        val root = java.nio.file.Files.createTempDirectory("orthant").nn.toString.nn.tt
        val store = Workspace.init(root, t"docs").axis(t"release", t"v1", t"custom")
        code(Sync(store, t"release", early, Nil))

      . assert(_ == t"↯/o.7.4")

    suite(m"Growth"):
      // Doubling the size twice multiplies a linear cost by four, and a quadratic one by
      // sixteen; anything over eight is taken to be worse than linear.
      test(m"Derivation, check-in and views grow linearly with the document"):
        Growth.cost(500)
        val small = Growth.cost(1000)
        val large = Growth.cost(4000)
        large.toDouble/small.toDouble

      . assert(_ < 8.0)

    suite(m"Invariants over random check-ins"):
      List.range(1, 41).each: seed =>
        test(m"A run of random check-ins keeps every invariant (seed $seed)"):
          Trials.trial(seed, 25)

        . assert(_ == Unset)

      List.range(1, 21).each: seed =>
        test(m"The store's laws hold along a run of random check-ins (seed $seed)"):
          Trials.lawful(seed, 20)

        . assert(_ == Unset)

      List.range(1, 41).each: seed =>
        test(m"On one axis a child is its parent with its own hunks applied (seed $seed)"):
          Solo.trial(seed, 30)

        . assert(_ == Unset)

// Runs of random check-ins over a space of three axes, each checked against the engine's
// invariants (`design/engine.md`). The lines are drawn from a tiny alphabet, so that the same
// line recurs within a document.
object Trials:
  val language =
    Axis
      ( t"language",
        List
          ( Value(t"en"), Value(t"fr", t"en", translation = true), Value(t"ca", t"fr"),
            Value(t"de", t"en", translation = true) ) )

  val release =
    Axis
      ( t"release",
        List(Value(t"v1"), Value(t"v2", t"v1"), Value(t"v3", t"v2"), Value(t"v2x", t"v2")) )

  val verbosity = Axis(t"verbosity", List(Value(t"concise"), Value(t"verbose", t"concise")))
  val space = Space(List(language, release, verbosity))
  val points = space.points
  val alphabet = List(t"a", t"b", t"c", t"d", t"")

  // A number below `bound` which depends only on the seed and the index.
  def random(seed: Int, index: Int, bound: Int): Int =
    val first = (seed*7919 + index*104729 + 17)
    val second = (first ^ (first >>> 16))*0x7feb352d
    val third = (second ^ (second >>> 15))*0x846ca68b
    ((third ^ (third >>> 16)) & 0x7fffffff)%bound

  def pick[element](list: List[element], index: Int): element =
    list.excerpt(index, index + 1).head.or(panic(m"the index is within the list"))

  def mutate(lines: List[Text], seed: Int, index: Int): List[Text] =
    val position = random(seed, index, lines.size + 1)
    val line = pick(alphabet, random(seed, index + 1, alphabet.size))

    random(seed, index + 2, 3) match
      case 0 => lines.excerpt(0, position) + List(line) + lines.excerpt(position, lines.size)
      case 1 => lines.excerpt(0, position) + lines.excerpt(position + 1, lines.size)
      case _ => lines.excerpt(0, position) + List(line) + lines.excerpt(position + 1, lines.size)

  def name(point: Point): Text = space.axes.map(point(_)).join(t",")

  // The check-in a run makes at a step: where, what, and within which scope.
  case class Step(point: Point, lines: List[Text], scope: Point => Boolean, label: Text)

  def plan(dossier: Dossier, seed: Int, step: Int): Step =
    val point = pick(points, random(seed, step*10, points.size))
    val axis = pick(space.axes, random(seed, step*10 + 1, space.axes.size))
    val only = random(seed, step*10 + 2, 3) == 0
    val edits = 1 + random(seed, step*10 + 3, 2)

    val lines = List.range(0, edits).fold(dossier.content(point).or(Nil)): (lines, edit) =>
      mutate(lines, seed, step*10 + 4 + edit*3)

    Step
      ( point, lines, other => !only || other(axis) == point(axis),
        t"step $step, ${name(point)}${if only then t" only ${axis.name}" else t""}" )

  // The first invariant which the check-in at a step breaks, and the dossier after it.
  def check(dossier: Dossier, seed: Int, step: Int): (Dossier, Optional[Text]) =
    val Step(point, lines, scope, label) = plan(dossier, seed, step)

    val outcome: Dossier | Text =
      try dossier.checkin(point, lines, scope)
      catch case error: Checkin.Error =>
        t"$label: at ${name(error.point)}, ${error.message.text}"

    outcome match
      case refusal: Text =>
        (dossier, refusal)

      case after: Dossier =>
        val moved = points.filter: other =>
          after.content(other) != dossier.content(other) &&
            !(space.below(other, point) && scope(other))

        val unwritten = after.folios.values.exists:
          case delta: Folio.Delta => Delta.parse(Delta.serialize(delta)) != delta
          case _                  => false

        val failure: Optional[Text] =
          if after.content(point) != lines then t"$label: the content is not the lines"
          else if !moved.nil then t"$label: a point outside the scope changed"
          else if unwritten then t"$label: a delta is not written as it is read"
          else if after.checkin(point, lines, scope).folios != after.folios
          then t"$label: checking in the same content changed the store"
          else Unset

        (after, failure)

  val start: Dossier = Dossier(space).checkin(space.root, List(t"a", t"b", t"", t"c", t"d"))

  private def first(failures: List[Optional[Text]]): Optional[Text] =
    failures.filter(_.present).head.or(Unset)

  // A dossier in another space, holding what `dossier` holds, at the point each point becomes.
  private def moved(dossier: Dossier, space: Space, become: Point => Point): Dossier =
    points.fold(Dossier(space)): (result, point) =>
      result.hold(become(point), dossier.folios.at(point))

  // Invariant 5: the order of the hunks in a delta does not matter.
  def unordered(dossier: Dossier): Optional[Text] =
    val reversed = points.fold(dossier): (result, point) =>
      dossier.folios.at(point) match
        case Folio.Delta(axis, hunks) => result.hold(point, Folio.Delta(axis, hunks.reverse))
        case _                        => result

    first:
      points.map: point =>
        if reversed.content(point) == dossier.content(point) then Unset
        else t"reversing the hunks changed ${name(point)}"

  // Invariant 6: a value may be renamed, and an empty value put between a value and its parent.
  def renamed(dossier: Dossier): Optional[Text] =
    val language2 =
      Axis
        ( t"language",
          List
            ( Value(t"en"), Value(t"fra", t"en", translation = true), Value(t"ca", t"fra"),
              Value(t"de", t"en", translation = true) ) )

    def become(point: Point): Point =
      if point(language) == t"fr" then point.move(language2, t"fra") else point

    val other = moved(dossier, Space(List(language2, release, verbosity)), become)

    first:
      points.map: point =>
        if other.content(become(point)) == dossier.content(point) then Unset
        else t"renaming a value changed ${name(point)}"

  def inserted(dossier: Dossier): Optional[Text] =
    val language3 =
      Axis
        ( t"language",
          List
            ( Value(t"en"), Value(t"mid", t"en"), Value(t"fr", t"mid", translation = true),
              Value(t"ca", t"fr"), Value(t"de", t"en", translation = true) ) )

    val release3 =
      Axis
        ( t"release",
          List
            ( Value(t"v1"), Value(t"v2", t"v1"), Value(t"v2a", t"v2"), Value(t"v3", t"v2a"),
              Value(t"v2x", t"v2") ) )

    val other = moved(dossier, Space(List(language3, release3, verbosity)), point => point)

    first:
      points.bind: point =>
        val between =
          if point(release) == t"v2" then other.content(point.move(release3, t"v2a"))
          else if point(language) == t"en" then other.content(point.move(language3, t"mid"))
          else dossier.content(point)

        List
          ( if other.content(point) == dossier.content(point) then Unset
            else t"inserting a value changed ${name(point)}",
            if between == dossier.content(point) then Unset
            else t"an empty value above ${name(point)} does not have its content" )

  // Invariant 7: hunks re-based onto the content they are already in are as they were.
  def rebased(dossier: Dossier): Optional[Text] = first:
    points.map: point =>
      val parent = space.storageParent(point).let(dossier.content(_))

      dossier.folios.at(point) match
        case Folio.Delta(_, hunks) =>
          parent.let: content =>
            val located = hunks.bind: hunk =>
              Rebase.occurrences(hunk, content).map: start => (start, hunk)

            val outcomes = Rebase(located, content, content)

            val same = outcomes.size == located.size && located.fold(true): (same, pair) =>
              same && outcomes.has(Rebase.Outcome.Fresh(pair(0), pair(1)))

            if same then Unset else t"re-basing onto the same content changed ${name(point)}"

        case _ =>
          Unset

  private def stale(dossier: Dossier, point: Point): List[Placement] =
    dossier.rendition(point).let(_.placements.filter(!_.fresh)).or(Nil)

  private def shape(dossier: Dossier, point: Point): List[(Int, Int, List[Text])] =
    dossier.rendition(point).let(_.placements).or(Nil).map: placement =>
      (placement.start, placement.end, placement.lines)

  // Invariant 8: a fresh hunk over a stale one clears it; the point's content, its other hunks
  // and every other point which does not inherit from it are as they were.
  def cleared(dossier: Dossier): Optional[Text] = first:
    points.bind: point =>
      stale(dossier, point).map: placement =>
        val checked = dossier.confirm(point, _ == placement)

        val elsewhere = points.filter: other =>
          !space.below(other, point) && checked.content(other) != dossier.content(other)

        if stale(checked, point).size != stale(dossier, point).size - 1
        then t"a fresh hunk over a stale one at ${name(point)} did not clear just that one"
        else if checked.content(point) != dossier.content(point)
        then t"a fresh hunk over a stale one changed the content of ${name(point)}"
        else if shape(checked, point) != shape(dossier, point)
        then t"a fresh hunk over a stale one moved another hunk of ${name(point)}"
        else if !elsewhere.nil
        then t"a fresh hunk over a stale one at ${name(point)} changed another point"
        else Unset

  // The first of invariants 5 to 8 which a run of check-ins breaks; they are looked for after
  // every fifth step.
  def lawful(seed: Int, steps: Int): Optional[Text] =
    List.range(0, steps).fold((start, Unset: Optional[Text])): (state, step) =>
      if state(1).present then state else
        val after = check(state(0), seed, step)(0)

        def laws =
          List(unordered(after), renamed(after), inserted(after), rebased(after), cleared(after))

        val failure = if step%5 != 4 then Unset else first(laws)

        (after, failure.let { failure => t"after step $step: $failure" })

    . apply(1)

  // The first invariant which a run of check-ins breaks, or `Unset` if none does.
  def trial(seed: Int, steps: Int): Optional[Text] =
    List.range(0, steps).fold((start, Unset: Optional[Text])): (state, step) =>
      if state(1).present then state else check(state(0), seed, step)

    . apply(1)

// Invariant 9: runs of random check-ins on a single axis, where the content of a point can be
// worked out without the engine, from its parent's content and the hunks it holds.
object Solo:
  import Trials.{mutate, pick, random}

  val release =
    Axis
      ( t"release",
        List
          ( Value(t"v1"), Value(t"v2", t"v1"), Value(t"v3", t"v2"), Value(t"v2x", t"v2"),
            Value(t"v4", t"v3") ) )

  val space = Space(List(release))
  val points = space.points
  val start: Dossier = Dossier(space).checkin(space.root, List(t"a", t"b", t"", t"c", t"d"))

  def run(seed: Int, steps: Int): Dossier = List.range(0, steps).fold(start): (dossier, step) =>
    val point = pick(points, random(seed, step*10, points.size))
    val lines = mutate(dossier.content(point).or(Nil), seed, step*10 + 4)
    try dossier.checkin(point, lines) catch case error: Checkin.Error => dossier

  // The content of a point, or the reason it cannot be worked out.
  def applied(dossier: Dossier, point: Point): Optional[List[Text]] | Text =
    val parent: Optional[List[Text]] | Text =
      space.storageParent(point).let(applied(dossier, _))

    dossier.folios.at(point) match
      case Folio.Base(lines) => lines
      case Folio.Absent(_)   => Unset

      case Folio.Delta(_, hunks) => parent match
        case content: List[Text] @unchecked =>
          val located = hunks.bind: hunk =>
            Rebase.occurrences(hunk, content).map: start => (start, hunk)

          if located.size != hunks.size then t"a hunk of ${point(release)} is not there once"
          else
            // From the end backwards, so that no hunk moves the lines of one yet to be applied.
            located.order { (start, hunk) => -hunk.rank(start) }.fold(content): (content, pair) =>
              val (start, hunk) = pair

              content.excerpt(0, start) + hunk.lines +
                content.excerpt(start + hunk.replaced.size, content.size)

        case _ =>
          t"${point(release)} has hunks and no parent content"

      case _ =>
        parent

  def trial(seed: Int, steps: Int): Optional[Text] =
    val dossier = run(seed, steps)

    points.map: point =>
      applied(dossier, point) match
        case content if content == dossier.content(point) => Unset
        case reason: Text @unchecked                      => t"seed $seed: $reason"

        case _ =>
          t"seed $seed: ${point(release)} differs"

    . filter(_.present).head.or(Unset)
