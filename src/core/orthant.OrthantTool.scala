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

// `soundness` re-exports Proscenium's collections, but an exported opaque type does not carry its
// companion's extensions into implicit scope, so the collection types come straight from
// Proscenium, as the Soundness modules themselves do.
import proscenium.{List, Nil, `::`}
import soundness.*
import soundness.backstops.stackTraceBackstop
import soundness.dysasymptotics.linearSize
import soundness.errorDiagnostics.emptyDiagnostics
import soundness.executives.completionsExecutive
import soundness.interpreters.posixInterpreter
import soundness.systems.javaBaseSystem
import soundness.threads.virtualThreads

// The command-line interface. Ethereal serves tab-completions by re-running this dispatch in
// completion mode, where the `execute` blocks are never run, so everything the shell can offer —
// every flag and every suggestion — is read outside `execute`.
object OrthantTool:
  private val StoreGroup = CommandGroup(t"The store")
  private val TreeGroup = CommandGroup(t"The working tree")
  private val WorkGroup = CommandGroup(t"The work to do")
  private val ToolGroup = CommandGroup(t"The tool")

  private val TodoCommand =
    Subcommand
      ( t"todo", t"list what is to be translated or checked, where best to do it: [<value>…]",
        group = WorkGroup )

  private val GridCommand =
    Subcommand
      ( t"grid", t"show the work at every pair of values of two axes: [<axis> [<axis>]]",
        group = WorkGroup )

  private val InitCommand =
    Subcommand
      ( t"init", t"create a store here, with its working tree in <directory>",
        group = StoreGroup )

  private val AxisCommand =
    Subcommand(t"axis", t"list the axes, or add an axis or a value", group = StoreGroup)

  private val AddCommand = Subcommand(t"add", t"add an axis: <axis> <root value> [<tag pattern>]")
  private val ValueCommand = Subcommand(t"value", t"add a value: <axis> <value> [<parent>]")

  private val PreferCommand =
    Subcommand(t"prefer", t"choose what a stale hunk shows: <axis> own|parent")

  private val ExportCommand =
    Subcommand
      ( t"export", t"write the plain documents at a point: <directory> [<value>…]",
        group = StoreGroup )

  private val ConfirmCommand =
    Subcommand
      ( t"confirm", t"mark stale passages as checked, unchanged: [<path>…]",
        group = TreeGroup )

  private val SyncCommand =
    Subcommand
      ( t"sync", t"take up the repository's new tags as values: [<branch>…]",
        group = StoreGroup )

  private val RefCommand =
    Subcommand
      ( t"ref", t"change what each document is shown beside: --none, --parent or <from>[..<to>]",
        group = TreeGroup )

  private val UpdateCommand =
    Subcommand
      ( t"update", t"bring the working tree up to a store which has changed beneath it",
        group = TreeGroup )

  private val DiffCommand =
    Subcommand(t"diff", t"show the edits which are not checked in: [<path>…]", group = TreeGroup)

  private val InstallCommand =
    Subcommand(t"install", t"install tab-completions into the shell", group = ToolGroup)

  // `upgrade` is always accepted, but offered only while a newer release is known: its hidden
  // twin is what the dispatch matches otherwise (see `SelfUpgrade`).
  private val upgradeText: Text = t"replace this executable with the newest release"
  private val UpgradeCommand = Subcommand(t"upgrade", upgradeText, group = ToolGroup)

  private val HiddenUpgrade =
    Subcommand(t"upgrade", upgradeText, group = ToolGroup, hidden = true)

  private val CheckCommand =
    Subcommand(t"check", t"verify everything the store holds", group = StoreGroup)

  private val CatCommand =
    Subcommand(t"cat", t"print a document: <path> [<value>…]", group = StoreGroup)

  private val CheckoutCommand =
    Subcommand
      ( t"checkout", t"make the working tree the documents at the values given",
        group = TreeGroup )

  private val StatusCommand =
    Subcommand
      ( t"status", t"show what is not checked in, and what wants attention: [<value>…]",
        group = TreeGroup )

  private val CheckinCommand =
    Subcommand(t"checkin", t"store the edited documents: [<path>…]", group = TreeGroup)

  private val HelpFlag =
    Flag[Unit](t"help", false, List('h'), t"describe the available subcommands and options")

  private val LanguageFlag =
    Flag[Unit](t"language", false, Nil, t"the axis's values are languages and their dialects")

  private val GitFlag =
    Flag[Unit](t"git", false, Nil, t"the axis's values are the repository's tags and branches")

  private val ForceFlag =
    Flag[Unit](t"force", false, List('f'), t"discard the edits which are not checked in")

  private val IndirectFlag =
    Flag[Unit]
      ( t"indirect", false, List('i'),
        t"show each document beside its storage parent's, every line with a sigil" )

  private val DirectFlag =
    Flag[Unit](t"direct", false, List('d'), t"show each document as its plain content")

  private val NoneFlag =
    Flag[Unit](t"none", false, Nil, t"show each document alone, as its plain content")

  private val ParentFlag =
    Flag[Unit](t"parent", false, Nil, t"show each document beside its storage parent's")

  private val OnlyFlag =
    Flag[Text](t"only", false, Nil, t"reach only the points with this point's value on the axis")

  private val SinceFlag =
    Flag[Text]
      ( t"since", false, Nil, t"store the edit at this ancestor value, and reach all below it" )

  private val OriginFlag =
    Flag[Text]
      ( t"origin", false, Nil,
        t"store each change at the value of the axis which first had the lines it changes" )

  private val valueFlags: List[Flag] = List(OnlyFlag, SinceFlag, OriginFlag)

  private val StrictFlag =
    Flag[Unit]
      ( t"strict", false, Nil,
        t"refuse while any document has stale, conflicting or untranslated lines" )

  private val TakeFlag =
    Flag[Unit]
      ( t"checkout", false, List('c'),
        t"check out the first point listed, in the view which shows its work" )

  private val RemoveFlag =
    Flag[Unit](t"remove", false, Nil, t"delete, at this point, the documents which are missing")

  private val DryRunFlag =
    Flag[Unit](t"dry-run", false, List('n'), t"say what would be stored, and store nothing")

  private val AllFlag =
    Flag[Unit](t"all", false, List('a'), t"report on every point, not only the one checked out")

  object UsageError extends Status(1, t"the command was not invoked correctly")
  object Refused extends Status(2, t"the store or the working tree did not allow the command")

  // `arguments` carries flags alongside operands, so the operands are those which do not look
  // like flags.
  private def operands(arguments: List[Argument]): List[Argument] =
    arguments.filter(!_().starts(t"-"))

  private type Posix = Interpreter { type Topic = Commandline }

  // The interpreter gives a flag every argument which follows it, up to the next flag. Each of
  // Orthant's flags which takes a value takes one, so the rest are the command's operands.
  private def surplus(using cli: Cli, interpreter: Posix): List[Argument] =
    val commandline = interpreter.interpret(cli.arguments)
    valueFlags.bind: flag => List.from(interpreter.find(commandline, flag).stdlib.toList.drop(1))

  // The value of a flag which takes one. Reading it registers the flag, which is what puts it
  // among the command's completions, so it is read outside `execute`.
  private def valued(flag: Flag of Text, label: Text)(using cli: Cli, interpreter: Posix)
  :   Optional[Text] =

    given (Text is Interpretable) = new Interpretable:
      type Self = Text
      override def placeholder: Optional[Text] = label

      def interpret(arguments: List[Argument]): Optional[Text] =
        arguments.stdlib.headOption.map(_()).getOrElse(Unset)

    flag().value.or:
      val commandline = interpreter.interpret(cli.arguments)
      interpreter.find(commandline, flag).stdlib.toList.headOption.map(_()).getOrElse(Unset)

  private given Text is Suggestible = Suggestion(_)

  // The values a point may be moved to, for the shell to offer; none where there is no store.
  private def values(using Cli): List[Text] =
    safely(Workspace.settings(Workspace.find(here))).lay(Nil): settings =>
      settings.axis.bind(_.value).bind: value => value.name :: value.labels

  // The command dispatch. The `@main` entry point and burdock's `externalize` wrapper live alone
  // in the `launcher` module, which depends on this module as a published coordinate.
  def run(): Unit = cli:
    val help = HelpFlag().present
    val upgrade = if SelfUpgrade.available.present then UpgradeCommand else HiddenUpgrade

    // What the launcher did with an upgrade staged earlier, and the daily check for a newer
    // release: for a command which is run, and never for a tab-completion.
    summon[Cli] match
      case _: Invocation =>
        SelfUpgrade.report()
        SelfUpgrade.check()

      case _ =>
        ()

    arguments match
      case _ if help =>
        val topic = operands(arguments).head.let(_())

        execute:
          topic.let(manual(_)).or(Nil) match
            case Nil   => Out.println(resident.help())
            case lines => lines.each(Out.println(_))

          Exit.Ok

      case InitCommand() :: rest =>
        execute(attempt(init(operands(rest).map(_()))))

      case AxisCommand() :: AddCommand() :: rest =>
        val language = LanguageFlag().present
        val git = GitFlag().present
        val kind = if git then t"git" else if language then t"language" else t"custom"

        execute:
          operands(rest).map(_()) match
            case axis :: value :: Nil =>
              attempt(addAxis(axis, value, kind, Unset))

            case axis :: value :: pattern :: Nil if git =>
              attempt(addAxis(axis, value, kind, pattern))

            case _ =>
              usage(t"orthant axis add <axis> <root value> [--language | --git [<tag pattern>]]")

      case AxisCommand() :: ValueCommand() :: rest =>
        execute:
          operands(rest).map(_()) match
            case axis :: value :: Nil           => attempt(addValue(axis, value, Unset))
            case axis :: value :: parent :: Nil => attempt(addValue(axis, value, parent))
            case _                              => usage(valueSynopsis)

      case AxisCommand() :: PreferCommand() :: rest =>
        execute:
          operands(rest).map(_()) match
            case axis :: t"own" :: Nil    => attempt(prefer(axis, false))
            case axis :: t"parent" :: Nil => attempt(prefer(axis, true))
            case _                        => usage(t"orthant axis prefer <axis> own|parent")

      case AxisCommand() :: _ =>
        execute(attempt(axes()))

      case SyncCommand() :: rest =>
        execute(attempt(sync(operands(rest).map(_()))))

      case RefCommand() :: rest =>
        val none = NoneFlag().present
        val parent = ParentFlag().present

        execute:
          operands(rest).map(_()) match
            case Nil if none != parent => attempt(refer(Unset, parent))
            case change :: Nil         => attempt(refer(change, false))
            case _                     => usage(t"orthant ref --none | --parent | <from>[..<to>]")

      case UpdateCommand() :: _ =>
        execute(attempt(update()))

      case DiffCommand() :: rest =>
        execute(attempt(diff(operands(rest).map(_()))))

      case InstallCommand() :: _ =>
        execute(install())

      case upgrade() :: _ =>
        execute(SelfUpgrade.upgrade())

      case ExportCommand() :: rest =>
        val strict = StrictFlag().present

        execute:
          operands(rest).map(_()) match
            case directory :: values => attempt(publish(directory, values, strict))
            case _                   => usage(t"orthant export <directory> [<value>…] [--strict]")

      case ConfirmCommand() :: rest =>
        execute(attempt(confirm(operands(rest).map(_()))))

      case CheckCommand() :: _ =>
        execute(attempt(check()))

      case CatCommand() :: rest =>
        execute:
          operands(rest).map(_()) match
            case path :: values => attempt(cat(path, values))
            case _              => usage(t"orthant cat <path> [<value>…]")

      case CheckoutCommand() :: rest =>
        val force = ForceFlag().present
        val indirect = IndirectFlag().present
        val direct = DirectFlag().present
        val offered = values
        operands(rest).each(_.select(offered))
        execute(attempt(checkout(operands(rest).map(_()), force, indirect, direct)))

      case StatusCommand() :: rest =>
        val all = AllFlag().present
        val offered = values
        operands(rest).each(_.select(offered))
        execute(attempt(status(operands(rest).map(_()), all)))

      case TodoCommand() :: rest =>
        val take = TakeFlag().present
        val offered = values
        operands(rest).each(_.select(offered))
        execute(attempt(todo(operands(rest).map(_()), take)))

      case GridCommand() :: rest =>
        val found = safely(Workspace.settings(Workspace.find(here)))
        val offered = found.lay(Nil)(_.axis.map(_.name))
        operands(rest).each(_.select(offered))
        execute(attempt(grid(operands(rest).map(_()))))

      case CheckinCommand() :: rest =>
        val remove = RemoveFlag().present
        val dry = DryRunFlag().present
        val only = valued(OnlyFlag, t"axis")
        val since = valued(SinceFlag, t"value")
        val origin = valued(OriginFlag, t"axis")
        val taken = List(only, since, origin).sweep { case value: Text => value }
        val extra = surplus

        // The paths are the operands which are not the value of a flag.
        def paths: List[Text] =
          (operands(rest).filter { argument => !taken.has(argument()) } + extra).map(_()).distinct

        val reach: Optional[Worktree.Reach] = taken match
          case Nil      => Worktree.Reach.Below

          case _ :: Nil =>
            only.let(Worktree.Reach.Only(_)).or(since.let(Worktree.Reach.Since(_)))
            . or(origin.let(Worktree.Reach.Origin(_)))

          case _        => Unset

        execute:
          def run(reach: Worktree.Reach): Refused.type | UsageError.type | Exit =
            attempt(checkin(paths, remove, dry, reach))

          reach.lay(usage(checkinSynopsis))(run(_))

      case Nil =>
        execute:
          Out.println(resident.help())
          Exit.Ok

      case _ =>
        execute:
          Err.println(t"orthant: ${Orthant.bolt(6, 1).label} unrecognised command")
          Out.println(resident.help())
          UsageError

  private val valueSynopsis: Text = t"orthant axis value <axis> <value> [<parent>]"

  private val checkinSynopsis: Text =
    t"orthant checkin [<path>…] [--only <axis> | --since <value> | --origin <axis>]"

  // What `--help` says of a command: how it is written, and what it does.
  private def manual(command: Text): List[Text] = command match
    case t"init" =>
      List
        ( t"orthant init [<directory>]",
          t"",
          t"Creates a store in .orthant/ here, whose working tree is <directory>, or docs/. The",
          t"working tree is added to .gitignore; .orthant/ is to be committed." )

    case t"axis" =>
      List
        ( t"orthant axis",
          t"orthant axis add <axis> <root value> [--language | --git [<tag pattern>]]",
          t"orthant axis value <axis> <value> [<parent>]",
          t"orthant axis prefer <axis> own|parent",
          t"",
          t"Lists the axes and their values; adds an axis, or a value beneath another; or chooses",
          t"whether a stale passage on an axis shows its own lines or those it would replace.",
          t"On a language axis, a value with another language code translates its parent." )

    case t"sync" =>
      List
        ( t"orthant sync [<branch>…]",
          t"",
          t"On each axis which follows the git repository, takes up the tags which are new, and",
          t"keeps a value for what is not yet released on the current branch, or those named." )

    case t"checkout" =>
      List
        ( t"orthant checkout <value>… [--indirect | --direct] [--force]",
          t"",
          t"Makes the working tree the documents at the values given; an axis which is not named",
          t"stays at its value. --indirect shows each document beside the one it derives from,",
          t"every line with a sigil; --direct shows it plain. Edits which are not checked in",
          t"prevent it, unless --force discards them." )

    case t"status" =>
      List
        ( t"orthant status [<value>…] [--all]",
          t"",
          t"Shows the edits which are not checked in, and at the point checked out the lines yet",
          t"to be translated and the passages which are stale, counting those edits. With values,",
          t"it reports on every point at or below them, as `orthant status fr`; with --all, on",
          t"every point. A point's work which the points above it share is counted, not listed." )

    case t"todo" =>
      List
        ( t"orthant todo [<value>…] [--checkout]",
          t"",
          t"Lists what is to be translated or checked at the points at or below the values given,",
          t"each piece of work at the earliest point which has it, where doing it does it for the",
          t"points below. --checkout checks out the first point listed, in the view which shows",
          t"its work: against the change which brought it, or else beside its reference." )

    case t"grid" =>
      List
        ( t"orthant grid [<axis> [<axis>]]",
          t"",
          t"Shows a cell for every pair of values of two axes, the first two unless they are",
          t"named, with the other axes at the values checked out. A cell gives how much is",
          t"translated, then the lines yet to be (?), the stale passages (!) and the hunks which",
          t"conflict or have no place (x), and is coloured by the worst of them." )

    case t"checkin" =>
      List
        ( t"orthant checkin [<path>…] [--only <axis> | --since <value> | --origin <axis>]",
          t"                [--remove] [--dry-run]",
          t"",
          t"Stores the edited documents at the point checked out, so that every point below it",
          t"follows. --only keeps the edit to the points with this point's value on an axis;",
          t"--since stores it at an earlier value; --origin stores each change where its lines",
          t"first appeared. It then says how the work at each point has changed. --dry-run",
          t"stores nothing, and names the points whose content would change.",
          t"",
          t"In an indirect view, translate a `? ` line by adding `+ ` lines beneath it; a `- `",
          t"line with no `+ ` line is left out; delete a passage's `! ` lines once it is right." )

    case t"confirm" =>
      List
        ( t"orthant confirm [<path>…]",
          t"",
          t"Marks the stale passages at the point checked out as right as they are." )

    case t"ref" =>
      List
        ( t"orthant ref --none | --parent | <from>[..<to>]",
          t"",
          t"Changes how the working tree shows each document, keeping the edits in it: alone, as",
          t"plain text; beside the document it derives from; or against the change to that from",
          t"an earlier value, where only what the change calls for is marked." )

    case t"update" =>
      List
        ( t"orthant update",
          t"",
          t"Brings the working tree up to a store which has changed beneath it, as after a pull,",
          t"carrying the edits which are not checked in onto the documents as they now are." )

    case t"diff" =>
      List
        ( t"orthant diff [<path>…]",
          t"",
          t"Shows the edits which are not checked in: the lines of the working tree which differ",
          t"from what was checked out, sigils and all in an indirect view." )

    case t"cat" =>
      List
        ( t"orthant cat <path> [<value>…]",
          t"",
          t"Prints a document as it is at a point, which is the point checked out moved to the",
          t"values given." )

    case t"export" =>
      List
        ( t"orthant export <directory> [<value>…] [--strict]",
          t"",
          t"Writes the plain documents at a point beneath a directory. --strict refuses while",
          t"any of them has stale, conflicting or untranslated lines." )

    case t"check" =>
      List
        ( t"orthant check",
          t"",
          t"Verifies everything the store holds." )

    case t"install" =>
      List
        ( t"orthant install",
          t"",
          t"Installs tab-completions for the shells it finds." )

    case t"upgrade" =>
      List
        ( t"orthant upgrade",
          t"",
          t"Downloads the newest release, if it is newer than this one, and checks it. It takes",
          t"the place of this executable when orthant next runs, if its signature is one which",
          t"this executable accepts." )

    case _ =>
      Nil

  private def usage(synopsis: Text)(using Stdio): UsageError.type =
    Err.println(t"orthant: ${Orthant.bolt(6, 2).label} usage: $synopsis")
    UsageError

  private type Raising[result] =
    (Tactic[Store.Error], Tactic[Workspace.Error], Tactic[Axis.Error]) ?=> result

  // Every command ends here: an error which the store, the axes or a check-in raises is
  // reported with its bolt code, and the command is refused.
  private def attempt(block: Raising[Refused.type | UsageError.type | Exit])(using Stdio)
  :   Refused.type | UsageError.type | Exit =

    def refuse(bolt: fulminate.Bolt, message: Text): Refused.type =
      Err.println(t"orthant: ${bolt.label} $message")
      Refused

    recover:
      case error: Store.Error     => refuse(error.bolt, Orthant.prose(error))
      case error: Workspace.Error => refuse(error.bolt, Orthant.prose(error))
      case error: Axis.Error      => refuse(error.bolt, Orthant.prose(error))

    . protect(block)

  // The directory the command was run in: the client's, not the daemon's.
  private def here(using cli: Cli): Text = cli.workingDirectory.directory()

  private def open()(using Cli): Workspace raises Store.Error =
    Workspace.load(Workspace.find(here))

  // A point as it is shown: the value on every axis.
  private def describe(store: Workspace, point: Point): Text =
    if store.space.axes.nil then t"the only point"
    else
      store.space.axes.map: axis => t"${axis.name}=${store.title(axis.name, point(axis))}"
      . join(t" ")

  private def documents(count: Int): Text =
    if count == 1 then t"1 document" else t"$count documents"

  private def init(operands: List[Text])(using Cli, Stdio): Raising[Exit] =
    val tree = operands.head.or(t"docs")
    val store = Workspace.init(here, tree)
    Disk.directory(store.tree)
    Out.println(t"Created an Orthant store in ${store.folder}; its working tree is $tree/")
    Exit.Ok

  private def addAxis(axis: Text, value: Text, kind: Text, pattern: Optional[Text])
    ( using Cli, Stdio )
  :   Raising[Exit] =

    val store = open()

    Disk.locked(store.lock):
      store.axis(axis, value, kind, pattern)
      Out.println(t"Added the axis $axis, whose root is $value")
      if kind == t"git" then Out.println(t"Run `orthant sync` to take up the repository's tags.")
      Exit.Ok

  // Every axis which follows the repository takes up its new tags; `branches` are tracked.
  private def sync(branches: List[Text])(using Cli, Stdio): Raising[Exit] =
    val store = open()

    Disk.locked(store.lock):
      val axes = store.settings.axis.filter(_.kind == t"git").map(_.name)

      Sync.reading(store.root): refs =>
        axes.fold(store): (store, axis) =>
          val (synced, notes) = Sync(store, axis, refs, branches)

          notes.each: note => Out.println(t"  $axis: $note")

          if notes.nil then Out.println(t"  $axis: nothing new")
          synced

      val advice = t"add one with `orthant axis add <axis> <root> --git`"
      if axes.nil then Out.println(t"No axis follows the repository; $advice.")
      Exit.Ok

  private def addValue(axis: Text, value: Text, parent: Optional[Text])(using Cli, Stdio)
  :   Raising[Exit] =

    val store = open()

    val git = store.entry(axis).let(_.kind == t"git").or(false)

    Disk.locked(store.lock):
      // On an axis which follows the repository, a value with a parent is a tag.
      val updated = parent.lay(store.value(axis, value, parent)): above =>
        if git then Sync.reading(store.root)(Sync.pin(store, axis, value, above, _))
        else store.value(axis, value, parent)

      val name = updated.named(axis, value).or(value)
      val found = updated.space.axes.filter(_.name == axis).head
      val above = found.let(_.value(name).parent).let(updated.title(axis, _)).or(t"")
      val translation = found.let(_.value(name).translation).or(false)
      val edge = if translation then t"a translation" else t"a variation"
      Out.println(t"Added the value $value to the axis $axis, as $edge of $above")
      Exit.Ok

  private def axes()(using Cli, Stdio): Raising[Exit] =
    val store = open()

    store.settings.axis.each: entry =>
      Out.println(t"${entry.name} (${entry.kind}, prefers ${entry.prefer})")

      store.space.axes.filter(_.name == entry.name).each: axis =>
        def show(value: Text, depth: Int): Unit =
          val indent = List.range(0, depth).map { level => t"  " }.join
          val detail = entry.value.filter(_.name == value).head
          val labels = detail.lay(Nil)(_.labels)
          val branch = detail.let(_.branch).or(false)

          val note =
            if axis.value(value).translation then t"  (translation)"
            else if branch then t"  (unreleased)"
            else if labels.nil then t""
            else t"  ${labels.join(t", ")}"

          Out.println(t"$indent$value$note")
          axis.children(value).each(show(_, depth + 1))

        show(axis.root, 1)

    if store.settings.axis.nil
    then Out.println(t"There are no axes; add one with `orthant axis add`.")

    Exit.Ok

  private def check()(using Cli, Stdio): Raising[Refused.type | Exit] =
    val (store, faults) = Workspace.scan(Workspace.find(here))

    faults.each: fault => Err.println(t"orthant: ${fault.bolt.label} ${Orthant.prose(fault)}")

    if faults.nil then
      val points = store.space.points.size
      Out.println(t"The store is sound: ${documents(store.documents.size)} over $points points")
      Exit.Ok
    else
      Refused

  private def cat(path: Text, values: List[Text])(using Cli, Stdio): Raising[Refused.type | Exit] =
    val store = open()
    val point = store.locate(Worktree.view(store), values)

    store.dossier(document(store, path)).content(point).lay(absent(path, describe(store, point))):
      lines =>
        Out.print(Store.text(lines))
        Exit.Ok

  private def absent(path: Text, place: Text)(using Stdio): Refused.type =
    Err.println(t"orthant: ${Orthant.bolt(4, 6).label} there is no document $path at $place")
    Refused

  // The document a path names: a path within the working tree, relative to the directory the
  // command was run in, or else relative to the working tree.
  private def document(store: Workspace, path: Text)(using Cli): Text =
    val full = if path.starts(t"/") then path else t"$here/$path"
    val prefix = t"${store.tree}/"
    if full.starts(prefix) then full.skip(prefix.length) else path

  // The mode of the view stays as it is unless one is asked for.
  private def checkout(values: List[Text], force: Boolean, indirect: Boolean, direct: Boolean)
    ( using Cli, Stdio )
  :   Raising[Exit] =

    val store = open()

    if values.nil && !force && !indirect && !direct then
      val point = Worktree.view(store)
      Out.println(t"At ${describe(store, point)}${shown(store, Worktree.mode(store))}")
      val hint = t"`orthant axis` lists them"
      Out.println(t"Name the values to check out, as in `orthant checkout <value>…`; $hint.")
      Exit.Ok

    else
      Disk.locked(store.lock):
        val point = store.locate(Worktree.view(store), values)

        val mode =
          if direct then Worktree.Mode.Plain else if indirect then Worktree.Mode.Beside
          else Worktree.mode(store) match
            // A view against a change is of one point; another point is shown beside its parent.
            case Worktree.Mode.Against(_, _) if !values.nil => Worktree.Mode.Beside
            case mode                                       => mode

        val count = Worktree.checkout(store, point, force, mode)
        val place = t"${describe(store, point)}${shown(store, mode)}"
        Out.println(t"Checked out ${documents(count)} at $place")
        Exit.Ok

  private def shown(store: Workspace, mode: Worktree.Mode): Text = mode match
    case Worktree.Mode.Plain  => t""
    case Worktree.Mode.Beside => t", as indirect views"

    case Worktree.Mode.Against(axis, value) =>
      t", as views against the change from ${store.title(axis, value)} on the axis $axis"

  // `change` is `<from>` or `<from>..<to>`, where `<to>`, if it is given, is the value checked
  // out.
  private def refer(change: Optional[Text], parent: Boolean)(using Cli, Stdio): Raising[Exit] =
    val store = open()

    Disk.locked(store.lock):
      val point = Worktree.view(store)

      val mode = change.lay(if parent then Worktree.Mode.Beside else Worktree.Mode.Plain): change =>
        val from = change.cut(t"..").head
        val to = change.skip(from.length + 2)
        val start = store.locate(point, List(from))
        val axis = store.space.axes.filter { axis => start(axis) != point(axis) }.head

        axis.lay(abort(Workspace.Error(Workspace.Reason.Unmoved(from)))): axis =>
          if to != t"" && store.locate(point, List(to)) != point
          then abort(Workspace.Error(Workspace.Reason.Against(axis.name, from)))

          Worktree.Mode.Against(axis.name, start(axis))

      val count = Worktree.refer(store, mode)
      Out.println(t"Showing ${documents(count)} at ${describe(store, point)}${shown(store, mode)}")
      Exit.Ok

  private def update()(using Cli, Stdio): Raising[Exit] =
    val store = open()

    Disk.locked(store.lock):
      val written = Worktree.update(store)

      written.each: document => Out.println(t"  updated  $document")

      if written.nil then Out.println(t"  the working tree is up to date")
      Exit.Ok

  private def diff(paths: List[Text])(using Cli, Stdio): Raising[Exit] =
    val store = open()

    Worktree.differences(store, paths.map(document(store, _))).each: (change, lines) =>
      Out.println(t"${label(change.kind)}  ${change.document}")
      lines.each: line => Out.println(t"  $line")

    Exit.Ok

  // Ethereal installs shell completions from the launcher it already knows about.
  private def install()(using Stdio, Resident, Diagnostics, Cli): Exit =
    import workingDirectories.javaBaseWorkingDirectory
    import environments.daemonClientEnvironment
    import logging.silentLogging
    given Entrypoint = scala.caps.unsafe.unsafeAssumePure(summon[Resident])

    Out.println(t"Installed tab-completions at:")
    Completions.ensure(force = true).each(Out.println(_))
    Exit.Ok

  private def prefer(axis: Text, parent: Boolean)(using Cli, Stdio): Raising[Exit] =
    val store = open()

    Disk.locked(store.lock):
      store.prefer(axis, parent)
      val shown = if parent then t"its storage parent's lines" else t"its own lines"
      Out.println(t"A stale hunk on the axis $axis shows $shown")
      Exit.Ok

  private def confirm(paths: List[Text])(using Cli, Stdio): Raising[Exit] =
    val store = open()

    Disk.locked(store.lock):
      val found = Worktree.confirm(store, paths.map(document(store, _)))

      found.each: (document, count) =>
        val passages = if count == 1 then t"1 stale passage" else t"$count stale passages"
        Out.println(t"  confirmed  $document  ($passages)")

      if found.nil then Out.println(t"  nothing is stale")
      Exit.Ok

  private def publish(directory: Text, values: List[Text], strict: Boolean)(using Cli, Stdio)
  :   Raising[Exit] =

    val store = open()
    val point = store.locate(Worktree.view(store), values)
    val target = if directory.starts(t"/") then directory else t"$here/$directory"
    val count = Worktree.publish(store, point, target, strict)
    Out.println(t"Exported ${documents(count)} at ${describe(store, point)} to $directory")
    Exit.Ok

  private def label(kind: Worktree.Kind): Text = kind match
    case Worktree.Kind.Added    => t"added   "
    case Worktree.Kind.Modified => t"modified"
    case Worktree.Kind.Missing  => t"missing "

  // The lines of the content which a hunk in place gives, counted from one.
  private def span(placement: Placement): Text =
    if placement.to <= placement.from then t"${placement.from + 1}"
    else if placement.to == placement.from + 1 then t"${placement.from + 1}"
    else t"${placement.from + 1}-${placement.to}"

  private def lines(count: Int): Text = if count == 1 then t"1 line" else t"$count lines"

  private def passages(count: Int): Text =
    if count == 1 then t"1 stale passage" else t"$count stale passages"

  private def progress(sum: Survey.Sum): Text =
    t"${sum.done} of ${lines(sum.total)} translated (${sum.percent}%)"

  // What wants doing in the documents at a point, a line for each piece of it. A stale passage
  // is given by its line in the working tree, if the point is the one checked out, and
  // otherwise by its line in the plain document.
  private def report(store: Workspace, point: Point, works: List[Survey.Work], here: Boolean)
    ( using Stdio )
  :   Raising[Unit] =

    works.each: work =>
      val document = work.document

      if !work.pending.nil
      then Out.println(t"  pending   $document (${lines(work.pending.size)} to translate)")

      work.stale.each: placement =>
        val line = if here then Worktree.line(store, document, placement) else placement.from + 1
        Out.println(t"  stale     $document:$line")

      store.dossier(document).issues(point).each:
        case Issue.Stale(_) =>
          ()

        case Issue.Conflict(shown, hidden) =>
          val origins = t"${store.name(shown.origin)} over ${store.name(hidden.origin)}"
          Out.println(t"  conflict  $document:${span(shown)} ($origins)")

        case Issue.Orphan(origin, _) =>
          Out.println(t"  orphan    $document (a hunk of ${store.name(origin)})")

        case Issue.Ambiguous(origin, _) =>
          Out.println(t"  ambiguous $document (a hunk of ${store.name(origin)})")

  // With no values and without `all`, the point checked out; otherwise every point at or below
  // the values. The edits which are not checked in are counted as if they were.
  private def status(values: List[Text], all: Boolean)(using Cli, Stdio): Raising[Exit] =
    val store = open()
    val target = Worktree.view(store)
    Out.println(t"At ${describe(store, target)}${shown(store, Worktree.mode(store))}")
    val changes = Worktree.changes(store)

    changes.each: change => Out.println(t"  ${label(change.kind)}  ${change.document}")

    if changes.nil then Out.println(t"  nothing to check in")
    val (seen, failed) = Worktree.prospect(store)

    failed.each: document =>
      val reason = t"`orthant checkin --dry-run` says why it cannot be checked in"
      Out.println(t"  unready   $document ($reason)")

    val counted = if changes.nil then t"" else t", counting the edits not checked in"

    if !all && values.nil then
      report(seen, target, seen.documents.map(Survey.work(seen, _, target)), true)
      val sum = Survey.sum(seen, target)
      if sum.total > 0 then Out.println(t"  ${progress(sum)}$counted")
    else
      if !changes.nil then Out.println(t"  (what follows counts the edits not checked in)")

      seen.within(values).each: point =>
        val sum = Survey.sum(seen, point)

        if sum.total > 0 || sum.wanting then
          val state = if sum.total > 0 then progress(sum) else t"not a translation"
          Out.println(t"${describe(store, point)}: $state")
          val own = seen.documents.map(Survey.own(seen, _, point))
          report(seen, point, own, point == target)
          val mine = Survey.sum(own)

          val more = sum.pending - mine.pending

          val shared =
            (if more > 0 then List(t"${lines(more)} to translate") else Nil) +
              (if sum.stale > mine.stale then List(passages(sum.stale - mine.stale)) else Nil)

          val above = t"to be seen to at the points above"
          if !shared.nil then Out.println(t"  shared    ${shared.join(t" and ")}, $above")

    Exit.Ok

  private def wants(work: Survey.Work): Text =
    val pending = t"${lines(work.pending.size)} to translate"
    val faults = t"${work.faults} hunks in conflict or out of place"

    ( (if work.pending.nil then Nil else List(pending)) +
      (if work.stale.nil then Nil else List(passages(work.stale.size))) +
      (if work.faults == 0 then Nil else List(faults)) )

    . join(t", ")

  // The points at or below the values which have work of their own, each with that work.
  private def agenda(store: Workspace, values: List[Text])
  :   Raising[List[(Point, List[Survey.Work])]] =

    store.within(values).bind: point =>
      val works = store.documents.map(Survey.own(store, _, point)).filter(_.wanting)
      if works.nil then Nil else List((point, works))

  private def todo(values: List[Text], take: Boolean)(using Cli, Stdio): Raising[Exit] =
    val store = open()

    if take then Disk.locked(store.lock):
      agenda(store, values).head.lay(Out.println(t"Nothing wants doing.")): (point, works) =>
        val mode = Worktree.approach(store, point)
        val count = Worktree.checkout(store, point, false, mode)
        val place = t"${describe(store, point)}${shown(store, mode)}"
        Out.println(t"Checked out ${documents(count)} at $place")
        report(store, point, works, true)

        if mode != Worktree.Mode.Plain then
          Out.println(t"Translate a `? ` line by adding `+ ` lines beneath it. Once a passage with")
          Out.println(t"`! ` lines is right, delete those lines. Then `orthant checkin`, and")
          Out.println(t"`orthant todo` for what is left.")

      Exit.Ok

    else
      val (seen, _) = Worktree.prospect(store)
      val found = agenda(seen, values)

      found.each: (point, works) =>
        Out.println(describe(store, point))

        works.each: work =>
          val reach = Worktree.reach(seen, work.document, point)

          val below =
            if reach == 0 then t""
            else if reach == 1 then t"; and so at 1 point below"
            else t"; and so at $reach points below"

          Out.println(t"  ${work.document}: ${wants(work)}$below")

      found.head.lay(Out.println(t"Nothing wants doing.")): (point, works) =>
        val command = (t"orthant todo" :: values).join(t" ")
        val mode = Worktree.approach(seen, point)
        val place = t"${describe(store, point)}${shown(store, mode)}"
        Out.println(t"`$command --checkout` checks out $place")

      Exit.Ok

  // An axis's values, each after its parent, with how deep it is.
  private def tree(axis: Axis): List[(Text, Int)] =
    def recur(value: Text, depth: Int): List[(Text, Int)] =
      (value, depth) :: axis.children(value).bind(recur(_, depth + 1))

    recur(axis.root, 0)

  private def pad(text: Text, width: Int): Text =
    t"$text${List.range(text.length, width).map { index => t" " }.join}"

  private val Done = Chroma(46, 125, 50)
  private val Checking = Chroma(249, 200, 40)
  private val Partial = Chroma(239, 125, 20)
  private val Untouched = Chroma(198, 40, 40)
  private val Faulty = Chroma(142, 36, 170)
  private val Dark = Chroma(0, 0, 0)
  private val Light = Chroma(255, 255, 255)

  private def paint(text: Text, colour: Chroma): Teletype =
    val ink = if colour == Checking || colour == Partial then Dark else Light
    e"${Bg(colour)}(${Fg(ink)}($text))"

  // A cell for each pair of values of two axes, the other axes being at the values checked
  // out: how much of the point is translated, and what wants doing there.
  private def grid(names: List[Text])(using Cli, Stdio): Raising[UsageError.type | Exit] =
    val store = open()
    val target = Worktree.view(store)
    val (seen, _) = Worktree.prospect(store)
    val axes = store.space.axes
    val chosen = names.map: name => axes.filter(_.name == name).head

    val pair: Optional[(Axis, Optional[Axis])] = chosen match
      case Nil                                  => axes.head.let((_, axes.excerpt(1, 2).head))
      case (row: Axis) :: Nil                   => (row, Unset)
      case (row: Axis) :: (column: Axis) :: Nil => (row, column)
      case _                                    => Unset

    val synopsis = t"orthant grid [<axis> [<axis>]], each an axis which `orthant axis` lists"

    pair.lay(usage(synopsis)):
      (rows, columns) =>
        val across: List[Optional[Text]] =
          columns.lay(List[Optional[Text]](Unset))(tree(_).map(_(0)))

        def title(axis: Optional[Axis], value: Optional[Text]): Text =
          axis.let { axis => value.let(store.title(axis.name, _)) }.or(t"")

        def at(row: Text, column: Optional[Text]): Point =
          val moved = target.move(rows, row)
          columns.let { axis => column.let(moved.move(axis, _)) }.or(moved)

        def content(sum: Survey.Sum): Text =
          if sum.total == 0 && !sum.wanting then t"·" else
            ( List(if sum.total == 0 then t"-" else t"${sum.percent}%") +
              (if sum.pending > 0 then List(t"${sum.pending}?") else Nil) +
              (if sum.stale > 0 then List(t"${sum.stale}!") else Nil) +
              (if sum.faults > 0 then List(t"${sum.faults}x") else Nil) )

            . join(t" ")

        def colour(sum: Survey.Sum): Optional[Chroma] =
          if sum.faults > 0 then Faulty
          else if sum.pending > 0 then (if sum.done == 0 then Untouched else Partial)
          else if sum.stale > 0 then Checking
          else if sum.total > 0 then Done
          else Unset

        val body: List[(Text, List[Survey.Sum])] = tree(rows).map: (row, depth) =>
          val indent = List.range(0, depth).map { level => t"  " }.join
          val sums = across.map: column => Survey.sum(seen, at(row, column))
          (t"$indent${store.title(rows.name, row)}", sums)

        val heads = across.map(title(columns, _))

        val wide =
          (heads.map(_.length) + body.bind(_(1).map(content(_).length))).fold(4)(_.max(_)) + 2

        val margin = body.map(_(0).length).fold(rows.name.length)(_.max(_)) + 2

        val fixed =
          axes.filter { axis => axis != rows && columns.lay(true)(_ != axis) }.map: axis =>
            t"${axis.name}=${store.title(axis.name, target(axis))}"

        val over = columns.lay(t""): axis => t" by ${axis.name}"
        val place = if fixed.nil then t"" else t", at ${fixed.join(t" ")}"
        Out.println(t"Lines translated, ${rows.name}$over$place")
        Out.println(t"")
        Out.println(t"${pad(t"", margin)}${heads.map { head => pad(t" $head", wide) }.join(t" ")}")

        body.each: (label, sums) =>
          val cells: List[Teletype] = sums.map: sum =>
            val text = pad(t" ${content(sum)}", wide)
            colour(sum).lay(e"$text")(paint(text, _))

          Out.println(e"${pad(label, margin)}${cells.join(e" ")}")

        Out.println(t"")

        Out.println:
          List
            ( paint(t" done ", Done), paint(t" stale passages (!) ", Checking),
              paint(t" partly translated (?) ", Partial), paint(t" not begun ", Untouched),
              paint(t" conflicts (x) ", Faulty) )

          . join(e" ")

        Exit.Ok

  private def checkin(paths: List[Text], remove: Boolean, dry: Boolean, reach: Worktree.Reach)
    ( using Cli, Stdio )
  :   Raising[Exit] =

    val store = open()

    Disk.locked(store.lock):
      val tally = Worktree.checkin(store, paths.map(document(store, _)), remove, dry, reach)
      val verb = if dry then t"would store" else t"stored"

      tally.stored.each: change =>
        Out.println(t"  $verb  ${label(change.kind)}  ${change.document}")

      tally.skipped.each: change =>
        Out.println(t"  skipped  ${change.document} is missing; --remove deletes it at this point")

      if dry then
        val changed = tally.effects.filter(_.changed).map: effect => describe(store, effect.point)

        val named = if changed.size <= 4 then changed.join(t", ") else
          t"${changed.keep(3).join(t", ")} and ${changed.size - 3} others"

        val points = if changed.size == 1 then t"1 point" else t"${changed.size} points"
        if !changed.nil then Out.println(t"  would change the content at $points: $named")

      // What the check-in does to the work at each point.
      tally.effects.each: effect =>
        def more(count: Int, one: Text, many: Text): List[Text] =
          val size = if count < 0 then 0 - count else count
          val noun = if size == 1 then one else many
          val change = if count > 0 then t"more" else t"fewer"
          if count == 0 then Nil else List(t"$size $change $noun")

        val parts =
          more(effect.pending, t"line to translate", t"lines to translate") +
            more(effect.stale, t"stale passage", t"stale passages")

        val verb = if dry then t"would leave" else t"leaves"
        val place = describe(store, effect.point)
        if !parts.nil then Out.println(t"  $verb  $place with ${parts.join(t", ")}")

      if tally.stored.nil && tally.skipped.nil then Out.println(t"  nothing to check in")
      Exit.Ok
