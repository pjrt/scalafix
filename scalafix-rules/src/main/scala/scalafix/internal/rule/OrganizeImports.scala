package scalafix.internal.rule

import scala.collection.mutable
import scala.meta._
import scala.meta.tokens.Token._

import metaconfig._
import metaconfig.annotation.Description
import metaconfig.generic.Surface

import scalafix.v1._

final case class OrganizeImportsConfig(
    @Description("Import groups") importGroups: List[String] = Nil)

object OrganizeImportsConfig {
  implicit val surface: Surface[OrganizeImportsConfig] =
    generic.deriveSurface[OrganizeImportsConfig]
  implicit val decoder: ConfDecoder[OrganizeImportsConfig] =
    generic.deriveDecoder[OrganizeImportsConfig](OrganizeImportsConfig())
}

final class OrganizeImports(config: OrganizeImportsConfig)
    extends SyntacticRule("OrganizeImports") {

  // DESNOTE(2018-11-28, pjrt): Needed for the testing framework to work.
  // This is a smell.
  def this() = this(OrganizeImportsConfig())

  override def description: String =
    "Sorts and organizes imports into a pattern"
  override def isRewrite: Boolean = true

  override def fix(implicit doc: SyntacticDocument): Patch = {

    type Acc[F[_]] = F[(Import, Importer)]
    val (fAcc, acc): (mutable.ListBuffer[Acc[List]], Acc[mutable.ListBuffer]) =
      (mutable.ListBuffer(), mutable.ListBuffer())

    def addIfNotEmpty =
      if (acc.isEmpty) ()
      else {
        fAcc += acc.toList
        acc.clear
      }

    // DESNOTE(2017-09-09, pjrt): Boy, mutability is complex
    // This is essentially trying to replicate a foldLeft but due to the
    // mutable Apis of scalameta, we can't currently just use a foldLeft.
    // The idea here is to collect all the import groups along with their
    // location.
    doc.tree.traverse {
      case stat @ Import(imports) =>
        acc ++= imports.map(stat -> _)
      case stat if isPartOfImport(stat) =>
        ()
      case _ =>
        addIfNotEmpty
    }

    // We need to make another call to addIfNotEmpty since the last stat
    // may be an import (which would mean we never ad the last group to fAcc.
    addIfNotEmpty

    fAcc.toList.flatMap { d =>
      val (toRemove, decopImps) = d.unzip
      if (decopImps.isEmpty) Nil
      else {
        val (positions, rms) =
          toRemove.flatMap(_.tokens.map(s => s -> Patch.removeToken(s))).unzip
        val sortedStr = replaceWithSorted(decopImps, config.importGroups)
        rms :+ Patch.addRight(positions.head, sortedStr)
      }
    }.asPatch
  }

  private def isPartOfImport(tree: Tree): Boolean =
    tree.is[Import] || tree.parent.exists(_.is[Import])

  // DESNOTE(2017-09-07, pjrt): Given a set of import statements (flatten) and a
  // list of import groups: sort them vertically, split them in groups and
  // sort their importees.
  private def replaceWithSorted(
      decopImps: Seq[Importer],
      importGroups: List[String]): String = {

    def startsWith(imp: Importer, p: String) = {
      val ps = p.split(',')
      val str = imp.toString
      ps.exists(str.startsWith(_))
    }
    import Matcher._
    val groups: Seq[Seq[Importer]] = decopImps
      .map { imp =>
        val group = importGroups.zipWithIndex.foldLeft(None: Option[Matcher]) {
          case (None, ("*", i)) => Some(GeneralFind(i))
          case (None, (p, i)) if (startsWith(imp, p)) => Some(ExactFind(i))
          case (Some(GeneralFind(_)), (p, i)) if (startsWith(imp, p)) =>
            Some(ExactFind(i))
          case (acc, _) => acc
        }
        // DESNOTE(2017-09-06, pjrt): If we don't find any pattern that
        // matches, just place it at the end. Only way this can happen is
        // if the user doesn't place a `*` anywhere in the pattern list.
        // If no patterns are given in the setting, they are all sorted into
        // the same group.
        val sortedImps =
          imp.copy(importees = sortImportees(imp.importees).toList)
        sortedImps -> group.fold(importGroups.length)(_.index)
      }
      .groupBy(_._2)
      .map { case (i, imps) => i -> imps.map(_._1) }
      .toSeq
      .sortBy(_._1)
      .map(_._2)

    groups
      .map(is => is.map(importerToString).sorted.mkString("\n"))
      .mkString("\n\n")
  }

  // DESNOTE(2017-09-08, pjrt): There seems to be a bug in scalameta where the
  // ` (back tick) character isn't printed if you just do `Impoter.toString`
  // or `.syntax`. So we get around it by printing importees directly.
  private def importerToString(imp: Importer): String = {
    val importees = imp.importees
    val importeesStr =
      if (importees.isEmpty) ""
      else if (importees.length == 1)
        s".${importees.head.toString}"
      else s".{ ${importees.mkString(", ")} }"

    s"import ${imp.ref}$importeesStr"
  }

  // sort contributed by @djspiewak: https://gist.github.com/djspiewak/127776c2b6a9d6cd3c21a228afd4580f
  private val LCase = """([a-z].*)""".r
  private val UCase = """([A-Z].*)""".r
  private val Other = """(.+)""".r

  private implicit val importeeOrdering: Ordering[Importee] =
    new Ordering[Importee] {
      def compare(a: Importee, b: Importee) =
        a.tokens.mkString compare b.tokens.mkString
    }

  private def sortImportees(strs: Seq[Importee]): Seq[Importee] = {
    // we really want partition, but there is no ternary version of it
    val (syms, lcs, ucs) =
      strs.foldLeft(
        (
          Vector.empty[Importee],
          Vector.empty[Importee],
          Vector.empty[Importee])) {
        case ((syms, lcs, ucs), imp) =>
          val str = imp.tokens.mkString
          str match {
            case LCase(_) => (syms, lcs :+ imp, ucs)
            case UCase(_) => (syms, lcs, ucs :+ imp)
            case Other(_) => (syms :+ imp, lcs, ucs)
          }
      }
    syms.sorted ++ lcs.sorted ++ ucs.sorted
  }
}

private sealed trait Matcher {

  def index: Int
}

private object Matcher {
  final case class ExactFind(index: Int) extends Matcher
  final case class GeneralFind(index: Int) extends Matcher
}
