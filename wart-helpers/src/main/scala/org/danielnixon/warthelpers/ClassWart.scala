package org.danielnixon.warthelpers

import org.wartremover.{ WartTraverser, WartUniverse }

import scala.util.control.Exception.catching

abstract class ClassWart(
    wartClassName: String,
    targetClassName: String,
    methods: List[(String, String)]
) extends WartTraverser {

  class Op(name: String, error: String) extends WartTraverser {
    override lazy val className: String = wartClassName

    def apply(u: WartUniverse): u.Traverser = {
      import u.universe._

      val symbol = catching(classOf[ScalaReflectionException]) opt {
        rootMirror.staticClass(targetClassName)
      }

      val Name = TermName(name)

      new u.Traverser {
        override def traverse(tree: Tree): Unit = {
          tree match {
            // Ignore trees marked by SuppressWarnings
            case t if hasWartAnnotation(u)(t) =>
            case Select(left, Name) if symbol.exists(s => left.tpe.baseType(s) != NoType) =>
              error(u)(tree.pos, error)
            // TODO: This ignores a lot
            case LabelDef(_, _, _) if isSynthetic(u)(tree) =>
            case _ ⇒
              super.traverse(tree)
          }
        }
      }
    }
  }

  def apply(u: WartUniverse): u.Traverser =
    WartTraverser.sumList(u)(methods.map(method => new Op(method._1, method._2)))
}