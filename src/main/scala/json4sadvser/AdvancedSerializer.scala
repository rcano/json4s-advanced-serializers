package json4sadvser

import language.experimental.macros

import org.json4s._
import scala.reflect.macros.blackbox.Context

object AdvancedSerializer {

  trait ForType[T] {
    final def ignoreField[R](field: T => R)(defaultValueOnRead: R): Rule[T] = ???
    final def renameField(field: T => Any)(newName: String): Rule[T] = ???
    final def mapField[U, V](field: T => U)(from: U => V)(to: V => U): Rule[T] = ???
    final def mapRenameField[U, V](field: T => U)(newName: String)(from: U => V)(to: V => U): Rule[T] = ???
    
    final def build(): Serializer[T] = macro AdvancedSerializerMacro.forType[T]
  }
  
  sealed trait Rule[T]
}

class AdvancedSerializerMacro(val c: Context) {
  import c.universe._, AdvancedSerializer._
  
  sealed trait Rule { def field: Tree }
  case class IgnoreField(field: Tree, defaultValue: Tree) extends Rule
  case class RenameField(field: Tree, newName: String) extends Rule
  case class MapField(field: Tree, newName: String, from: Tree, to: Tree) extends Rule
  
  def forType[T: c.WeakTypeTag](): Tree = {
    val selectedTypeSymbol = {
      val res = c.weakTypeOf[T]
      //validate class type was used
      if (!res.typeSymbol.isClass || res.typeSymbol.isAbstract || !res.typeSymbol.asClass.primaryConstructor.isPublic) {
        c.abort(c.prefix.tree.pos, s"Invalid type selected for serialization. This builder only supports classes with a primary public constructor")
      }
      res.typeSymbol.asClass
    }
    val selectedType = c.weakTypeOf[T]
    
    lazy val extractRule: PartialFunction[Tree, Rule] =  {
      case t@q"$pref.ignoreField[..$tparams]($field)($defaultVal)" => IgnoreField(field, defaultVal)
      case t@q"$pref.renameField($field)($newName)" => RenameField(field, c.eval(c.Expr[String](c untypecheck newName)))
      case t@q"$pref.mapField[..$tparams]($field)($from)($to)" =>  MapField(field, field.children.last.symbol.name.toString, from, to)
      case t@q"$pref.mapRenameField[..$tparams]($field)($newName)($from)($to)" => MapField(field, c.eval(c.Expr[String](c untypecheck newName)), from, to)
    }
    
    val rules = c.prefix.tree collect extractRule
    
    //validate fields
    for (rule <- rules) {
      rule.field match {
        case q"($param) => $pref.$expr" if param.symbol == pref.symbol =>
          if (!selectedTypeSymbol.primaryConstructor.asMethod.paramLists.flatten.exists(p => p.name == expr)) 
            c.abort(rule.field.pos, s"Selected field is not a constructor parameter of $selectedTypeSymbol")
        case other => c.abort(rule.field.pos, s"Invalid expression. Only field selections of the form _.someField are allowed here. Please select one of $selectedTypeSymbol fields")            
      }
//      println(s"Good rule $rule. Field name: ${rule.field.children.last.symbol}")
    }
    
    //map ctor fields to rules
    val constructorRules = selectedTypeSymbol.primaryConstructor.asMethod.paramLists.flatten.map {s => 
      val possibleRules = rules.filter(_.field.children.last.symbol.name == s.name)
      if (possibleRules.size > 1) c.abort(possibleRules(1).field.pos, s"There is already a rule for field $s. There can only exists one rule per field")
      s -> possibleRules.headOption
    }
    
    val generatedSerializators = constructorRules map { case (ctorArg, rule) => 
        rule match {
          case Some(IgnoreField(_, defaultValue)) => q"" -> c.untypecheck(defaultValue)
          case Some(RenameField(_, newName)) => q"$newName -> Extraction.decompose(v.${ctorArg.name.toTermName})" -> q"""Extraction.extract[${ctorArg.info}](jv \ $newName)"""
          case Some(MapField(_, newName, from, to)) => 
            val toTpe = to.tpe.baseType(symbolOf[_ => _]).typeArgs(0)
            val uTo = c untypecheck to
            val uFrom = c untypecheck from
            q"$newName -> Extraction.decompose($uFrom(v.${ctorArg.name.toTermName}))" -> q"""$uTo(Extraction.extract[$toTpe](jv \ $newName))"""
        
          case None => q"${ctorArg.name.toString} -> Extraction.decompose(v.${ctorArg.name.toTermName})" -> q"""Extraction.extract[${ctorArg.info}](jv \ ${ctorArg.name.toString})"""}
    }
    
//    generatedSerializators foreach (t => println(showCode(t._1) + "  ========   " + showCode(t._2)))
    
    val writeLogic = generatedSerializators.map(_._1).filterNot(_.isEmpty).reduceOption((a, b) => q"$a ~ $b") getOrElse q"JNothing"
    val readLogic = q"new $selectedType(..${generatedSerializators.map(_._2)})"
    
    val res = q"""
import org.json4s._, JsonDSL._
    
new Serializer[$selectedType] {
  
  val Class = classOf[$selectedType]
   
  override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), $selectedType] = {
    case (TypeInfo(Class, _), jv) => $readLogic
  }
    
  override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case v: $selectedType => $writeLogic
  }
}"""
//    println(showCode(res))
    res
    
  }
}
