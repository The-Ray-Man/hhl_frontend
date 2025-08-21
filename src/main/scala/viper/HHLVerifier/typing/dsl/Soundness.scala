package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.SpecialId
import viper.HHLVerifier.ast.AssertVar
import viper.HHLVerifier.ast.AssertVarDecl
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.ImpliesExpr
import viper.HHLVerifier.ast.Assertion
import viper.HHLVerifier.ast.StateExistsExpr
import viper.HHLVerifier.ast.LoopIndex
import viper.HHLVerifier.ast.HintDecl
import viper.HHLVerifier.ast.Hint
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.ast.SeqAssignExpr
import viper.HHLVerifier.ast.SetAssignExpr
import viper.HHLVerifier.ast.MapAssignExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.LengthExpr
import viper.HHLVerifier.ast.CombExpr
import viper.HHLVerifier.ast.UpdateMapExpr
import viper.HHLVerifier.ast.MapTupleExpr
import viper.HHLVerifier.typing.IntType
import viper.HHLVerifier.typing.BoolType
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.symbols.SymbolChecker
import viper.HHLVerifier.typing.TypeChecker
import viper.HHLVerifier.generation.Generator
import viper.HHLVerifier.management.ViperRunner
import viper.silver.verifier.{Failure => ResFailure, Success => ResSuccess}
import viper.HHLVerifier.management.PrettyPrinter
import viper.HHLVerifier.ast.AssumeStmt

object Soundness {
  var typeSystem: TypeSystem = null
  // report contains the information about the check.
  //   the first string is the ruleName e.g. exprplus
  // the number is the index of the rule
  var report: Map[String, Map[Int, String]] = Map.empty

  def main(args: Array[String]): Unit = {
    report = Map.empty

    val typeSystem = TypeSystem.loadTypeSystem(Seq("/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/value.type"))
    check(typeSystem)

    generateReport()
  }

  def generateReport(): Unit = {
    report.foreach { case (ruleName, indices) =>
      println(s"Rule ${ruleName.dropRight(1)}")
      indices.toSeq.sortBy(_._1).foreach { case (index, status) =>
        println(s"  $index: $status")
      }
    }
  }

  def isOnlyStateExists(expr: Expr): Boolean = {
    expr match {
      case StateExistsExpr(_, _)  => true
      case BinaryExpr(e1, op, e2) => isOnlyStateExists(e1) && isOnlyStateExists(e2)
      case UnaryExpr(op, e)       => isOnlyStateExists(e)
      case _                      => false
    }
  }

  def removeStateExistsImplication(expr: Expr): Expr = {
    expr match {
      case Assertion(quantifier, assertVarDecls, body) => {
        val newBody = body match {
          case ImpliesExpr(left, right) if isOnlyStateExists(left) => right
          case _                                                   => body
        }
        Assertion(quantifier, assertVarDecls, newBody)
      }
      case _ => expr
    }
  }

  def programToFile(filePath: String, program: HHLProgram): Unit = {
    // needs to filter out the state exists expressions.

    val newProgram = HHLProgram(program.methods.map(method => {
      val filteredPrecondition  = method.pre.map(removeStateExistsImplication(_))
      val filteredPostcondition = method.post.map(removeStateExistsImplication(_))
      method.copy(pre = filteredPrecondition, post = filteredPostcondition)
    }))
    val file   = new java.io.File(filePath)
    val writer = new java.io.PrintWriter(file)
    writer.write(PrettyPrinter.formatProgram(newProgram))
    writer.close()
    println(s"Wrote soundness check program to $filePath")

  }

  def checkProgram(name: String, parsedProgram: HHLProgram): Boolean = {

    programToFile(s"${name}.hhl", parsedProgram)

    SymbolChecker.reset()
    TypeChecker.reset()
    Generator.reset()

    SymbolChecker.checkSymbolsProg(parsedProgram)

    TypeChecker.typeCheckProg(parsedProgram)
    val viperProgram = Generator.generate(parsedProgram, "")

    val consistencyErrors = viperProgram.checkTransitively
    // We check whether the program is well-defined (i.e., has no consistency errors such as ill-typed expressions)
    if (consistencyErrors.nonEmpty) {
      consistencyErrors.foreach(err => println(err.readableMessage))
      return false
    } else {
      val result = ViperRunner.runSiliconAndCarbon(viperProgram)

      result match {
        case ResSuccess => {
          return true
        }
        case ResFailure(err) =>
          err.foreach(e => println(e))
          return false
      }
    }
  }

  def check(ts: TypeSystem): Unit = {
    typeSystem = ts

    val programs = ts.expressionTypeSystem.map(generateExpressionRule(_))

    programs.foreach { derivationProgram =>
      derivationProgram._2.foreach {
        case (i, Some(prog)) => {
          if (checkProgram(s"${derivationProgram._1}$i", prog)) {
            report = report.updated(derivationProgram._1, report(derivationProgram._1).updated(i, "sound"))
          } else {
            report = report.updated(derivationProgram._1, report(derivationProgram._1).updated(i, "might be unsound"))
          }
        }
        case (i, None) =>
          report = report.updated(derivationProgram._1, report(derivationProgram._1).updated(i, "notChecked"))
      }
    }
  }

  def getRuleName(derivationRule: DerivationRule): String = {
    derivationRule match {
      case exprRule: ExpressionDerivationRule =>
        "expr_" + (exprRule.expr match {
          case BinaryExpr(e1, op, e2) =>
            op match {
              case "+"   => "plus"
              case "-"   => "minus"
              case "*"   => "times"
              case "/"   => "div"
              case "%"   => "modulo"
              case "<"   => "lessThan"
              case "<="  => "lessThanOrEqual"
              case ">"   => "greaterThan"
              case ">="  => "greaterThanOrEqual"
              case "=="  => "equal"
              case "!="  => "notEqual"
              case "&&"  => "and"
              case "||"  => "or"
              case "==>" => "implies"
            }
          case UnaryExpr(op, e) =>
            op match {
              case "!" => "not"
              case "-" => "negate"
            }
          case ImpliesExpr(left, right) => "implies"
          case Id(name)                 =>
            name match {
              case "var" => "variable"
              case "n"   => "num"
              case "b"   => "bool"
            }
          case LookupExpr(id, index)  => "lookup"
          case LengthExpr(id)         => "length"
          case CombExpr(lhs, rhs, op) =>
            op match {
              case "++"           => "concat"
              case "setminus"     => "setminus"
              case "union"        => "union"
              case "in"           => "contains"
              case "intersection" => "intersection"
            }
          case _ => throw new Exception("For this expression no typing rules are considered")
        }) + "_"
      case _: StatementDerivationRule => "StatementRule"
    }
  }

  def generateExpressionRule(expressionRule: ExpressionDerivationRule): (String, Seq[(Int, Option[HHLProgram])]) = {
    report += (getRuleName(expressionRule) -> Map.empty)
    expressionRule.expr match {
      case binOp @ BinaryExpr(e1, op, e2)                    => checkBinOpExpressionRule(binOp, expressionRule)
      case unOp @ UnaryExpr(op, e)                           => checkUnaryExpressionRule(unOp, expressionRule)
      case variable @ Id(name) if name == "n" || name == "b" =>
        checkConstExpressionRule(variable, expressionRule)
      case implicationOp @ ImpliesExpr(left, right) =>
        checkBinOpExpressionRule(BinaryExpr(left, "==>", right), expressionRule)
      case _ => {
        (getRuleName(expressionRule), expressionRule.rules.zipWithIndex.map { case (rule, index) => (index, None) })
      }
    }
  }

  def buildPrograms(ruleName: String, args: Seq[Id], formulas: Seq[(Int, Option[(Seq[Expr], Seq[Expr], Seq[Stmt])])]): Seq[(Int, Option[HHLProgram])] = {
    formulas.map {
      case (i, Some((conds, concs, assertion))) => {
        (i, Some(HHLProgram(Seq(Method(s"${ruleName}$i", args, Seq.empty, conds, concs, CompositeStmt(assertion))))))
      }
      case (i, None) => (i, None)
    }
  }

  def checkConstExpressionRule(const: Id, expressionRule: ExpressionDerivationRule): (String, Seq[(Int, Option[HHLProgram])]) = {
    var inputVar  = Id("inputVar")
    val inputType = const.name match {
      case "n" => IntType()
      case "b" => BoolType()
    }
    inputVar.typ = inputType
    var templateToIds     = Map(const -> inputVar)
    val rulesWithIndex    = expressionRule.rules.zipWithIndex
    val prePostConditions = ruleToPreAndPostCondition(rulesWithIndex, templateToIds, inputVar)
    val ruleName          = getRuleName(expressionRule)
    val programs          = buildPrograms(ruleName, Seq(inputVar), prePostConditions)
    (ruleName, programs)
  }

  def checkUnaryExpressionRule(unOp: UnaryExpr, expressionRule: ExpressionDerivationRule): (String, Seq[(Int, Option[HHLProgram])]) = {
    val (inputType, outputType) = unOp.op match {
      case "!" => (BoolType(), BoolType())
      case "-" => (IntType(), IntType())
    }
    var inputVar = Id("inputVar")
    inputVar.typ = inputType

    val templateToIds     = Map(unOp.e.asInstanceOf[Id] -> inputVar)
    val rulesWithIndex    = expressionRule.rules.zipWithIndex
    val prePostConditions = ruleToPreAndPostCondition(rulesWithIndex, templateToIds, UnaryExpr(unOp.op, inputVar))
    val ruleName          = getRuleName(expressionRule)
    val programs          = buildPrograms(ruleName, Seq(inputVar), prePostConditions)
    (ruleName, programs)
  }

  def checkBinOpExpressionRule(binOp: BinaryExpr, expressionRule: ExpressionDerivationRule): (String, Seq[(Int, Option[HHLProgram])]) = {
    // check if the left and right expressions are of the same type

    val (leftType, rightType, resType) = binOp.op match {
      case "+" | "-" | "*" | "/" | "%"           => (IntType(), IntType(), IntType())
      case "<" | "<=" | ">" | ">=" | "==" | "!=" => (IntType(), IntType(), BoolType())
      case "&&" | "||" | "==>"                   => (BoolType(), BoolType(), BoolType())
    }

    var lhs = Id("lhsVar")
    var rhs = Id("rhsVar")
    lhs.typ = leftType
    rhs.typ = rightType
    val templateToIds = Map(binOp.e1.asInstanceOf[Id] -> lhs, binOp.e2.asInstanceOf[Id] -> rhs)

    val rulesWithIndex = expressionRule.rules.zipWithIndex

    val resultingExpr = binOp.op match {
      case "==>" => ImpliesExpr(lhs, rhs)
      case _     => BinaryExpr(lhs, binOp.op, rhs)
    }
    val prePostConditions = ruleToPreAndPostCondition(rulesWithIndex, templateToIds, resultingExpr)

    val ruleName = getRuleName(expressionRule)

    val programs = buildPrograms(ruleName, Seq(lhs, rhs), prePostConditions)

    (ruleName, programs)

  }

  def transformCondition(condition: Condition, templateToIds: Map[Id, Id]): Option[(Boolean, Expr)] = {
    condition match {
      case InSet(htyp: HyperType, HyperTypeCheck(expr, Gamma(), Delta())) => {
        val variable = templateToIds.getOrElse(expr, throw new Exception("This should never happen!"))
        Some((true, getExpression(variable, htyp)))
      }
      case ArithCondition(expr, op, num) => {
        val variable = templateToIds.getOrElse(expr, throw new Exception("This should never happen!"))
        Some((false, BinaryExpr(variable, op, Num(num))))
      }
      case BoolCondition(expr) => {
        val variable = templateToIds.getOrElse(expr, throw new Exception("This should never happen!"))
        Some((false, variable))
      }
      case NotOperator(condition) => {
        val subCondition = transformCondition(condition, templateToIds)
        subCondition match {
          case Some((true, _))     => None
          case Some((false, expr)) => Some((false, UnaryExpr("!", expr)))
          case None                => None
        }
      }
      case _ => None
    }
  }

  def transformConclusion(conclusion: Conclusion, resultExpr: Expr, templateToIds: Map[Id, Id]): Option[Expr] = {
    conclusion match {
      case AddToSet(htyp: HyperType, HyperCollectionResult()) => {
        Some(getExpression(resultExpr, htyp))
      }
      case _ => None
    }
  }

  def ruleToPreAndPostCondition(rulesWithIndex: Seq[(Rule, Int)], templateToIds: Map[Id, Id], resultExpr: Expr): Seq[(Int, Option[(Seq[Expr], Seq[Expr], Seq[Stmt])])] = {
    rulesWithIndex.map {
      case (rule, index) => {
        var conditions   = Seq.empty[Expr]
        var assertions   = Seq.empty[Stmt]
        var conclusion   = Seq.empty[Expr]
        var notCheckable = false
        rule.conditions.foreach(cond => {
          transformCondition(cond, templateToIds) match {
            case Some((true, expr))  => conditions :+= expr
            case Some((false, expr)) => assertions :+= AssumeStmt(expr)
            case None                => notCheckable = true
          }
        })

        rule.conclusions.foreach(conc => {
          transformConclusion(conc, resultExpr, templateToIds) match {
            case Some(expr) => conclusion :+= expr
            case None       => notCheckable = true
          }
        })
        if (notCheckable) {
          (index, None)
        } else {
          (index, Some((conditions, conclusion, assertions)))
        }
      }
    }
  }

  def getExpression(expr: Expr, htyp: HyperType): Expr = {
    val possibleHTDeclarations = typeSystem.hyperTypeDeclaration.map(decl => (decl, getMapping(decl.hty, htyp))).filter { case (_, d) => d.isDefined }.map { case (decl, d) => (decl, d.get) }
    if (possibleHTDeclarations.length != 1) {
      throw new Exception(s"Expected exactly one matching HyperType declaration for $htyp, found ${possibleHTDeclarations.length}.")
    }
    val declaration            = possibleHTDeclarations.head._1
    val mapping: Map[Id, Expr] = possibleHTDeclarations.head._2 + (declaration.variable -> expr)
    // val expr = applyMapping(, declaration.definition)
    val conditionExpr = pushIntoStates(applyMapping(mapping, declaration.definition))
    conditionExpr
  }

  def getExpression(id: Id, htyp: HyperType): Expr = {
    // find the matching expression
    val possibleHTDeclarations = typeSystem.hyperTypeDeclaration.map(decl => (decl, getMapping(decl.hty, htyp))).filter { case (_, d) => d.isDefined }.map { case (decl, d) => (decl, d.get) }

    if (possibleHTDeclarations.length != 1) {
      throw new Exception(s"Expected exactly one matching HyperType declaration for $htyp, found ${possibleHTDeclarations.length}.")
    }
    val declaration = possibleHTDeclarations.head._1
    val mapping     = possibleHTDeclarations.head._2 + (declaration.variable -> id)

    val expr = applyMapping(mapping, declaration.definition)
    expr
  }

  def getMapping(fromElement: Element, toElement: Element): Option[Map[Id, Id]] = {
    (fromElement, toElement) match {
      case (SimpleHyperType(fromName), SimpleHyperType(toName)) if fromName == toName                                                                   => Some(Map.empty)
      case (HyperTypeWithListArgs(fromName, fromArgs), HyperTypeWithListArgs(toName, toArgs)) if fromName == toName && fromArgs.length == toArgs.length => {
        val argsMapped = fromArgs.zip(toArgs).map { case (fromArg, toArg) =>
          getMapping(fromArg, toArg)
        }
        if (!argsMapped.forall(_.isDefined)) None
        else {
          val mapping = argsMapped.flatten.flatten.toMap
          Some(mapping)
        }
      }
      case (HyperTypeWithSetArgs(fromName, fromArgs), HyperTypeWithSetArgs(toName, toArgs)) if fromName == toName && fromArgs.size == toArgs.size => {
        val argsMapped = fromArgs.zip(toArgs).map { case (fromArg, toArg) =>
          getMapping(fromArg, toArg)
        }
        if (!argsMapped.forall(_.isDefined)) None
        else {
          val mapping = argsMapped.flatten.flatten.toMap
          Some(mapping)
        }
      }
      case (_, _) => None
    }
  }

  def pushIntoStates(expr: Expr): Expr = {
    expr match {
      case id: Id                                      => id
      case num: Num                                    => num
      case bool: BoolLit                               => bool
      case BinaryExpr(e1, op, e2)                      => BinaryExpr(pushIntoStates(e1), op, pushIntoStates(e2))
      case UnaryExpr(op, e)                            => UnaryExpr(op, pushIntoStates(e))
      case ImpliesExpr(left, right)                    => ImpliesExpr(pushIntoStates(left), pushIntoStates(right))
      case Assertion(quantifier, assertVarDecls, body) => Assertion(quantifier, assertVarDecls, pushIntoStates(body))
      case StateExistsExpr(state, err)                 => StateExistsExpr(state, err)
      case LookupExpr(id: AssertVar, index)            => {
        insideState(id, index)
      }
      case _ => throw new Exception(s"Unsupported expression type: ${expr.getClass().getSimpleName()}")
    }
  }

  def insideState(state: SpecialId, expr: Expr): Expr = {
    expr match {
      case id: Id                   => LookupExpr(state, id)
      case num: Num                 => num
      case bool: BoolLit            => bool
      case BinaryExpr(e1, op, e2)   => BinaryExpr(insideState(state, e1), op, insideState(state, e2))
      case UnaryExpr(op, e)         => UnaryExpr(op, insideState(state, e))
      case ImpliesExpr(left, right) => ImpliesExpr(insideState(state, left), insideState(state, right))
      case _                        => throw new Exception(s"Unsupported expression type: ${expr.getClass().getSimpleName()}")
    }
  }

  def applyMapping(mapping: Map[Id, Expr], expr: Expr): Expr = {
    expr match {
      case id @ Id(name)                               => applyMapping(mapping, id)
      case num @ Num(value)                            => num
      case bool @ BoolLit(value)                       => bool
      case BinaryExpr(e1, op, e2)                      => BinaryExpr(applyMapping(mapping, e1), op, applyMapping(mapping, e2))
      case UnaryExpr(op, e)                            => UnaryExpr(op, applyMapping(mapping, e))
      case ImpliesExpr(left, right)                    => ImpliesExpr(applyMapping(mapping, left), applyMapping(mapping, right))
      case Assertion(quantifier, assertVarDecls, body) =>
        Assertion(quantifier, assertVarDecls.map(ad => applyMapping(mapping, ad)), applyMapping(mapping, body))
      case StateExistsExpr(state, err)                                                   => StateExistsExpr(applyMapping(mapping, state).asInstanceOf[SpecialId], err)
      case LookupExpr(id, index)                                                         => LookupExpr(applyMapping(mapping, id), applyMapping(mapping, index))
      case LengthExpr(id)                                                                => LengthExpr(applyMapping(mapping, id))
      case CombExpr(lhs, rhs, op)                                                        => CombExpr(applyMapping(mapping, lhs), applyMapping(mapping, rhs), op)
      case UpdateMapExpr(id, update)                                                     => UpdateMapExpr(applyMapping(mapping, id), applyMapping(mapping, update))
      case mapTupleExpr @ MapTupleExpr(k, v)                                             => applyMapping(mapping, mapTupleExpr)
      case LoopIndex() | HintDecl(_) | Hint(_, _)                                        => expr
      case _: SpecialId                                                                  => expr
      case MethodCallExpr(_, _) | SetAssignExpr(_) | MapAssignExpr(_) | SeqAssignExpr(_) => throw new Exception(s"${expr.getClass().getSimpleName()} is not yet supported in the Soundness Check.")
    }
  }

  def applyMapping(mapping: Map[Id, Expr], mapTupleExpr: MapTupleExpr): MapTupleExpr = {
    MapTupleExpr(applyMapping(mapping, mapTupleExpr.k), applyMapping(mapping, mapTupleExpr.v))
  }

  def applyMapping(mapping: Map[Id, Expr], id: Id): Expr = {
    mapping.getOrElse(id, id)
  }

  def applyMapping(mapping: Map[Id, Expr], varDecl: AssertVarDecl): AssertVarDecl = {
    varDecl
  }
  def applyMapping(mapping: Map[Id, Expr], specialId: SpecialId): SpecialId = {
    specialId
  }

  def applyMapping(mapping: Map[Id, Expr], element: Element): Element = {
    element match {
      case SimpleHyperType(name)             => element
      case id @ Id(name)                     => applyMapping(mapping, id)
      case HyperTypeWithListArgs(name, args) => HyperTypeWithListArgs(name, args.map(applyMapping(mapping, _)))
      case HyperTypeWithSetArgs(name, args)  => HyperTypeWithSetArgs(name, args.map(applyMapping(mapping, _)))
    }
  }
}
