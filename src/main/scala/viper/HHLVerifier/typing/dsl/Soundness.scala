package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast._
import viper.HHLVerifier.typing.{BoolType, IntType}
import viper.HHLVerifier.symbols.SymbolChecker
import viper.HHLVerifier.typing.TypeChecker
import viper.HHLVerifier.generation.Generator
import viper.HHLVerifier.management.{PrettyPrinter, ViperRunner}
import viper.silver.verifier.{Failure => ResFailure, Success => ResSuccess}
import viper.HHLVerifier.typing.dsl.ast._
import viper.HHLVerifier.typing.dsl.RuleName.humanReadableRuleName

object Soundness {
  // Current type system (used by expression builders)
  var currentTypeSystem: TypeSystem = null

  // Report of rule soundness
  var report: Map[RuleName, String] = Map.empty

  // -------- Entry points ----------------------------------------------------

  def main(args: Array[String]): Unit = {
    report = Map.empty
    // read the filename from args
    val typeSystemFiles = args.toSeq

    val ts = TypeSystem.loadTypeSystem(typeSystemFiles, false)
    check(ts)
    generateReport()
  }

  /** Prints the aggregated report to stdout. */
  def generateReport(): Unit = {
    report.toSeq.sortBy(_._1.toString()).foreach {
      case (ruleName, status) => {
        println(s"${ruleName.toString()}: $status")
      }
    }
  }

  /** Top-level driver. Builds programs for each rule and runs verification. */
  def check(ts: TypeSystem): Unit = {
    currentTypeSystem = ts

    val generated =
      ts.expressionTypeSystem.flatMap(generateProgramsForExpressionRule)

    generated.foreach { case (ruleName, program) =>
      program match {
        case Some(program) =>
          val ok = verifyProgram(ruleName, program)
          updateReport(ruleName, if (ok) "sound" else "might be unsound")
        case None =>
          updateReport(ruleName, "notChecked")
      }
    }
  }

  // -------- Verification pipeline ------------------------------------------

  /** Writes a copy of the (filtered) program to disk for inspection. */
  private def writeProgramToFile(path: String, program: HHLProgram): Unit = {
    // Filter out state-exists implications from pre/post conditions before writing.
    val sanitized = HHLProgram(program.methods.map { m =>
      val pre  = m.pre.map(stripStateExistsAntecedent)
      val post = m.post.map(stripStateExistsAntecedent)
      m.copy(pre = pre, post = post)
    })

    val content = PrettyPrinter.formatProgram(sanitized)
    val file    = new java.io.File(path)
    val writer  = new java.io.PrintWriter(file)
    try writer.write(content)
    finally writer.close()

    println(s"Wrote soundness check program to $path")
  }

  /** Runs the full Viper pipeline and returns true if verification succeeds. */
  private def verifyProgram(name: RuleName, program: HHLProgram): Boolean = {
    writeProgramToFile(s"$name.hhl", program)

    SymbolChecker.reset()
    TypeChecker.reset()
    Generator.reset()

    SymbolChecker.checkSymbolsProg(program)
    TypeChecker.typeCheckProg(program)

    val vpr               = Generator.generate(program, "")
    val consistencyErrors = vpr.checkTransitively

    if (consistencyErrors.nonEmpty) {
      consistencyErrors.foreach(err => println(err.readableMessage))
      false
    } else {
      ViperRunner.runSiliconAndCarbon(vpr) match {
        case ResSuccess       => true
        case ResFailure(errs) =>
          errs.foreach(e => println(e))
          false
      }
    }
  }

  // -------- Reporting helpers ----------------------------------------------

  private def updateReport(rule: RuleName, status: String): Unit = {
    report = report.updated(rule, status)
  }

  // -------- Rule -> Program generation -------------------------------------

  private def generateProgramsForExpressionRule(
      exprRule: ExpressionDerivationRule
  ): Seq[(RuleName, Option[HHLProgram])] = {

    exprRule.expr match {
      case bin: BinaryExpr                        => buildForBinaryExpr(bin, exprRule)
      case un: UnaryExpr                          => buildForUnaryExpr(un, exprRule)
      case Id(name) if name == "n" || name == "b" => buildForConst(Id(name), exprRule)
      case ImpliesExpr(left, right)               => buildForBinaryExpr(BinaryExpr(left, "==>", right), exprRule)
      case _                                      => exprRule.rules.map(r => (r.name, None)) // unsupported expression
    }
  }

  private def buildForConst(constId: Id, exprRule: ExpressionDerivationRule): Seq[(RuleName, Option[HHLProgram])] = {
    val (inputName, inputType) = constId.name match {
      case "n" => ("input", IntType())
      case "b" => ("input", BoolType())
    }

    val input = Id(inputName)
    input.typ = inputType

    val mapping      = Map(constId -> input)
    val rulesWithIdx = exprRule.rules.zipWithIndex
    val prePost      = rulesToTriples(rulesWithIdx, mapping, input)
    val programs     = triplesToPrograms(Seq(input), prePost)
    programs
  }

  private def buildForUnaryExpr(un: UnaryExpr, exprRule: ExpressionDerivationRule): Seq[(RuleName, Option[HHLProgram])] = {
    val (inT, outT) = un.op match {
      case "!" => (BoolType(), BoolType())
      case "-" => (IntType(), IntType())
    }

    val input = Id("input")
    input.typ = inT

    val mapping      = Map(un.e.asInstanceOf[Id] -> input)
    val rulesWithIdx = exprRule.rules.zipWithIndex
    val resultExpr   = UnaryExpr(un.op, input)

    val prePost  = rulesToTriples(rulesWithIdx, mapping, resultExpr)
    val programs = triplesToPrograms(Seq(input), prePost)

    programs
  }

  private def buildForBinaryExpr(bin: BinaryExpr, exprRule: ExpressionDerivationRule): Seq[(RuleName, Option[HHLProgram])] = {
    val (leftT, rightT, resT) = bin.op match {
      case "+" | "-" | "*" | "/" | "%"           => (IntType(), IntType(), IntType())
      case "<" | "<=" | ">" | ">=" | "==" | "!=" => (IntType(), IntType(), BoolType())
      case "&&" | "||" | "==>"                   => (BoolType(), BoolType(), BoolType())
    }

    val left  = Id("left")
    val right = Id("right")
    left.typ = leftT
    right.typ = rightT

    val mapping          = Map(bin.e1.asInstanceOf[Id] -> left, bin.e2.asInstanceOf[Id] -> right)
    val rulesWithIdx     = exprRule.rules.zipWithIndex
    val resultExpr: Expr = bin.op match {
      case "==>" => ImpliesExpr(left, right)
      case _     => BinaryExpr(left, bin.op, right)
    }

    val prePost  = rulesToTriples(rulesWithIdx, mapping, resultExpr)
    val programs = triplesToPrograms(Seq(left, right), prePost)

    programs
  }

  /** Convert (conditions, conclusions, stmts) triples to complete HHL programs. */
  private def triplesToPrograms(
      args: Seq[Id],
      triples: Seq[(RuleName, Option[(Seq[Expr], Seq[Expr], Seq[Stmt])])]
  ): Seq[(RuleName, Option[HHLProgram])] = {
    triples.map {
      case (name, Some((pres, posts, stmts))) =>
        (name, Some(HHLProgram(Seq(Method(s"${name.shortFileName}", args, Seq.empty, pres, posts, CompositeStmt(stmts))))))
      case (name, None) => (name, None)
    }
  }

  // -------- Rule translation ------------------------------------------------

  /** Translate rules into (preconditions, postconditions, assertion statements).
    *
    * A rule can be marked as not-checkable (None) if it contains constructs that are not supported by the current transformation.
    */
  private def rulesToTriples(
      rulesWithIdx: Seq[(Rule, Int)],
      templateToIds: Map[Id, Id],
      resultExpr: Expr
  ): Seq[(RuleName, Option[(Seq[Expr], Seq[Expr], Seq[Stmt])])] = {
    rulesWithIdx.map { case (rule, idx) =>
      var pres         = Seq.empty[Expr]
      var asserts      = Seq.empty[Stmt]
      var posts        = Seq.empty[Expr]
      var notCheckable = false

      rule.conditions.foreach { c =>
        transformCondition(c, templateToIds) match {
          case Some((true, e))  => pres :+= e
          case Some((false, e)) => asserts :+= AssumeStmt(e)
          case None             => notCheckable = true
        }
      }

      rule.conclusions.foreach { conc =>
        transformConclusion(conc, resultExpr, templateToIds) match {
          case Some(e) => posts :+= e
          case None    => notCheckable = true
        }
      }

      if (notCheckable) (rule.name, None)
      else (rule.name, Some((pres, posts, asserts)))
    }
  }

  /** Transform a DSL condition to an AST expression.
    * @return
    *   Some((isPrecondition, expr)) or None if unsupported.
    */
  private def transformCondition(condition: Condition, templateToIds: Map[Id, Id]): Option[(Boolean, Expr)] = condition match {
    case InSet(ht: HyperType, HyperTypeCheck(template, Gamma(), Delta())) =>
      val id = templateToIds.getOrElse(template, sys.error("Missing template mapping"))
      Some(true -> buildExpression(id, ht))

    case ArithCondition(template, op, num) =>
      val id = templateToIds.getOrElse(template, sys.error("Missing template mapping"))
      Some(false -> BinaryExpr(id, op, Num(num)))

    case BoolCondition(template) =>
      val id = templateToIds.getOrElse(template, sys.error("Missing template mapping"))
      Some(false -> id)

    case NotOperator(inner) =>
      transformCondition(inner, templateToIds) match {
        case Some((true, _))     => None // negated precondition not supported
        case Some((false, expr)) => Some(false -> UnaryExpr("!", expr))
        case None                => None
      }

    case _ => None
  }

  /** Transform a DSL conclusion to an AST postcondition expression. */
  private def transformConclusion(conclusion: Conclusion, resultExpr: Expr, templateToIds: Map[Id, Id]): Option[Expr] = conclusion match {
    case AddToSet(ht: HyperType, HyperCollectionResult()) => Some(buildExpression(resultExpr, ht))
    case _                                                => None
  }

  // -------- Expression builders / mappings ---------------------------------

  /** Convert a (expr, HyperType) pair to a concrete AST expression via the current type system. */
  private def buildExpression(expr: Expr, ht: HyperType): Expr = {
    val candidates: Seq[(HyperTypeDeclaration, Map[Id, Id])] =
      currentTypeSystem.hyperTypeDeclaration
        .map(decl => decl -> getMapping(decl.hty, ht))
        .collect { case (decl, Some(m)) => decl -> m }

    if (candidates.length != 1)
      throw new Exception(s"Expected exactly one matching HyperType declaration for $ht, found ${candidates.length}.")

    val (decl, mapping)            = candidates.head
    val fullMapping: Map[Id, Expr] = mapping + (decl.variable -> expr)

    val mapped = applyMapping(fullMapping, decl.definition)
    propagateStateLookups(mapped)
  }

  /** Convert an (Id, HyperType) pair to a concrete AST expression. */
  def buildExpression(id: Id, ht: HyperType): Expr = {
    val candidates: Seq[(HyperTypeDeclaration, Map[Id, Id])] =
      currentTypeSystem.hyperTypeDeclaration
        .map(decl => decl -> getMapping(decl.hty, ht))
        .collect { case (decl, Some(m)) => decl -> m }

    if (candidates.length != 1)
      throw new Exception(s"Expected exactly one matching HyperType declaration for $ht, found ${candidates.length}.")

    val (decl, mapping)            = candidates.head
    val fullMapping: Map[Id, Expr] = mapping + (decl.variable -> id)
    applyMapping(fullMapping, decl.definition)
  }

  /** Attempt to map one DSL element onto another; returns a variable mapping if compatible. */
  private def getMapping(from: Element, to: Element): Option[Map[Id, Id]] = (from, to) match {
    case (SimpleHyperType(fn), SimpleHyperType(tn)) if fn == tn => Some(Map.empty)

    case (HyperTypeWithListArgs(fn, fArgs), HyperTypeWithListArgs(tn, tArgs)) if fn == tn && fArgs.length == tArgs.length =>
      val pairs = fArgs.zip(tArgs).map { case (fa, ta) => getMapping(fa, ta) }
      if (pairs.forall(_.isDefined)) Some(pairs.flatten.flatten.toMap) else None

    case (HyperTypeWithSetArgs(fn, fArgs), HyperTypeWithSetArgs(tn, tArgs)) if fn == tn && fArgs.size == tArgs.size =>
      val pairs = fArgs.zip(tArgs).map { case (fa, ta) => getMapping(fa, ta) }
      if (pairs.forall(_.isDefined)) Some(pairs.flatten.flatten.toMap) else None

    case _ => None
  }

  // -------- AST rewriting ---------------------------------------------------

  /** True if the expression only contains StateExistsExpr nodes (modulo boolean structure). */
  private def containsOnlyStateExists(expr: Expr): Boolean = expr match {
    case StateExistsExpr(_, _) => true
    case BinaryExpr(l, _, r)   => containsOnlyStateExists(l) && containsOnlyStateExists(r)
    case UnaryExpr(_, e)       => containsOnlyStateExists(e)
    case _                     => false
  }

  /** Remove an implication whose antecedent is solely built from StateExistsExpr. */
  private def stripStateExistsAntecedent(expr: Expr): Expr = expr match {
    case Assertion(q, decls, body) =>
      val newBody = body match {
        case ImpliesExpr(left, right) if containsOnlyStateExists(left) => right
        case _                                                         => body
      }
      Assertion(q, decls, newBody)
    case _ => expr
  }

  /** Push a SpecialId (state) lookup through the expression. */
  private def insideState(state: SpecialId, e: Expr): Expr = e match {
    case id: Id               => LookupExpr(state, id)
    case n: Num               => n
    case b: BoolLit           => b
    case BinaryExpr(l, op, r) => BinaryExpr(insideState(state, l), op, insideState(state, r))
    case UnaryExpr(op, sub)   => UnaryExpr(op, insideState(state, sub))
    case ImpliesExpr(l, r)    => ImpliesExpr(insideState(state, l), insideState(state, r))
    case other                => throw new Exception(s"Unsupported expression type inside state: ${other.getClass.getSimpleName}")
  }

  /** After mapping, convert any LookupExpr with AssertVar into a state-indexed form. */
  private def propagateStateLookups(e: Expr): Expr = e match {
    case id: Id                         => id
    case n: Num                         => n
    case b: BoolLit                     => b
    case BinaryExpr(l, op, r)           => BinaryExpr(propagateStateLookups(l), op, propagateStateLookups(r))
    case UnaryExpr(op, sub)             => UnaryExpr(op, propagateStateLookups(sub))
    case ImpliesExpr(l, r)              => ImpliesExpr(propagateStateLookups(l), propagateStateLookups(r))
    case Assertion(q, decls, body)      => Assertion(q, decls, propagateStateLookups(body))
    case StateExistsExpr(s, err)        => StateExistsExpr(s, err)
    case LookupExpr(av: AssertVar, idx) => insideState(av, idx)
    case other                          => throw new Exception(s"Unsupported expression type: ${other.getClass.getSimpleName}")
  }

  // -------- Mapping over different AST nodes --------------------------------

  private def applyMapping(mapping: Map[Id, Expr], e: Expr): Expr = e match {
    case id: Id                                                                        => applyMapping(mapping, id)
    case n: Num                                                                        => n
    case b: BoolLit                                                                    => b
    case BinaryExpr(l, op, r)                                                          => BinaryExpr(applyMapping(mapping, l), op, applyMapping(mapping, r))
    case UnaryExpr(op, sub)                                                            => UnaryExpr(op, applyMapping(mapping, sub))
    case ImpliesExpr(l, r)                                                             => ImpliesExpr(applyMapping(mapping, l), applyMapping(mapping, r))
    case Assertion(q, decls, body)                                                     => Assertion(q, decls.map(d => applyMapping(mapping, d)), applyMapping(mapping, body))
    case StateExistsExpr(s, err)                                                       => StateExistsExpr(applyMapping(mapping, s).asInstanceOf[SpecialId], err)
    case LookupExpr(id, idx)                                                           => LookupExpr(applyMapping(mapping, id), applyMapping(mapping, idx))
    case LengthExpr(id)                                                                => LengthExpr(applyMapping(mapping, id))
    case CombExpr(l, r, op)                                                            => CombExpr(applyMapping(mapping, l), applyMapping(mapping, r), op)
    case UpdateMapExpr(id, update)                                                     => UpdateMapExpr(applyMapping(mapping, id), applyMapping(mapping, update))
    case mt: MapTupleExpr                                                              => applyMapping(mapping, mt)
    case LoopIndex() | HintDecl(_) | Hint(_, _)                                        => e
    case _: SpecialId                                                                  => e
    case MethodCallExpr(_, _) | SetAssignExpr(_) | MapAssignExpr(_) | SeqAssignExpr(_) =>
      throw new Exception(s"${e.getClass.getSimpleName} is not yet supported in the Soundness Check.")
  }

  private def applyMapping(mapping: Map[Id, Expr], mt: MapTupleExpr): MapTupleExpr =
    MapTupleExpr(applyMapping(mapping, mt.k), applyMapping(mapping, mt.v))

  private def applyMapping(mapping: Map[Id, Expr], id: Id): Expr =
    mapping.getOrElse(id, id)

  private def applyMapping(mapping: Map[Id, Expr], decl: AssertVarDecl): AssertVarDecl = decl

  private def applyMapping(mapping: Map[Id, Expr], sid: SpecialId): SpecialId = sid

  private def applyMapping(mapping: Map[Id, Expr], el: Element): Element = el match {
    case s @ SimpleHyperType(_)         => s
    case id: Id                         => applyMapping(mapping, id)
    case HyperTypeWithListArgs(n, args) => HyperTypeWithListArgs(n, args.map(applyMapping(mapping, _)))
    case HyperTypeWithSetArgs(n, args)  => HyperTypeWithSetArgs(n, args.map(applyMapping(mapping, _)))
  }
}
