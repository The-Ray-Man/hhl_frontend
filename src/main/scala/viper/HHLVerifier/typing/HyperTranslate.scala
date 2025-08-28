package viper.HHLVerifier.typing

import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.ast.HyperAssertStmt
import viper.HHLVerifier.ast.FoldStmt
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.UnfoldStmt
import viper.HHLVerifier.ast.HyperAssumeStmt
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.typing.dsl.TypeSystem
import viper.HHLVerifier.ast.IfElseStmt
import viper.HHLVerifier.ast.WhileLoopStmt

/** `HyperTranslate` removes `fold` and `unfold` expression from a program and replaces them with a `hyperAssume` and `hyperAssert` statement. Since these new statements are not yet type checked one needs to run the typechecker afterwards.
  */
object HyperTranslate {

  def translateProgram(typeSystem: TypeSystem, program: HHLProgram): HHLProgram = {

    HHLProgram(
      methods = program.methods.map(method => translateMethod(typeSystem, method))
    )
  }

  def translateMethod(typeSystem: TypeSystem, method: Method): Method = {

    Method(
      method.mName,
      params = method.params,
      res = method.res,
      pre = method.pre,
      post = method.post,
      body = CompositeStmt(method.body.stmts.map(stmt => translateStmt(typeSystem, stmt)))
    )
  };

  def translateStmt(typeSystem: TypeSystem, stmt: Stmt): Stmt = {

    stmt match {
      case UnfoldStmt(t, id) => {
        val semantics = t.semantics(typeSystem, id)
        HyperAssumeStmt(semantics);
      }
      case FoldStmt(t, id) => {
        val semantics = t.semantics(typeSystem, id)
        HyperAssertStmt(semantics);
      }
      case CompositeStmt(stmts)                       => CompositeStmt(stmts.map(s => translateStmt(typeSystem, s)))
      case IfElseStmt(cond, ifStmt, elseStmt)         => IfElseStmt(cond, translateStmt(typeSystem, ifStmt).asInstanceOf[CompositeStmt], translateStmt(typeSystem, elseStmt).asInstanceOf[CompositeStmt])
      case WhileLoopStmt(cond, body, inv, decr, rule) => WhileLoopStmt(cond, translateStmt(typeSystem, body).asInstanceOf[CompositeStmt], inv, decr, rule)
      case _                                          => stmt
    }
  };
}
