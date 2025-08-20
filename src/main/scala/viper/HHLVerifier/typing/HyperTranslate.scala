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

object HyperTranslate {

  def translateProgram(program: HHLProgram): HHLProgram = {

    HHLProgram(
      methods = program.methods.map(translateMethod)
    )
  }

  def translateMethod(method: Method): Method = {

    Method(
      method.mName,
      params = method.params,
      res = method.res,
      pre = method.pre,
      post = method.post,
      body = CompositeStmt(method.body.stmts.map(translateStmt))
    )
  };

  def translateStmt(stmt: Stmt): Stmt = {

    stmt match {
      case UnfoldStmt(t, id) => {
        val semantics = t.semantics(id)
        HyperAssumeStmt(semantics);
      }
      case FoldStmt(t, id) => {
        val semantics = t.semantics(id)
        HyperAssertStmt(semantics);
      }
      case _ => stmt
    }
  };
}
