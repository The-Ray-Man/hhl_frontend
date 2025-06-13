package viper.HHLVerifier.typing

import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.ast.HyperAssertStmt
import viper.HHLVerifier.ast.FoldStmt
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.UnfoldStmt
import viper.HHLVerifier.ast.HyperAssumeStmt

object HyperTranslate {

    def translateProgram(program : HHLProgram) : HHLProgram = {
    
        HHLProgram(
            methods = program.methods.map(translateMethod),
        )
    }

    def translateMethod(method: Method) : Method = {

        Method(
            method.mName,
            params = method.params,
            res = method.res,
            pre = method.pre,
            post = method.post,
            body = CompositeStmt(method.body.stmts.map(translateStmt)),
        )
    };

    def translateStmt(stmt: Stmt) : Stmt = {

        stmt match {
            case UnfoldStmt(t, id) => {
                val semantics = t match {
                    case Low() => Low().semantic(id)
                    case _ => {throw new Exception("Unfolding a hyper type that is not low is not supported.")}
                }
                HyperAssumeStmt(semantics);
            }
            case FoldStmt(t, id) => {
                val semantics = t match {
                    case Low() => Low().semantic(id)
                    case _ => {throw new Exception("Folding a hyper type that is not low is not supported.")}
                }
                HyperAssertStmt(semantics);
            }
            case _ => stmt
        }
    };
}