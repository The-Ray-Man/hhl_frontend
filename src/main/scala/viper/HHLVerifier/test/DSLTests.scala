package viper.HHLVerifier.test

import viper.HHLVerifier.typing.dsl.TypeSystem.loadTypeSystem

object DSLTests {

  def result(testName: String, res: fastparse.Parsed[Any]): Unit = {
    if (res.isSuccess) {
      println(s"$testName: Success")
    } else {
      println(s"$testName: Failure - ${res.asInstanceOf[fastparse.Parsed.Failure].msg}")
    }
  }

  def parsingTests(): Unit = {
    val parseSet = fastparse.parse("Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("1", parseSet)

    val parseCondition = fastparse.parse("LOW in H0", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("2", parseCondition)

    val parseConclusion = fastparse.parse("POS addTo Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.conclusion(_))
    result("3", parseConclusion)

    val parseRule = fastparse.parse("POS in Gamma(var) => POS addTo Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("4", parseRule)

    val negate = fastparse.parse("-e1", viper.HHLVerifier.typing.dsl.Parser.expression(_))
    result("5", negate)

    val negateRule = fastparse.parse("(Gamma, Delta) |- -e1 :: []", viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_))
    result("6", negateRule)

    val parseDerivationRule = fastparse.parse("(Gamma, Delta) |- e1 + e2 :: []", viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_))
    result("10", parseDerivationRule)

    val testRules = fastparse.parse("[]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("11", testRules)

    val testRulesNoneEmpty = fastparse.parse("[POS in Gamma(var) => POS addTo H]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("12", testRulesNoneEmpty)

    val testRulesNoneTwoElements = fastparse.parse("[POS in Gamma(var) => POS addTo H, POS in Gamma(var) => POS addTo H]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("13", testRulesNoneTwoElements)

    val testRuleWithSubtypeChecking = fastparse.parse(
      "[POS in Gamma(var) && POS in H[e1](Gamma, Delta) => POS addTo H, POS in Gamma(var) => POS addTo H]",
      viper.HHLVerifier.typing.dsl.Parser.expressionRules(_)
    )
    result("13", testRuleWithSubtypeChecking)

    val content     = "(Gamma, Delta) |- n :: [=> LOW addTo H]"
    val parseSystem = fastparse.parse(content, viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_))
    result("14", parseSystem)

    val content2     = """(Gamma, Delta) |- var :: []
    """
    val parseSystem2 = fastparse.parse(content2, viper.HHLVerifier.typing.dsl.Parser.specification(_))
    result("15", parseSystem2)

    val content3     = "[LOW in H[e1](Gamma, Delta) => LOW addTo H,LOW in H[e2](Gamma, Delta) => LOW addTo H]"
    val parseSystem3 = fastparse.parse(content3, viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("16", parseSystem3)

    val res17 = fastparse.parse("LOW", viper.HHLVerifier.typing.dsl.Parser.hyperType(_))
    result("17", res17)
    val res18 = fastparse.parse("LOW{a,b,c}", viper.HHLVerifier.typing.dsl.Parser.hyperType(_))
    result("18", res18)
    val res19 = fastparse.parse("LOW[a,b,c]", viper.HHLVerifier.typing.dsl.Parser.hyperType(_))
    result("19", res19)
    val res20 = fastparse.parse("LOW in H[e1](Gamma, Delta)", viper.HHLVerifier.typing.dsl.Parser.inSet(_))
    result("20", res20)

    val res21 = fastparse.parse("LOW{a,b,c} in H[e1](Gamma, Delta)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("21", res21)

    val res22 = fastparse.parse("LOW{a,b,c} in H[e1](Gamma, Delta) => LOW{a,b,c} addTo H", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("22", res22)

    val res23 = fastparse.parse("[LOW{a,b,c} in H[e1](Gamma, Delta) => LOW{a,b,c} addTo H]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("23", res23)

    val res24 = fastparse.parse(
      "[LOW{a,b,c} in H[e1](Gamma, Delta) => LOW{a,b,c} addTo H,LOW in H[e2](Gamma, Delta) => LOW{a,b,c} addTo H]",
      viper.HHLVerifier.typing.dsl.Parser.expressionRules(_)
    )
    result("24", res24)

    val res25 = fastparse.parse(
      "(Gamma, Delta) |- e1 + e2 :: [LOW{a,b,c} in H[e1](Gamma, Delta) => LOW{a,b,c} addTo H, LOW in H[e2](Gamma, Delta) => LOW{a,b,c} addTo H]",
      viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_)
    )
    result("25", res25)

    val res26 = fastparse.parse(
      "(Gamma, Delta) |- e1 * e2 :: [LOW{a,b,c} in H[e1](Gamma, Delta) => LOW{a,b,c} addTo H,\n LOW in H[e2](Gamma, Delta) => LOW{a,b,c} addTo H]",
      viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_)
    )
    result("26", res26)

    val res27 = fastparse.parse("[LOW in H[e1](Gamma, Delta) => LOW addTo H,LOW in H[e2](Gamma, Delta) => LOW addTo H]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("27", res27)

    val res28 = fastparse.parse("H[e1](Gamma, Delta)", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("28", res28)
    val res29 = fastparse.parse("LOW", viper.HHLVerifier.typing.dsl.Parser.element(_))
    result("29", res29)

    val res30 = fastparse.parse("LOW in H[e1](Gamma, Delta)", viper.HHLVerifier.typing.dsl.Parser.inSet(_))
    result("30", res30)

    val res31 = fastparse.parse("LOW in H[e1](Gamma, Delta)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("31", res31)

    val res32 = fastparse.parse("LOW in H[e1](Gamma, Delta) && LOW in H[e2](Gamma, Delta) => LOW addTo H", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("32", res32)

    val res33 = fastparse.parse("x", viper.HHLVerifier.typing.dsl.Parser.variable(_))
    result("33", res33)

    val res34 = fastparse.parse("Gamma", viper.HHLVerifier.typing.dsl.Parser.mapping(_))
    result("34", res34)

    val res35 = fastparse.parse("Gamma hasKey x", viper.HHLVerifier.typing.dsl.Parser.inMapping(_))
    result("35", res35)

    val res36 = fastparse.parse("LOW in D[e1](Gamma, Delta)(x)", viper.HHLVerifier.typing.dsl.Parser.inSet(_))
    result("36", res36)

    val res37 = fastparse.parse("D[e1](Gamma, Delta) hasKey x", viper.HHLVerifier.typing.dsl.Parser.inMapping(_))
    result("37", res37)

    val res38 = fastparse.parse("DH[s1](Gamma, Delta, Context)", viper.HHLVerifier.typing.dsl.Parser.mapping(_))
    result("38", res38)

    val res39 = fastparse.parse("DD[s1](Gamma, Delta, Context)", viper.HHLVerifier.typing.dsl.Parser.mapping(_))
    result("39", res39)

    val res40 = fastparse.parse("(Context \\ LOW)", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("40", res40)

    val res41 = fastparse.parse("Gamma'", viper.HHLVerifier.typing.dsl.Parser.gammaResult(_))
    result("41", res41)

    val res42 = fastparse.parse("Delta'", viper.HHLVerifier.typing.dsl.Parser.deltaResult(_))
    result("42", res42)

    val res43 = fastparse.parse("Gamma' = DH[s1](Gamma, Delta, Context)", viper.HHLVerifier.typing.dsl.Parser.mapEquals(_))
    result("43", res43)

    val res44 = fastparse.parse("Delta' = Delta'", viper.HHLVerifier.typing.dsl.Parser.doubleMapEquals(_))
    result("44", res44)

    val res45 = fastparse.parse("Delta' = DD[s1](Gamma, Delta, Context)", viper.HHLVerifier.typing.dsl.Parser.conclusion(_))
    result("45", res45)

    val res46 = fastparse.parse("Delta' = DD[s1](DH[s1](Gamma, Delta, Context), Delta, Context)", viper.HHLVerifier.typing.dsl.Parser.conclusion(_))
    result("46", res46)

    val res47 = fastparse.parse("!(TRUE in H[b](Gamma, Delta))", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("47", res47)

    val res48 = fastparse.parse("(Context \\ LOW)", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("48", res48)

    val res49 = fastparse.parse("!(TRUE in H[b](Gamma, Delta)) => POS addTo Gamma'(x)", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("49", res49)

    val res50 = fastparse.parse("!(TRUE in H[b](Gamma, Delta)) && !(FALSE in H[b](Gamma,Delta)) => POS addTo Gamma'(x)", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("50", res50)

    val res51 = fastparse.parse("POS in DH[s1](Gamma, Delta, (Context \\ LOW))(x)", viper.HHLVerifier.typing.dsl.Parser.inSet(_))
    result("51", res51)

    val res52 = fastparse.parse("POS in DH[s1](Gamma, Delta, (Context \\ LOW))(x)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("52", res52)

    val res53 = fastparse.parse("DH[s1](Gamma, Delta, Context)(x) = DH[s2](Gamma, Delta, Context)(x)", viper.HHLVerifier.typing.dsl.Parser.setEquals(_))
    result("53", res53)

    val res54 = fastparse.parse("DH[s1](Gamma, Delta, Context)(x) = DH[s2](Gamma, Delta, Context)(x)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("54", res54)

    val res55 = fastparse.parse(
      "DH[s1](Gamma, Delta, Context)(x) = DH[s2](Gamma, Delta, Context)(x) => (DH[s1](Gamma, Delta, Context)(x) \\ LOW) addTo Gamma'(x)",
      viper.HHLVerifier.typing.dsl.Parser.expressionRule(_)
    )
    result("55", res55)

    val res56 = fastparse.parse("(DH[s1](Gamma, Delta, Context)(x) \\ LOW)", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("56", res56)

    val res57 = fastparse.parse("Vars[x]", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("57", res57)

    val res58 = fastparse.parse("x in Vars[e1]", viper.HHLVerifier.typing.dsl.Parser.inSet(_))
    result("58", res58)

    val res59 = fastparse.parse("POS in DD[s1](Gamma, Delta, Context)(y)(y)", viper.HHLVerifier.typing.dsl.Parser.inSet(_))
    result("59", res59)

    val res60 = fastparse.parse("D[e](Gamma, Delta) = Delta'(var)", viper.HHLVerifier.typing.dsl.Parser.conclusion(_))
    result("60", res60)

    val res61 = fastparse.parse("!(Delta hasKey var)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("61", res61)

    val res62 = fastparse.parse("Delta hasKey var", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("62", res62)

    val res63 = fastparse.parse("POS in Delta(var)(var)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("63", res63)

    val res64 = fastparse.parse("ZERO addTo Delta'(var)(var)", viper.HHLVerifier.typing.dsl.Parser.conclusion(_))
    result("64", res64)
    val res65 = fastparse.parse("ZERO in D[e](Gamma, Delta)(var)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("65", res65)

    val res66 = fastparse.parse("ZERO in Delta(var)(var) && ZERO in D[e](Gamma, Delta)(var) => ZERO addTo Delta'(x)(x)", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("66", res66)

    val res69 = fastparse.parse("Delta'(var)(var)", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("69", res69)

    val res70 = fastparse.parse("Delta(x) hasKey x && ZERO in Delta(y)(x) && !(D[e](Gamma, Delta) hasKey x) && POS in D[e](Gamma, Delta)(var) => POS addTo Delta'(z)(x)", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("70", res70)

    val res71 = fastparse.parse("Delta(x) hasKey x", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("71", res71)

    val res72 = fastparse.parse("ZERO in Delta(y)(x)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("72", res72)

    val res73 = fastparse.parse("!(D[e](Gamma, Delta) hasKey x)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("73", res73)

    val res74 = fastparse.parse("POS in D[e](Gamma, Delta)(var)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("74", res74)

    val res75 = fastparse.parse("POS addTo Delta'(z)(x)", viper.HHLVerifier.typing.dsl.Parser.conclusion(_))
    result("75", res75)
  }
  def loadingTypeSystemTest(filename: String): Unit = {
    println("testing:", filename)
    val path     = s"/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/$filename"
    val stmtPath = s"/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/stmt.type"
    val _        = viper.HHLVerifier.typing.dsl.TypeSystem.loadTypeSystem(Seq(path, stmtPath))
    println("success", filename)
  }

  def main(args: Array[String]): Unit = {
    parsingTests()
    loadingTypeSystemTest("value.type")
    loadingTypeSystemTest("infFlow.type")
    loadingTypeSystemTest("deltaOnValue.type")
    loadingTypeSystemTest("mono.type")

  }
}
