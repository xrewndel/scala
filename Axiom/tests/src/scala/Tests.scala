package tests.axiom

import org.scalatest.Suites

class Tests extends Suites(
  new RawAPISpec,
  new APISpec
)