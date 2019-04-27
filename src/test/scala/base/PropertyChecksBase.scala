package base

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait PropertyChecksBase extends ScalaCheckPropertyChecks {

  implicit val propertyCheckConf = PropertyCheckConfiguration(minSuccessful = 1000, maxDiscardedFactor = 1.0)
}
