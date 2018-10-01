package base

import org.scalatest.prop.PropertyChecks

trait PropertyChecksBase extends PropertyChecks {

  implicit val propertyCheckConf = PropertyCheckConfiguration(minSuccessful = 100)
}
