package base

import org.scalatest.prop.PropertyChecks

class PropertyChecksBase extends PropertyChecks{

  implicit val propertyCheckConf = PropertyCheckConfiguration(minSuccessful = 100)
}
