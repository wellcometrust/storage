package weco.storage_service.fixtures

import scala.reflect.runtime.universe._

trait ReflectionHelpers {
  // See https://stackoverflow.com/q/34534002/1558022
  def getSubclasses[T: TypeTag]: Set[Symbol] = {
    val tpe = typeOf[T]
    val clazz = tpe.typeSymbol.asClass

    require(clazz.isSealed)
    require(clazz.isTrait)

    clazz.knownDirectSubclasses
  }
}
