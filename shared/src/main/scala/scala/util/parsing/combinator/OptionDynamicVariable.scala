package scala.util.parsing.combinator


class OptionDynamicVariable[T] {
  private val tl = new InheritableThreadLocal[Option[T]] {
    override def initialValue = None
  }

  /** Retrieve the current value */
  def value: Option[T] = tl.get

  /** Set the value of the variable while executing the specified
    * thunk.
    *
    * @param newval The value to which to set the variable
    * @param thunk The code to evaluate under the new setting
    */
  def withValue[S](newval: Option[T])(thunk: => S): S = {
    val oldval = value
    tl set newval

    try thunk
    finally{
      if(oldval.isEmpty) tl.remove()
      else tl set oldval
    }
  }

  /** Change the currently bound value, discarding the old value.
    * Usually withValue() gives better semantics.
    */
  def value_=(newval: Option[T]) = tl set newval

  override def toString: String = "OptionDynamicVariable(" + value + ")"
}
