package turbolift.internals.engine
import turbolift.internals.interpreter.Void


private[engine] final class Frame(
  val height: Int,
  val isBase: Boolean,
):
  def copy(
    height: Int = height,
    isBase: Boolean = isBase,
  ): Frame = new Frame(
    height = height,
    isBase = isBase,
  )


private[engine] object Frame:
  def local(height: Int) = new Frame(height, false)
  def base(height: Int) = new Frame(height, true)
