package operations

/**
 * Created by jessa on 8/12/16.
 *
 */
abstract class OperationCompletionType

case object Complete extends OperationCompletionType
case object Minimal extends OperationCompletionType
case class MaxDepth(k:Int) extends OperationCompletionType
