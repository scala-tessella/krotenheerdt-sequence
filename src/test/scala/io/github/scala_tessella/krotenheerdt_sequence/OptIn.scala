package io.github.scala_tessella.krotenheerdt_sequence

/** Guard helper for the opt-in runs.
  *
  * Asserting on `sys.props.contains("certs")` directly would print the ENTIRE system-properties map when the
  * guard cancels a test — which buries the run. Going through a named predicate keeps ScalaTest's diagram
  * down to `enabled("certs") was false`.
  */
object OptIn:

  /** True iff the run was started with `-D<prop>` (any value, including empty). */
  def enabled(prop: String): Boolean = sys.props.contains(prop)
