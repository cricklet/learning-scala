{
  // Non-strictness: a proprety of a function where the function may choose not
  // to evaluate one or more of its arguments.

  // In contrast, a strict function always evaluates its arguments.

  // Functions in Scala are, by default, strict.

  // Boolean operators are non-strict.
  // For example, neither of these evaluate the right-side.
  true || { println("!!"); false }
  false && { println("!!"); true }

  // If statements are similarly non-strict.
  if (false) println("!!")
}
