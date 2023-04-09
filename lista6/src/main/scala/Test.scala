object Test extends App {
  def and[A](x: A, y: A): Boolean = {
    if (x != null) {
      if (y != null) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }
}
