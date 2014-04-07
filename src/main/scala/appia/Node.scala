package appia

trait Node {

  type Context

  def done(c: Context): Boolean

}