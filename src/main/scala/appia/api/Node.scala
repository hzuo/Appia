package appia.api

trait Node[-Context] {

  def done(c: Context): Boolean

}