package homework3

import org.mindrot.jbcrypt.BCrypt

object PasswordUtils:
  def hash(password: String): String = BCrypt.hashpw(password, BCrypt.gensalt())
