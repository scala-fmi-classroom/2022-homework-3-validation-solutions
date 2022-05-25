package homework3

case class RegistrationForm(
  name: String,
  email: String,
  password: String,
  passwordConfirmation: String,
  birthYear: String,
  birthMonth: String,
  birthDay: String,
  postalCode: String
)

case class Email(user: String, domain: String)

object Email:
  def unapply(email: String): Option[(String, String)] = email.split("@", -1) match
    case Array(user, domain) if user.nonEmpty && domain.nonEmpty => Some((user, domain))
    case _ => None

case class User(
  name: String,
  email: Email,
  passwordHash: String,
  birthday: Date,
  postalCode: Option[String]
)

object UserRegistration:
  def registerUser(
    userCountryPostalCodeVerifier: String => Boolean,
    today: Date
  )(
    form: RegistrationForm
  ): Validated[RegistrationFormError, User] =
    import UserValidation.*

    (
      validateName(form.name),
      validateEmail(form.email),
      validatePassword(form.password, form.passwordConfirmation).map(PasswordUtils.hash),
      validateBirthday(today)(form.birthYear, form.birthMonth, form.birthDay),
      validatePostalCode(userCountryPostalCodeVerifier)(form.postalCode)
    ).mapN(User.apply)
