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

enum RegistrationFormError:
  case NameIsEmpty

  case InvalidEmail(email: String)

  case PasswordTooShort
  case PasswordRequiresGreaterSymbolVariety
  case PasswordsDoNotMatch

  case InvalidBirthdayDate(dateErrors: Chain[DateError])
  case BirthdayDateIsInTheFuture(date: Date)

  case InvalidPostalCode(code: String)

enum DateError:
  case YearIsNotAnInteger(year: String)
  case MonthIsNotAnInteger(month: String)
  case DayIsNotAnInteger(day: String)
  case MonthOutOfRange(month: Int)
  case DayOutOfRange(day: Int)
  case InvalidDate(year: Int, month: Int, day: Int)

case class Email(user: String, domain: String)

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
  ): Validated[RegistrationFormError, User] = ???
