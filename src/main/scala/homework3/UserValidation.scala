package homework3

import homework3.RegistrationFormError.*

enum RegistrationFormError:
  case NameIsEmpty

  case InvalidEmail(email: String)

  case PasswordTooShort
  case PasswordRequiresGreaterSymbolVariety
  case PasswordsDoNotMatch

  case InvalidBirthdayDate(dateErrors: Chain[DateError])
  case BirthdayDateIsInTheFuture(date: Date)

  case InvalidPostalCode(code: String)

object UserValidation:
  val MinPasswordLength = 8

  def validateName(name: String): Validated[RegistrationFormError, String] =
    if name.nonEmpty then Valid(name)
    else Invalid(NameIsEmpty)

  def validateEmail(email: String): Validated[RegistrationFormError, Email] = email match
    case Email(user, domain) => Valid(Email(user, domain))
    case _ => Invalid(InvalidEmail(email))

  def validatePassword(password: String, passwordConfirmation: String): Validated[RegistrationFormError, String] =
    val validatedPasswordLength =
      if password.length >= MinPasswordLength then Valid(password)
      else Invalid(PasswordTooShort)

    val validatedPasswordVariety =
      val hasLetters = password.exists(_.isLetter)
      val hasDigits = password.exists(_.isDigit)
      val hasSpecialSymbols = password.exists(c => !c.isLetter && !c.isDigit)

      if hasLetters && hasDigits && hasSpecialSymbols then Valid(password)
      else Invalid(PasswordRequiresGreaterSymbolVariety)

    val validatedPasswordsMatch =
      if password == passwordConfirmation then Valid(password)
      else Invalid(PasswordsDoNotMatch)

    (
      validatedPasswordLength,
      validatedPasswordVariety,
      validatedPasswordsMatch
    ).zipN.map(_ => password)

  def validateBirthday(
    today: Date
  )(
    year: String,
    month: String,
    day: String
  ): Validated[RegistrationFormError, Date] =
    def validateDateIsBeforeToday(date: Date): Validated[RegistrationFormError, Date] =
      if date <= today then Valid(date)
      else Invalid(BirthdayDateIsInTheFuture(date))

    DateValidation
      .validateDate(year, month, day)
      .mapErrors(errors => InvalidBirthdayDate(errors))
      .flatMap(validateDateIsBeforeToday)

  def validatePostalCode(
    userCountryPostalCodeVerifier: String => Boolean
  )(
    postalCode: String
  ): Validated[RegistrationFormError, Option[String]] =
    if postalCode.isEmpty then Valid(None)
    else if userCountryPostalCodeVerifier(postalCode) then Valid(Some(postalCode))
    else Invalid(InvalidPostalCode(postalCode))
