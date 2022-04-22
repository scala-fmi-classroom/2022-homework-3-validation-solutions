package homework3

import homework3.io.Console.*
import homework3.io.IO

object UserRegistrationApp:
  val validPostalCodes: String => Boolean = Set("1000", "1164", "9000")
  val today = Date(2020, 1, 1)
  val register = UserRegistration.registerUser(validPostalCodes, today)(_)

  val registrationInput: IO[RegistrationForm] = for
    name <- promptInput("Enter your name:")
    email <- promptInput("Enter your email:")
    password <- promptInput("Enter your password:")
    passwordConfirmation <- promptInput("Confirm your password:")
    date <- dateInput("Enter your birthday:")
    (year, month, day) = date // We cannot use pattern matching on the line above because IO doesn't have withFilter
    postalCode <- promptInput("Enter your postal code (optional):")
  yield RegistrationForm(name, email, password, passwordConfirmation, year, month, day, postalCode)

  def promptInput(prompt: String): IO[String] = for
    _ <- putStrLn(prompt)
    input <- getStrLn
  yield input

  def dateInput(prompt: String): IO[(String, String, String)] = for
    _ <- putStrLn(prompt)
    year <- promptInput("year:")
    month <- promptInput("month:")
    day <- promptInput("day:")
  yield (year, month, day)

  def registrationOutput(userValidation: Validated[RegistrationFormError, User]): IO[Unit] =
    def onSuccess(user: User) = for
      _ <- putStrLn("User registered successfully:")
      _ <- putStrLn(user.toString)
    yield ()

    def onFailure(errors: Chain[RegistrationFormError]) =
      errors
        .map(errorToDescription)
        .map(putStrLn)
        .foldLeft(putStrLn("The following errors have been found:")) { (acc, next) =>
          acc.flatMap(_ => next)
        }

    userValidation.fold(onFailure, onSuccess)

  def errorToDescription(error: RegistrationFormError): String =
    import RegistrationFormError.*
    import DateError.*

    error match
      case NameIsEmpty => "Name is empty"
      case InvalidEmail(email) => s"Provided email $email is not valid"
      case PasswordTooShort => "Password is too short. It should be at least 8 symbols long"
      case PasswordRequiresGreaterSymbolVariety =>
        "Password should contain at least one letter, digit and a special symbol"
      case PasswordsDoNotMatch => "Passwords do not match"
      case InvalidBirthdayDate(dateErrors) =>
        val errors = dateErrors.map {
          case YearIsNotAnInteger(year) => s"Year $year is not an integer"
          case MonthIsNotAnInteger(month) => s"Month $month is not an integer"
          case DayIsNotAnInteger(day) => s"Day $day is not an integer"

          case MonthOutOfRange(month) => s"Month $month is out of range"
          case DayOutOfRange(day) => s"Day $day is out of range"

          case InvalidDate(year, month, day) => s"Date $year-$month-$day is not a valid date"
        }.toList

        ("Provided birthday date is invalid:" :: errors).mkString("\n")
      case BirthdayDateIsInTheFuture(date) => s"Provided birthday date '$date' is in the future"
      case InvalidPostalCode(code) => s"Provided postal code '$code' is invalid"

  def promptBoolean(prompt: String): IO[Boolean] = promptInput(prompt) flatMap {
    case "true" | "yes" | "t" | "y" => IO.of(true)
    case "false" | "no" | "f" | "n" => IO.of(false)
    case _ => promptBoolean(prompt)
  }

  def loop(activity: IO[?]): IO[Unit] = for
    _ <- activity
    shouldContinue <- promptBoolean("Continue with another registration?")
    _ <- if shouldContinue then loop(activity) else putStrLn("Have a good day :)!")
  yield ()

  def main(args: Array[String]): Unit =
    val registration = registrationInput
      .map(register)
      .flatMap(registrationOutput)

    val program = args match
      case Array("--loop") => loop(registration)
      case _ => registration

    program.unsafeRun()
