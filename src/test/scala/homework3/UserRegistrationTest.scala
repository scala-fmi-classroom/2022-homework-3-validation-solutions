package homework3

import homework3.UserRegistration.registerUser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import RegistrationFormError.*
import DateError.*

class UserRegistrationTest extends AnyFlatSpec with Matchers:
  "An empty form" should "generate errors for the non optional fields" in {
    val emptyForm = RegistrationForm("", "", "", "", "", "", "", "")

    val validation = registerUser(Set.empty, Date(2020, 1, 1))(emptyForm)

    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet = errors.toSet
    val birthdayErrors = errorsSet.collectFirst { case InvalidBirthdayDate(dateErrors) =>
      dateErrors.toSet
    }

    errorsSet should have size 5

    errorsSet should contain allOf (
      NameIsEmpty,
      InvalidEmail(""),
      PasswordTooShort,
      PasswordRequiresGreaterSymbolVariety
    )

    birthdayErrors shouldBe Some(
      Set(
        YearIsNotAnInteger(""),
        MonthIsNotAnInteger(""),
        DayIsNotAnInteger("")
      )
    )
  }
