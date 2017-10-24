package chapter12

import java.text.SimpleDateFormat
import java.util.Date

import scala.util.control.NonFatal

case class WebForm(name: String, birthdate: Date, phoneNumber: String)

object WebForm {

  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    Applicative.validationApplicative[String]
      .map3( validName(name), validBirthdate(birthdate), validPhone(phone))( WebForm(_,_,_))

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name) else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case NonFatal(_) => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else
      Failure("Phone number must be 10 digits")
}