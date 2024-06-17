import java.time.{LocalDate, ZonedDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.{Calendar, Date}
import java.io.PrintWriter

import scala.io.Source
import scala.util.{Try, Success, Failure}

import play.api.libs.json._
import play.api.libs.functional.syntax._


case class UpdatedProduct(skuCode: String, price: Int, quantity: Int, taxAmt: Int, paidAmount: Int)
case class Product(skuCode: String, price: Int, quantity: Int, taxAmt: Int)

case class UpdatedBill(date: String, products: List[UpdatedProduct], discount: Int, boughtForBirthday: Boolean, grossTotal: Int, payableAmt: Int)
case class Bill(date: String, products: List[Product], discount: String)

case class UpdatedPerson(name: String, dob: String, bills: List[UpdatedBill], age: Int, ltv: Int, lastBillDate: Date)
case class Person(name: String, dob: String, bills: List[Bill])

object DateParser {
  val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

  def parseDate(dateString: String): Date = {
    def toDate(localDate: LocalDate): Date = {
      val calendar = Calendar.getInstance()
      calendar.set(localDate.getYear, localDate.getMonthValue - 1, localDate.getDayOfMonth)
      calendar.getTime
    }

    Try(LocalDate.parse(dateString, localDateFormatter)).orElse {
      Try(ZonedDateTime.parse(dateString, zonedDateTimeFormatter).toLocalDate)
    } match {
      case Success(localDate) => toDate(localDate)
      case Failure(_)         => throw new IllegalArgumentException("Invalid date format")
    }
  }
}

object Main extends App {
  val inputFile = "/home/dharshu/scala/task_temp/src/resources/data.json"
  val calc = new Calculations

  implicit val productFormat: Reads[Product] = (
    (__ \ "skuCode").read[String] and
    (__ \ "price").read[Int] and
    (__ \ "quantity").read[Int] and
    (__ \ "taxAmt").read[Int]
  )(Product.apply)

  implicit val billFormat: Reads[Bill] = (
    (__ \ "date").read[String] and
    (__ \ "products").read[List[Product]] and
    (__ \ "discount").read[String]
  )(Bill.apply)

  implicit val personFormat: Reads[Person] = (
    (__ \ "name").read[String] and
    (__ \ "dob").read[String] and
    (__ \ "bills").read[List[Bill]]
  )(Person.apply)

  val content = Source.fromFile(inputFile).mkString
  val jsValue = Json.parse(content)
  val jsPerson = jsValue.validate[Person]

  println("The given json file is imported")

  val person = jsPerson match {
    case JsSuccess(per, _) => per
    case JsError(errors) => throw new IllegalArgumentException("Invalid JSON format")
  }

  val updatedPerson: UpdatedPerson = {
    val lastRecentDate = calc.lastRecentDate(person.bills.map(bill => DateParser.parseDate(bill.date)))
    val age = calc.age(person.dob, lastRecentDate)
    val updatedBills = person.bills.map { bill =>
      val isBirthdayPurchase = calc.isBoughtForBirthday(person.dob, bill.date)
      val grossTotal = bill.products.map(prod => (prod.price * prod.quantity) + prod.taxAmt).sum

      val updatedProducts = bill.products.map { prod =>
        val discount = bill.discount.stripSuffix("%").toInt
        val discountAmount = (discount / 100.0) * (prod.price * prod.quantity)
        val amount = ((prod.price * prod.quantity) + prod.taxAmt) - discountAmount
        UpdatedProduct(prod.skuCode, prod.price, prod.quantity, prod.taxAmt, amount.toInt)
      }

      val payableAmt = updatedProducts.map(_.paidAmount).sum
      UpdatedBill(bill.date, updatedProducts, bill.discount.stripSuffix("%").toInt, isBirthdayPurchase, grossTotal, payableAmt)
    }
    val ltv = updatedBills.map(_.payableAmt).sum
    UpdatedPerson(person.name, person.dob, updatedBills, age, ltv, lastRecentDate)
  }

  
  implicit val updatedProductWrites: Writes[UpdatedProduct] = new Writes[UpdatedProduct] {
    def writes(product: UpdatedProduct): JsObject = Json.obj(
      "skuCode" -> product.skuCode,
      "price" -> product.price,
      "quantity" -> product.quantity,
      "taxAmt" -> product.taxAmt,
      "paidAmount" -> product.paidAmount
    )
  }

  implicit val updatedBillWrites: Writes[UpdatedBill] = new Writes[UpdatedBill] {
    def writes(bill: UpdatedBill): JsObject = Json.obj(
      "date" -> bill.date,
      "products" -> Json.toJson(bill.products),
      "discount" -> s"${bill.discount}%",
      "boughtForBirthday" -> bill.boughtForBirthday,
      "grossTotal" -> bill.grossTotal,
      "payableAmt" -> bill.payableAmt
    )
  }

  implicit val updatedPersonWrites: Writes[UpdatedPerson] = new Writes[UpdatedPerson] {
  def writes(person: UpdatedPerson): JsObject = {
    val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    val formattedLastBillDate = ZonedDateTime.ofInstant(person.lastBillDate.toInstant, ZoneId.systemDefault()).format(dateFormatter)

    Json.obj(
      "name" -> person.name,
      "dob" -> person.dob,
      "bills" -> Json.toJson(person.bills),
      "age" -> person.age,
      "ltv" -> person.ltv,
      "lastBillDate" -> formattedLastBillDate
    )
  }
}


  val updatedPersonJson: JsValue = Json.toJson(updatedPerson)

  val outputFile = "/home/dharshu/scala/task_temp/src/resources/result.json"
  val writer = new PrintWriter(outputFile)
  try {
    writer.write(Json.prettyPrint(updatedPersonJson))
  } finally {
    writer.close()
  }

  println(s"Using the given json generated all the fields and convert it back another json file")
}

class Calculations {

  val calendar = Calendar.getInstance()
  def getDate(date: Date, isBefore: Boolean): Date = {
    calendar.setTime(date)
    if (isBefore) calendar.add(Calendar.DAY_OF_YEAR, -30)
    else calendar.add(Calendar.DAY_OF_YEAR, 30)
    calendar.getTime
  }

  def isBoughtForBirthday(dob: String, purchaseDate: String): Boolean = {
    val date1 = DateParser.parseDate(dob)
    val date2 = DateParser.parseDate(purchaseDate)

    calendar.setTime(date1)
    calendar.set(Calendar.YEAR, date2.getYear + 1900)

    val beforeDob = getDate(calendar.getTime, true)
    val afterDob = getDate(calendar.getTime, false)

    date2.compareTo(beforeDob) >= 0 && date2.compareTo(afterDob) <= 0
  }

  def lastRecentDate(dates: List[Date]) = {
    dates.maxBy(_.getTime())
  }

  def age(dob: String, lastBillDate: Date): Int = {
    val date1 = DateParser.parseDate(dob)

    calendar.setTime(date1)
    
    calendar.setTime(lastBillDate)

    calendar.get(Calendar.YEAR) - calendar.get(Calendar.YEAR)

  }
}
