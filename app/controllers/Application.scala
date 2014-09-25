package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.iteratee.Enumerator
import domain.user._
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.format.DateTimeFormat

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready. Run commands on /api/v1"))
    //implicit request => Ok("Got request [" + request + "]")
  }
  
implicit val userIDWrites: Writes[Int] = new Writes[Int] {
    def writes(i:Int): JsValue = {
        return JsString("u-"+i.toString)
    }
}

implicit val cardIDWrites: Writes[Int] = new Writes[Int] {
    def writes(i:Int): JsValue = {
        return JsString("c-"+i.toString)
    }
}

implicit val webIDWrites: Writes[Int] = new Writes[Int] {
    def writes(i:Int): JsValue = {
        return JsString("l-"+i.toString)
    }
}

implicit val bankIDWrites: Writes[Int] = new Writes[Int] {
    def writes(i:Int): JsValue = {
        return JsString("b-"+i.toString)
    }
}

private lazy val ISODateTimeFormatter = ISODateTimeFormat.dateTime.withZone(DateTimeZone.UTC)
private lazy val ISODateTimeParser = ISODateTimeFormat.dateTimeParser

implicit val cardJodaDateReads = Reads.jodaDateReads("MM-dd-yyyy")
implicit val cardJodaDateWrites = Writes.jodaDateWrites("MM-dd-yyyy")
  
implicit val DateTimeFormatter = new Format[DateTime] {
    def reads(j: JsValue) = JsSuccess(ISODateTimeParser.parseDateTime(j.as[String]))
    def writes(o: DateTime): JsValue = JsString(ISODateTimeFormatter.print(o))
  } 
  
implicit val jodaTimeWrites: Writes[DateTime] = new Writes[DateTime] {
    def writes(c: DateTime): JsValue = {
      return DateTimeFormatter.writes(c)
    }
  }
  
  // Read obj -> write JSON
implicit val userWrites: Writes[User] = (
    (JsPath \ "user_id").writeNullable[Int](userIDWrites) and
    (JsPath \ "email").write[String] and
    (JsPath \ "password").write[String] and
    (JsPath \ "name").writeNullable[String] and
    (JsPath \ "created_at").writeNullable[DateTime](jodaTimeWrites) and
    (JsPath \ "updated_at").writeNullable[DateTime](jodaTimeWrites)
)(unlift(User.unapply))

implicit val cardWrites: Writes[IDCard] = (
    (JsPath \ "card_id").writeNullable[Int](cardIDWrites) and
    (JsPath \ "card_name").write[String] and
    (JsPath \ "card_number").write[String] and
    (JsPath \ "expiration_date").writeNullable[DateTime](cardJodaDateWrites)
)(unlift(IDCard.unapply))

implicit val webWrites: Writes[WebLogin] = (
    (JsPath \ "login_id").writeNullable[Int](webIDWrites) and
    (JsPath \ "url").write[String] and
    (JsPath \ "login").write[String] and
    (JsPath \ "password").write[String] 
)(unlift(WebLogin.unapply))  
    
implicit val bankWrites: Writes[BankAccount] = (
    (JsPath \ "ba_id").writeNullable[Int](bankIDWrites) and
    (JsPath \ "account_name").writeNullable[String] and
    (JsPath \ "routing_number").write[String] and
    (JsPath \ "account_number").write[String] 
)(unlift(BankAccount.unapply))

// Input JSON, output Object
implicit val userReads: Reads[User] = (
    (JsPath \ "user_id").readNullable[Int] and
    (JsPath \ "email").read[String] and
    (JsPath \ "password").read[String] and
    (JsPath \ "name").readNullable[String] and
    (JsPath \ "created_at").readNullable[DateTime] and
    (JsPath \ "updated_at").readNullable[DateTime] 
)(User.apply _)

implicit val cardReads: Reads[IDCard] = (
    (JsPath \ "card_id").readNullable[Int] and
    (JsPath \ "card_name").read[String] and
    (JsPath \ "card_number").read[String] and
    (JsPath \ "expiration_date").readNullable[DateTime](cardJodaDateReads)
)(IDCard.apply _)

implicit val webReads: Reads[WebLogin] = (
    (JsPath \ "login_id").readNullable[Int] and
    (JsPath \ "url").read[String] and
    (JsPath \ "login").read[String] and
    (JsPath \ "password").read[String] 
)(WebLogin.apply _)

implicit val bankReads: Reads[BankAccount] = (
    (JsPath \ "ba_id").readNullable[Int] and
    (JsPath \ "account_name").readNullable[String] and
    (JsPath \ "routing_number").read[String] and
    (JsPath \ "account_number").read[String] 
)(BankAccount.apply _)
  
  // Utility functions
  def getIntIdValue(string_id:String): Int = {
      return string_id.drop(2).toInt
  }
  
  // User
  def getUser(user_id:String) = Action { request =>
    val modifiedSince:DateTime = DateTime.parse(request.headers.get("If-Modified-Since").getOrElse("1970-01-01T00:00:00.000Z"))
    val user:User = User.getUser(getIntIdValue(user_id)).getOrElse(null)
    val lastModified:DateTime = user.updated_at.getOrElse(user.created_at.getOrElse(new DateTime()))
    if  (modifiedSince.compareTo(lastModified) >= 0) {
        NotModified
    } else {
        val json = Json.toJson(user)
        Ok(json).withHeaders(
            LAST_MODIFIED -> DateTimeFormatter.writes(lastModified).toString()
            )
    }
    
  }
  
  def updateUser (user_id:String) = Action(BodyParsers.parse.json) { request =>
    val userResult = request.body.validate[User]
    userResult.fold(
    errors => {
        Result(
            header = ResponseHeader(400, Map(CONTENT_TYPE->"application/json")),
            body = Enumerator(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)).toString.getBytes())
            )
      //BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)))
    },
    user => { 
        val updatedUser = User.updateUser(getIntIdValue(user_id), user)
      Result(
          header = ResponseHeader(201, Map(CONTENT_TYPE->"application/json")),
          body = Enumerator(Json.toJson(updatedUser).toString().getBytes())
          )
    }
  )}
  
  def listUsers = Action {
    val json = Json.toJson(User.userHashMap.values.toList)
    Ok(json)
}
  
def createUser = Action(BodyParsers.parse.json) { request =>
  val userResult = request.body.validate[User]
  userResult.fold(
    errors => {
        Result(
            header = ResponseHeader(400, Map(CONTENT_TYPE->"application/json")),
            body = Enumerator(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)).toString.getBytes())
            )
      //BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)))
    },
    user => { 
      val savedUser = User.save(user)
      Result(
          header = ResponseHeader(201, Map(CONTENT_TYPE->"application/json")),
          body = Enumerator(Json.toJson(savedUser).toString().getBytes())
          )
    }
  )
}

def createCard(user_id:String) = Action(BodyParsers.parse.json) { request =>
   val cardResult = request.body.validate[IDCard]
   cardResult.fold(
    errors => {
        Result(
            header = ResponseHeader(400, Map(CONTENT_TYPE->"application/json")),
            body = Enumerator(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)).toString.getBytes())
            )
      //BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)))
    },
    card => { 
      val savedCard = User.createCard(getIntIdValue(user_id), card)
      Result(
          header = ResponseHeader(201, Map(CONTENT_TYPE->"application/json")),
          body = Enumerator(Json.toJson(savedCard).toString().getBytes())
          )
    }
  )
}

def getCards(user_id:String) = Action { request =>
    val json = Json.toJson(User.getCards(getIntIdValue(user_id)))
    Ok(json)
}

def getCard(user_id:String, card_id:String) = Action { request =>
    val json = Json.toJson(User.getCard(getIntIdValue(user_id), getIntIdValue(card_id)))
    Ok(json)
}

def deleteCard(user_id:String, card_id:String) = Action { request =>
    User.deleteCard(getIntIdValue(user_id), getIntIdValue(card_id))
    NoContent
}


// Web Logins
def createLogin(user_id:String) = Action(parse.json) { request =>
    val webResult = request.body.validate[WebLogin]
    webResult.fold(
    errors => {
        Result(
            header = ResponseHeader(400, Map(CONTENT_TYPE->"application/json")),
            body = Enumerator(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)).toString.getBytes())
            )
      //BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)))
    },
    login => { 
      val savedLogin = User.createLogin(getIntIdValue(user_id), login)
      Result(
          header = ResponseHeader(201, Map(CONTENT_TYPE->"application/json")),
          body = Enumerator(Json.toJson(savedLogin).toString().getBytes())
          )
    }
  )
}

def getLogins(user_id:String) = Action { request =>
    val json = Json.toJson(User.getLogins(getIntIdValue(user_id)))
    Ok(json)
}

def deleteLogin(user_id:String, login_id:String) = Action { request =>
    User.deleteLogin(getIntIdValue(user_id), getIntIdValue(login_id))
    NoContent
}

// Bank Accounts
def createBankAccount(user_id:String) = Action(parse.json) { request =>
    val bankResult = request.body.validate[BankAccount]
    bankResult.fold(
    errors => {
        Result(
            header = ResponseHeader(400, Map(CONTENT_TYPE->"application/json")),
            body = Enumerator(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)).toString.getBytes())
            )
      //BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)))
    },
    bank => { 
      val savedBank = User.createBankAccount(getIntIdValue(user_id), bank)
      Result(
          header = ResponseHeader(201, Map(CONTENT_TYPE->"application/json")),
          body = Enumerator(Json.toJson(savedBank).toString().getBytes())
          )
    }
  )
}

def getBankAccounts(user_id:String) = Action { request =>
   val json = Json.toJson(User.getBankAccounts(getIntIdValue(user_id)))
    Ok(json)
}
def deleteBankAccount(user_id:String, ba_id:String) = Action { request =>
   User.deleteBankAccount(getIntIdValue(user_id), getIntIdValue(ba_id))
    NoContent
}


}