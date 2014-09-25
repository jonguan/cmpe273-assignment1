package domain.user

import org.joda.time._
import java.util.Date
import java.text.SimpleDateFormat
import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.ISODateTimeFormat
import scala.util.Random._
import scala.collection.mutable.HashMap

case class User(user_id:Option[Int], email: String, password: String, name:Option[String], created_at:Option[DateTime], updated_at:Option[DateTime])
case class IDCard(card_id:Option[Int], card_name:String, card_number:String, expiration_date:Option[DateTime])
case class WebLogin(login_id:Option[Int], url:String, login:String, password:String)
case class BankAccount(ba_id:Option[Int], account_name:Option[String], routing_number:String, account_number:String)

object User {
    var userHashMap = scala.collection.mutable.HashMap[Int, User]()
    // Hash maps of hash maps {user_id: hashMap} --> {card_id:card}
    var cardHashMap = scala.collection.mutable.HashMap[Int, HashMap[Int, IDCard]]()
    var webHashMap = scala.collection.mutable.HashMap[Int, HashMap[Int, WebLogin]]()
    var bankHashMap = scala.collection.mutable.HashMap[Int, HashMap[Int, BankAccount]]()
    
    val ID_MAX: Int = 1000000
    
    var list: List[User] = {
    List(
      User(
        Some(1),
        "abc@abc.com",
        "pwd123",
        Some("abc"),
        Some(new DateTime()),
        None
        
      ),
      User(
         Some(2),
        "xyz@xyz.com",
        "pass456",
        None,
        Some(new DateTime()),
        Some(new DateTime())
      )
    )
  }
    
  def save(user: User): User = {
      var newUser:User = null;
      val user_id = user.user_id.getOrElse(-1)
      if (user_id == -1) {
          // New User
          val new_id:Int = scala.util.Random.nextInt(ID_MAX)
          newUser = user.copy(user_id=Some(new_id), created_at=Some(new DateTime()))
          
      } else {
          // Update the updated_at
          newUser = user.copy(updated_at=Some(new DateTime()))
      }
    //list = list ::: List(newUser)
    userHashMap.put(newUser.user_id.getOrElse(-1), newUser)
    return newUser
  }
  
  def getUser(user_id:Int)  = {
      userHashMap.get(user_id)
  }
  
  def updateUser(user_id:Int, bodyUser:User): User = {
      val dbUser:User = userHashMap.get(user_id).getOrElse(null)
      if (dbUser == null) {
          return null
      }
      var newUser:User = bodyUser.copy(user_id=dbUser.user_id, created_at=dbUser.created_at, updated_at=Some(new DateTime()))
      userHashMap.put(newUser.user_id.getOrElse(-1), newUser)
      newUser
  }
  
  
  // Cards
  def createCard(user_id:Int, card:IDCard): IDCard = {
      var userCardHashMap = cardHashMap.get(user_id).getOrElse(null)
      // Create hash map if necessary
      if (userCardHashMap == null) {
          userCardHashMap = HashMap[Int, IDCard]()
          cardHashMap.put(user_id, userCardHashMap);
      }
      
      // Validate card
      var newCard:IDCard = null;
      val card_id = card.card_id.getOrElse(-1)
      if (card_id == -1) {
          // New User
          val newCard_id = scala.util.Random.nextInt(ID_MAX)
          newCard = card.copy(card_id = Some(newCard_id))
          userCardHashMap.put(newCard.card_id.getOrElse(-1), newCard)
          return newCard
      } else {
          // error
          return null
      }
  }
  
  def getCards(user_id:Int) :List[IDCard] = {
      var userCardHashMap = cardHashMap.get(user_id).getOrElse(null)
      if (userCardHashMap == null) {
          List()
      } else {
          userCardHashMap.values.toList
      }
  }
  
  def getCard(user_id:Int, card_id:Int): IDCard = {
      var userCardHashMap = cardHashMap.get(user_id).getOrElse(null)
      if (userCardHashMap == null) {
          return null
      } else {
          userCardHashMap.get(card_id).getOrElse(null)
      }
  }
  
  def deleteCard(user_id:Int, card_id:Int) = {
      var userCardHashMap = cardHashMap.get(user_id).getOrElse(null)
      if (userCardHashMap != null) {
          userCardHashMap.remove(card_id)
      }
  }
  
  // Web Login
    def createLogin(user_id:Int, login:WebLogin): WebLogin = {
      var userWebHashMap = webHashMap.get(user_id).getOrElse(null)
      // Create hash map if necessary
      if (userWebHashMap == null) {
          userWebHashMap = HashMap[Int, WebLogin]()
          webHashMap.put(user_id, userWebHashMap);
      }
      
      // Validate 
      var newLogin:WebLogin = null;
      val login_id = login.login_id.getOrElse(-1)
      if (login_id == -1) {
          // New User
          val newLogin_id = scala.util.Random.nextInt(ID_MAX)
          newLogin = login.copy(login_id = Some(newLogin_id))
          userWebHashMap.put(newLogin.login_id.getOrElse(-1), newLogin)
          return newLogin
      } else {
          // error
          return null
      }
  }
  
  def getLogins(user_id:Int) :List[WebLogin] = {
      var userWebHashMap = webHashMap.get(user_id).getOrElse(null)
      if (userWebHashMap == null) {
          List()
      } else {
          userWebHashMap.values.toList
      }
  }
  
  def deleteLogin(user_id:Int, login_id:Int) = {
      var userWebHashMap = webHashMap.get(user_id).getOrElse(null)
      if (userWebHashMap != null) {
          userWebHashMap.remove(login_id)
      }
  }
  
  // Bank Account
  def createBankAccount(user_id:Int, account:BankAccount): BankAccount = {
      var userBankHashMap = bankHashMap.get(user_id).getOrElse(null)
      // Create hash map if necessary
      if (userBankHashMap == null) {
          userBankHashMap = HashMap[Int, BankAccount]()
          bankHashMap.put(user_id, userBankHashMap);
      }
      
      // Validate 
      var newAccount:BankAccount = null;
      val ba_id = account.ba_id.getOrElse(-1)
      if (ba_id == -1) {
          // New User
          val newBa_id = scala.util.Random.nextInt(ID_MAX)
          newAccount = account.copy(ba_id = Some(newBa_id))
          userBankHashMap.put(newAccount.ba_id.getOrElse(-1), newAccount)
          return newAccount
      } else {
          // error
          return null
      }
  }
  
  def getBankAccounts(user_id:Int) :List[BankAccount] = {
      var userBankHashMap = bankHashMap.get(user_id).getOrElse(null)
      if (userBankHashMap == null) {
          List()
      } else {
          userBankHashMap.values.toList
      }
  }
  
  def deleteBankAccount(user_id:Int, ba_id:Int) = {
      var userBankHashMap = bankHashMap.get(user_id).getOrElse(null)
      if (userBankHashMap != null) {
          userBankHashMap.remove(ba_id)
      }
  }
}