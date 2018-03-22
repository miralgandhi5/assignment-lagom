package edu.knoldus

import javax.inject.Inject

import akka.{Done, NotUsed}
import com.lightbend.lagom.scaladsl.api.ServiceCall
import edu.knoldus.entities.{ExternalData, User, UserData}
import play.api.Logger

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.parsing.json.JSONArray

class UserServiceImpl (externalService: ExternalService)(implicit ec: ExecutionContext) extends UserService {
  val userList: ListBuffer[User] = ListBuffer.empty[User]

  override def hello(id: String) = ServiceCall { _ =>
    Future.successful(s"hello miral")
  }

  override def addUser(): ServiceCall[User, Done] = ServiceCall { request =>
    val user = User(request.id, request.name, request.age)
    userList += user
    Future.successful(Done)
  }

  override def getUser(id: Int): ServiceCall[NotUsed, String] = ServiceCall { _ =>
    val userOptional = userList.find(user => user.id == id)
    userOptional.fold(Future.successful(s"not found"))(user => Future.successful(s"$user"))
  }

  override def deleteUser(id: Int): ServiceCall[NotUsed, String] = ServiceCall { _ =>
    val user = userList.filter(user => user.id == id)
    userList --= user
    Logger.info(s"$userList")
    Future.successful(s"$userList")
  }

  override def updateUser(id: Int): ServiceCall[UserData, String] = ServiceCall { updateUser =>
    val userOptional: Option[User] = userList.find(user => user.id == id)
    userOptional match {
      case Some(user) => userList -= user
        val newUser: User = user.copy(name = updateUser.name, age = updateUser.age)
        userList += newUser
        Future.successful(s" in some $userList")
      case None => Future.successful(s"in none $userList")
    }
  }

  override def anyUpdate(id: Int): ServiceCall[JSONArray, String] = ServiceCall { updateUser =>
    val userOptional: Option[User] = userList.find(user => user.id == id)
    userOptional match {
      case Some(user) => userList -= user
        Future.successful(s" in some $userList")
      case None => Future.successful(s"in none $userList")
    }
  }

  def getExternal(): ServiceCall[NotUsed,ExternalData] = ServiceCall {_ =>
    val result: Future[ExternalData] = externalService.testService().invoke()
    result
  }
}
