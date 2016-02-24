package config

/**
 * Created by alex on 4/15/2015.
 */

import java.lang.reflect.Constructor
import javax.inject.Inject

import com.github.mauricio.async.db.Configuration
import com.github.mauricio.async.db.mysql.pool.MySQLConnectionFactory
import com.github.mauricio.async.db.pool.{ConnectionPool, PoolConfiguration}
import com.google.inject.{TypeLiteral, AbstractModule, Guice}
import models.UserProfile
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.util.control.NonFatal

//import com.google.inject.{Guice, AbstractModule}
import controllers.CustomRoutesService
import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import plugins.securesocial.{LocalUser, MyDogUserService, MyDogEventListener}

import securesocial.core.{BasicProfile, RuntimeEnvironment}

import util.MyDogConfig

object Global extends play.api.GlobalSettings {

    var pool: ConnectionPool[_] = _

    override def onStart(app: Application): Unit = {
      //read database configuration
      val configuration = new Configuration(
        username = MyDogConfig.dbUsername,
        host=MyDogConfig.dbHost,
        port = MyDogConfig.dbPort,
        password = Some(MyDogConfig.dbPassword),
        database = Some(MyDogConfig.dbName)
      )

      //instantiate Connection Factory
      val factory = new MySQLConnectionFactory(configuration)

      //define db connection pool
      pool = new ConnectionPool(factory, new PoolConfiguration(MyDogConfig.dbPoolMaxObjects, MyDogConfig.dbPoolMaxIdle, MyDogConfig.dbPoolMaxQueueSize))
    }

    override def onStop(app: Application) {
      pool.close
    }



   /**
    * The runtime environment for this app.
    */

   class MyRuntimeEnvironment @Inject()(userProfile: UserProfile) extends RuntimeEnvironment.Default[BasicProfile] {
     override implicit val executionContext = play.api.libs.concurrent.Execution.defaultContext
     override lazy val routes = new CustomRoutesService()
     override lazy val userService: MyDogUserService = new MyDogUserService(userProfile)
     override lazy val eventListeners = List(new MyDogEventListener())
   }

  val injector = Guice.createInjector(new AbstractModule {
    protected def configure(): Unit = {
      bind(new TypeLiteral[RuntimeEnvironment[BasicProfile]]{}).to(classOf[MyRuntimeEnvironment])
    }
  })

   /**
    * An implementation that checks if the controller expects a RuntimeEnvironment and
    * passes the instance to it if required.
    *
    * This can be replaced by any DI framework to inject it differently.
    *
    * @param controllerClass
    * @tparam A
    * @return
    */
   override def getControllerInstance[A](controllerClass: Class[A]): A = {
     /*val instance = controllerClass.getConstructors.find { c =>
       val params = c.getParameterTypes
       params.length == 1 && params(0) == classOf[RuntimeEnvironment[LocalUser]]
     }.map {
       _.asInstanceOf[Constructor[A]].newInstance(MyRuntimeEnvironment)
     }

     instance.getOrElse(super.getControllerInstance(controllerClass))*/
     injector.getInstance(controllerClass)
   }

  /**
  * Called when an exception occurred.
  *
  * The default is to send the framework default error page.
    *
  * @param request The HTTP request header
    * @param ex The exception
    * @return The result to send to the client
  */
  override def onError(request: RequestHeader, ex: Throwable): Future[Result] = {
    def devError = views.html.defaultpages.devError(Option(System.getProperty("play.editor"))) _
    def prodError = views.html.defaultpages.error.f
    try {
      Future.successful(InternalServerError(Json.obj("status" -> "Something went wrong!")))
    } catch {
      case NonFatal(e) => {
        Logger.error("Error while rendering default error message", e)
        Future.successful(InternalServerError)
      }
    }
  }

 }