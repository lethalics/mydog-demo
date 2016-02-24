package models

import java.io.File

import com.github.mauricio.async.db.RowData
import controllers.{CommentPostRequest, WallPostRequest}
import org.joda.time.{LocalDateTime, DateTime}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import db.DB
import org.apache.commons.io.FilenameUtils
import play.api.Logger
import play.api.Play._
import play.api.libs.json.{Json, Writes}

import scala.concurrent.Future
import scala.util.control.NonFatal

import models.ProfileMyDogModel.UserNotValidatedException

import javax.inject.{Inject, Singleton}

@Singleton
class WallPost @Inject() (userModel: User) extends DB{

  import WallPostModel._

  def insertWallPost(userId: Int, postFile: PostFile, post: WallPostRequest): Future[Long] = {
    insert("""###""",
      Array(post.body, userId, DateTime.now, postFile.webImg.getOrElse(null), postFile.webBig.getOrElse(null), postFile.webVideoX264.getOrElse(null), postFile.webVideoVp8.getOrElse(null), postFile.`type`,
        WALL_POST_PUBLIC, WALL_POST_MOBILE, post.lng.getOrElse(null), post.lat.getOrElse(null), post.address.getOrElse(null)))
  }

  def updateWallPostVp8(vp8: String, postId: Long) = {
    execute("###", vp8, postId).recover{
      case NonFatal(e) =>
        Logger.error(s"Exception updateting vp8 file: ", e)
    }
  }

  def deleteWallPost(postId: Long, email: String):Future[Option[Long]] = {
    userModel.findByEmail(email).flatMap{
      userDetails =>
        //get post owner
        findOneOption("""###""", Array(postId, userDetails.id), (rowData => (rowData("userId").asInstanceOf[Int], rowData("image_web"), rowData("image_big"), rowData("web_video_x264"), rowData("web_video_vp8")))).flatMap{
          case Some((postOwner, imgWeb, imgBig, videox264, videovp8)) =>

               execute("""DELETE FROM wallpost WHERE id_post=?""", postId).map{
                 res =>
                   Logger.debug(s"Deleted wallpost $postId with status: $res")
                   execute("""DELETE FROM wall_comment WHERE id_post=?""", postId).recover{case NonFatal(e) => Logger.error("Exception deleting wallcomment", e)}
                   execute("""DELETE FROM notifications WHERE postId=?""", postId).recover{case NonFatal(e) => Logger.error("Exception deleting notification", e)}

                   //remove files from disk
                   val rootPath = current.configuration.getString("root.images.path").getOrElse("")

                   if(imgWeb.isInstanceOf[String]) {
                     val webFile = new File(rootPath + imgWeb.asInstanceOf[String])
                     if(webFile.exists()) webFile.delete
                   }

                   if(imgBig.isInstanceOf[String]) {
                     val bigFile = new File(rootPath + imgBig.asInstanceOf[String])
                     if(bigFile.exists()) bigFile.delete
                   }

                   if(videox264.isInstanceOf[String]) {
                     val x264File = new File(rootPath + videox264.asInstanceOf[String])
                     if(x264File.exists()) x264File.delete
                   }

                   if(videovp8.isInstanceOf[String]) {
                     val vp8File = new File(rootPath + videovp8.asInstanceOf[String])
                     if(vp8File.exists()) vp8File.delete
                   }

                   Some(postId)

               }

          case None =>
            Logger.debug(s"Post with id $postId not found")
            Future.successful(None)
          }
        }
  }

  def addComment(email: String, comment: CommentPostRequest): Future[Long] = {
    userModel.findByEmail(email).flatMap{
      userDetails =>
        userDetails.enabled match {
          case true =>
            insert("""###""", Array(comment.postId, userDetails.id, comment.comment, DateTime.now))
          case false =>
            Future.failed(UserNotValidatedException("User account not validated"))
        }

    }
  }

  def deleteComment(email: String, commentId: Long):Future[Boolean] = {
    userModel.findByEmail(email).flatMap {
      userDetails =>
        findOneOption("""####""", Array(commentId, userDetails.id, userDetails.id),
          (rowData => (rowData("commentOwner").asInstanceOf[Int], rowData("postOwner").asInstanceOf[Int]))).flatMap {
            //delete comment
            case Some(owners) =>
              Logger.debug(s"DELETE comment with id: $commentId")
              execute("""DELETE FROM wall_comment WHERE id_comment=?""", commentId).map{
                res => true
              }
            case None => Future.successful(false)
        }
    }
  }

  def loadWallPostFile(id: Long): Future[Option[PostFile]] = {
    findOneOption("""####""", Array(id),wallPostFileMapper)
  }

  def listWallPosts(skip:Int, size: Int, email: String):Future[IndexedSeq[WallPostResponse]] = {
    userModel.findByEmail(email).flatMap {
      userDetails =>
        val blockedUsersFuture = userModel.getBlockedUsers(userDetails.id)
        val blockingMeUsersFuture = userModel.getBlockingMeUsers(userDetails.id)
        val unfollowedUsersFuture = userModel.getUnfollowedUsers(userDetails.id)

        val ignoredUsers = for {
          blockedUsers <- blockedUsersFuture
          blockingMeUsers <- blockingMeUsersFuture
          unfollowedUsers <- unfollowedUsersFuture
        } yield {
            ((blockedUsers ++ blockingMeUsers ++ unfollowedUsers).toSet, (blockedUsers ++ blockingMeUsers))
          }
        ignoredUsers.flatMap { users =>
          val limitDays = current.configuration.getInt("wall.history.limit.days").getOrElse(60)
          val blocked = if(users._2.size > 0) users._2.mkString(",") else "0"
          val ignored = if(users._1.size > 0) users._1.mkString(",") else "0"
          findIn(
            """####""".stripMargin,
            Array(userDetails.id, TREAT_GIVEN,DateTime.now.minusDays(limitDays), WALL_POST_PUBLIC, skip, size), Map(":blockedUsers" -> blocked, ":ignoredUsers" -> ignored), WallPostResponseMapper)

        }
    }
  }

  def listUserPosts(userId: Long, skip:Int, size: Int, email: String): Future[IndexedSeq[WallPostResponse]] = {
    userModel.findByEmail(email).flatMap {
      userDetails =>
        val blockedUsersFuture = userModel.getBlockedUsers(userDetails.id)
        val blockingMeUsersFuture = userModel.getBlockingMeUsers(userDetails.id)

        val ignoredUsers = for {
          blockedUsers <- blockedUsersFuture
          blockingMeUsers <- blockingMeUsersFuture
        } yield {
            (blockedUsers ++ blockingMeUsers)
          }
        ignoredUsers.flatMap { users =>
          users.contains(userId) match {
            //blocked or blocking me user
            case true =>
              Future.successful(IndexedSeq.empty[WallPostResponse])
            //show posts
            case false =>
              val blocked = if(users.size > 0) users.mkString(",") else "0"
              findIn(
                """####""".stripMargin,
                Array(userDetails.id, TREAT_GIVEN,userId, WALL_POST_PUBLIC, skip, size), Map(":blockedUsers" -> blocked), WallPostResponseMapper)
          }
        }
    }
  }

  def getPostById(postId: Long, email: String): Future[Option[WallPostResponse]] = {
    userModel.findByEmail(email).flatMap {
      userDetails =>
        val blockedUsersFuture = userModel.getBlockedUsers(userDetails.id)
        val blockingMeUsersFuture = userModel.getBlockingMeUsers(userDetails.id)

        val ignoredUsers = for {
          blockedUsers <- blockedUsersFuture
          blockingMeUsers <- blockingMeUsersFuture
        } yield {
            (blockedUsers ++ blockingMeUsers)
          }
        ignoredUsers.flatMap { users =>
          val blocked = if(users.size > 0) users.mkString(",") else "0"
          findIn(
            """#### """.stripMargin,
            Array(userDetails.id, TREAT_GIVEN, postId), Map(":blockedUsers" -> blocked), WallPostResponseMapper).map{posts => posts.headOption}

        }
    }
  }

  def listPostComments(postId: Long, skip: Int, size: Int, email: String):Future[IndexedSeq[PostCommentResponse]] = {
    userModel.findByEmail(email).flatMap {
      userDetails =>
        userModel.getIgnoredUsers(userDetails.id).flatMap{
          users =>
            val ignored = if(users.size > 0) users.mkString(",") else "0"
            findIn(
              """####""".stripMargin,
            Array(postId, skip, size), Map(":ignoredUsers" -> ignored), PostCommentResponseMapper)
        }
    }
  }

  def listPostCommentsOwners(postId: Long, email: String): Future[IndexedSeq[CommentOwner]] = {
    userModel.findByEmail(email).flatMap {
      userDetails =>
        userModel.getIgnoredUsers(userDetails.id).flatMap {
          users =>
            val ignored = if (users.size > 0) users.mkString(",") else "0"
            findIn(
              """###""".stripMargin, Array(postId), Map(":ignoredUsers" -> ignored), CommentOwnerMapper)
        }
    }
  }

  def listPostTreats(postId: Long, skip: Int, size: Int): Future[IndexedSeq[PostTreatResponse]] = {
    find(
      """###""".stripMargin,
      Array(postId, TREAT_GIVEN, skip, size), PostTreatResponseMapper)
  }

  def addPostTreat(postId: Long, email: String):Future[Long] = {
    userModel.findByEmail(email).flatMap {
      userDetails =>
        insert("""###""", Array(postId, userDetails.id, TREAT_GIVEN, DateTime.now, TREAT_GIVEN, DateTime.now))
    }
  }

  def removePostTreat(postId: Long, email: String):Future[Long] = {
    userModel.findByEmail(email).flatMap {
      userDetails =>
        execute("""###""", TREAT_TAKEN, DateTime.now, postId, userDetails.id).map {
         res =>
           Logger.debug(s"Removed treat for post: $postId with status: $res")
            res.rowsAffected
        }
    }
  }

}

object WallPostModel {
  val WALL_POST_PHOTO = 3
  val WALL_POST_EMBEDDED_VIDEO = 5
  val WALL_POST_MOBILE = 1
  val WALL_POST_PUBLIC = 2

  val TREAT_TAKEN = 0
  val TREAT_GIVEN = 1

  case class PostFile(webImg: Option[String], webBig: Option[String], webVideoX264: Option[String], webVideoVp8: Option[String], `type`: Int)

  case class WallPostResponse(id: Long, userId: Int, text: Option[String], date: DateTime, webImgUrl: String, bigImgUrl: String, webVideoX264Url: String, webVideoVp8Url: String,
                              postType: Int, title: String, description: String, url: String, treats: Int, lng: Option[String], lat: Option[String], address: Option[String],
                              firstName: String, lastName: String, dogId: Option[Int], dogName: Option[String], dogSex: Option[Int], dogBirthdate: Option[LocalDateTime], dogBreed: Option[String], commentsNo: Long, treatGiven: Boolean)

  case class PostCommentResponse(id: Long, idPost: Int, userId: Int, firstName: String, lastName: String, content: String, date: DateTime)

  case class PostTreatResponse(userId: Int, firstName: String, lastName: String, dogId: String, dogName: String, date: DateTime)

  case class CommentOwner(userId: Int, firstName: String, lastName: String, dogId: String, dogName: String)

  implicit val WallPostResponseWrites = new Writes[WallPostResponse]{
    def writes(response: WallPostResponse) = Json.obj(
      "id" -> response.id,
      "userId" -> response.userId,
      "text" -> response.text.getOrElse(null),
      "date" -> response.date,
      "webImgUrl" -> response.webImgUrl,
      "bigImgUrl" -> response.bigImgUrl,
      "webVideoX264Url" -> response.webVideoX264Url,
      "webVideoVp8Url" -> response.webVideoVp8Url,
      "postType" -> response.postType,
      "title" -> response.title,
      "description" -> response.description,
      "url" -> response.url,
      "treats" -> response.treats,
      "lng" -> response.lng.getOrElse(null),
      "lat" -> response.lat.getOrElse(null),
      "address" -> response.address.getOrElse(null),
      "firstName" -> response.firstName,
      "lastName" -> response.lastName,
      "dogId" -> response.dogId,
      "dogName" -> response.dogName,
      "dogSex" -> response.dogSex,
      "dogBirthdate" -> response.dogBirthdate.map(d => d.toString),
      "dogBreed" -> response.dogBreed,
      "commentsNo" -> response.commentsNo,
      "treatGiven" -> response.treatGiven
    )
  }
   def wallPostFileMapper(rowData: RowData) = {
    PostFile(
      if(rowData("image_web").isInstanceOf[String]) Some(rowData("image_web").asInstanceOf[String]) else None,
      if(rowData("image_big").isInstanceOf[String]) Some(rowData("image_big").asInstanceOf[String]) else None,
      if(rowData("web_video_x264").isInstanceOf[String]) Some(rowData("web_video_x264").asInstanceOf[String]) else None,
      if(rowData("web_video_vp8").isInstanceOf[String]) Some(rowData("web_video_vp8").asInstanceOf[String]) else None,
      rowData("type").asInstanceOf[Int]
    )
  }

  def WallPostResponseMapper(rowData: RowData) = {
    WallPostResponse(
      rowData("id_post").asInstanceOf[Int].toLong,
      rowData("userId").asInstanceOf[Int],
      Some(rowData("content").asInstanceOf[String]),
      rowData("date").asInstanceOf[LocalDateTime].toDateTime,
      if(rowData("image_web").isInstanceOf[String]) controllers.routes.WallPosts.getPostFile( rowData("id_post").asInstanceOf[Int], FilenameUtils.getExtension(rowData("image_web").asInstanceOf[String]) ).url else "",
      if(rowData("image_big").isInstanceOf[String]) controllers.routes.WallPosts.getPostFile( rowData("id_post").asInstanceOf[Int], FilenameUtils.getExtension(rowData("image_big").asInstanceOf[String]), "big").url else "",
      if(rowData("web_video_x264").isInstanceOf[String]) controllers.routes.WallPosts.getPostFile( rowData("id_post").asInstanceOf[Int], FilenameUtils.getExtension(rowData("web_video_x264").asInstanceOf[String])).url else "",
      if(rowData("web_video_vp8").isInstanceOf[String]) controllers.routes.WallPosts.getPostFile( rowData("id_post").asInstanceOf[Int], FilenameUtils.getExtension(rowData("web_video_vp8").asInstanceOf[String])).url else "",
      rowData("type").asInstanceOf[Int],
      rowData("title").asInstanceOf[String],
      rowData("description").asInstanceOf[String],
      rowData("url").asInstanceOf[String],
      rowData("total_treats").asInstanceOf[Int],
      if(rowData("lng").isInstanceOf[String]) Some(rowData("lng").asInstanceOf[String]) else Some(null),
      if(rowData("lat").isInstanceOf[String]) Some(rowData("lat").asInstanceOf[String]) else Some(null),
      if(rowData("address").isInstanceOf[String]) Some(rowData("address").asInstanceOf[String]) else Some(null),
      rowData("first_name").asInstanceOf[String],
      rowData("last_name").asInstanceOf[String],
      if(rowData("dog_id").isInstanceOf[Int]) Some(rowData("dog_id").asInstanceOf[Int]) else None,
      if(rowData("dog_name").isInstanceOf[String]) Some(rowData("dog_name").asInstanceOf[String]) else None,
      if(rowData("dog_sex").isInstanceOf[Byte]) Some(rowData("dog_sex").asInstanceOf[Byte]) else None,
      if(rowData("dog_birthdate").isInstanceOf[LocalDateTime]) Some(rowData("dog_birthdate").asInstanceOf[LocalDateTime]) else None,
      if(rowData("breed_name").isInstanceOf[String]) Some(rowData("breed_name").asInstanceOf[String]) else None,
      rowData("commentsNo").asInstanceOf[Long],
      if(rowData("treatGiven").asInstanceOf[Long] == 0 ) false else true
    )
  }

  def PostCommentResponseMapper(rowData: RowData) = {
    PostCommentResponse(
      rowData("id_comment").asInstanceOf[Int].toLong,
      rowData("id_post").asInstanceOf[Int],
      rowData("userId").asInstanceOf[Int],
      rowData("first_name").asInstanceOf[String],
      rowData("last_name").asInstanceOf[String],
      rowData("content").asInstanceOf[String],
      rowData("date").asInstanceOf[LocalDateTime].toDateTime
    )
  }

  def PostTreatResponseMapper(rowData: RowData) = {
    PostTreatResponse(
      rowData("userId").asInstanceOf[Int],
      rowData("first_name").asInstanceOf[String],
      rowData("last_name").asInstanceOf[String],
      if(rowData("dogId").isInstanceOf[Int]) rowData("dogId").asInstanceOf[Int].toString else null,
      if(rowData("dogName").isInstanceOf[String]) rowData("dogName").asInstanceOf[String] else null,
      rowData("date").asInstanceOf[LocalDateTime].toDateTime
    )
  }

  def CommentOwnerMapper(rowData: RowData) = {
    CommentOwner(
      rowData("userId").asInstanceOf[Int],
      rowData("first_name").asInstanceOf[String],
      rowData("last_name").asInstanceOf[String],
      if(rowData("dogId").isInstanceOf[Int]) rowData("dogId").asInstanceOf[Int].toString else null,
      if(rowData("dogName").isInstanceOf[String]) rowData("dogName").asInstanceOf[String] else null
    )
  }

  implicit val WallPostResponseWrites = new Writes[WallPostResponse]{
    def writes(response: WallPostResponse) = Json.obj(
      "id" -> response.id,
      "userId" -> response.userId,
      "text" -> response.text.getOrElse(null),
      "date" -> response.date,
      "webImgUrl" -> response.webImgUrl,
      "bigImgUrl" -> response.bigImgUrl,
      "webVideoX264Url" -> response.webVideoX264Url,
      "webVideoVp8Url" -> response.webVideoVp8Url,
      "postType" -> response.postType,
      "title" -> response.title,
      "description" -> response.description,
      "url" -> response.url,
      "treats" -> response.treats,
      "lng" -> response.lng.getOrElse(null),
      "lat" -> response.lat.getOrElse(null),
      "address" -> response.address.getOrElse(null),
      "firstName" -> response.firstName,
      "lastName" -> response.lastName,
      "dogId" -> response.dogId,
      "dogName" -> response.dogName,
      "dogSex" -> response.dogSex,
      "dogBirthdate" -> response.dogBirthdate.map(d => d.toString),
      "dogBreed" -> response.dogBreed,
      "commentsNo" -> response.commentsNo,
      "treatGiven" -> response.treatGiven
    )
  }
}


