package services

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.inject.{Inject, Singleton}

import controllers.{CommentPostRequest, WallPostRequest}
import models.WallPostModel._
import models._
import models.ProfileMyDogModel._
import org.apache.commons.io.FilenameUtils
import org.joda.time.DateTime
import play.api.Logger
import play.api.Play._
import play.api.libs.Files
import play.api.mvc.MultipartFormData
import util.{ImageUtils, XugglerUtils}

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.util.Random
import scala.util.control.NonFatal


@Singleton
class WallPostService @Inject()(wallpost: WallPost, user: User, notification: Notification, xuggler: XugglerUtils, imageUtils: ImageUtils) {

  def addWallPost(email:String, file: Option[MultipartFormData.FilePart[Files.TemporaryFile]], post:WallPostRequest):Future[WallPostResponse] = {

    user.findByEmail(email).flatMap{
      userDetails =>
        userDetails.enabled  match {
          case true =>
            saveWallPostFile(file, userDetails.id, post).flatMap{
              postFile =>
                wallpost.insertWallPost(userDetails.id, postFile, post).map{
                  lastPostId =>
                    //process async the vp8 conversion
                    if(postFile.`type` ==  WALL_POST_EMBEDDED_VIDEO)
                      saveWallVideoVP8(file, userDetails.id, post, lastPostId)

                    //return
                    WallPostResponse(lastPostId, userDetails.id, Some(post.body), DateTime.now,
                      if(postFile.webImg.nonEmpty) controllers.routes.WallPosts.getPostFile(lastPostId, FilenameUtils.getExtension(postFile.webImg.get)).url else "",
                      if(postFile.webBig.nonEmpty) controllers.routes.WallPosts.getPostFile(lastPostId, FilenameUtils.getExtension(postFile.webBig.get), "big").url else "",
                      if(postFile.webVideoX264.nonEmpty) controllers.routes.WallPosts.getPostFile(lastPostId, FilenameUtils.getExtension(postFile.webVideoX264.get)).url else "",
                      if(postFile.webVideoVp8.nonEmpty) controllers.routes.WallPosts.getPostFile(lastPostId, FilenameUtils.getExtension(postFile.webVideoVp8.get)).url else "",
                      postFile.`type`, "", "","", 0, if(post.lng.isDefined) post.lng.flatMap(lng => Some(lng.toString)) else Some(null),if(post.lat.isDefined) post.lat.flatMap(lat => Some(lat.toString)) else Some(null), Some(post.address.getOrElse(null)), userDetails.firstName, userDetails.lastName, userDetails.dogId, userDetails.dogName, userDetails.dogSex, userDetails.dogBirthDate, userDetails.dogBreed, 0, false )
                }
            }
          case false =>
            Logger.debug("User account not validated")
            Future.failed(UserNotValidatedException("User account not validated"))
        }
    }
  }

  def listWallPosts(skip:Int, size: Int, email: String):Future[IndexedSeq[WallPostResponse]] = {
    wallpost.listWallPosts(skip, size, email)
  }

  def saveWallPostFile(file: Option[MultipartFormData.FilePart[Files.TemporaryFile]], userId: Int, post:WallPostRequest): Future[PostFile] = {
    import ImageUtils._
    file match {
      case Some(file) =>
        //wallpost paths
        val paths = getWallPostPath(userId)
        val extension = FilenameUtils.getExtension(file.filename).toLowerCase

        Logger.debug(s"file extension $extension")

        extension match {
          //save video files
          case "mp4" =>
            //convert video file
            val convertX264 = xuggler.convert_x264(file.ref.file, paths.filename, paths.rootPath, paths.localVideosPath)
            val videoThumbnail = xuggler.extract_frame(file.ref.file, paths.filename, paths.rootPath, paths.localImagesPath)
            for{
              x264 <- convertX264
              thumbnail <- videoThumbnail
            }yield(PostFile(Some(thumbnail), None, Some(x264), None, WALL_POST_EMBEDDED_VIDEO))

          //save image files
          case "jpg" | "jpeg" =>
            //rotate image
            val rotatedImg = imageUtils.rotate(file.ref.file, post.exifOrientation)

            //save image versions
            rotatedImg.flatMap {
              rbimg =>
                val webImg = imageUtils.saveImage(paths.rootPath, paths.localImagesPath, paths.filename, extension, rbimg, IMAGE_WEB_WIDTH, IMAGE_WEB_HEIGHT)
                val bigImg = imageUtils.saveImage(paths.rootPath, paths.localImagesPath, paths.filename, extension, rbimg, IMAGE_BIG_WIDTH, IMAGE_BIG_HEIGHT)
                for{
                  web <- webImg
                  big <- bigImg
                  cleaned <- Future.successful(file.ref.clean)
                }yield(PostFile(web, big,None, None, WALL_POST_PHOTO))
            }

          case _ =>
            Logger.debug(s"Illegal file extension $extension")
            Future.failed(new IllegalArgumentException(s"Illegal file extension $extension"))
        }

      case None =>
        Logger.error("Missing post file")
        Future.failed(new IllegalArgumentException("Missing post file"))
    }
  }

  def saveWallVideoVP8(file: Option[MultipartFormData.FilePart[Files.TemporaryFile]], userId: Int, post:WallPostRequest, postId: Long) = {
    file match {
      case Some(file) =>
        val paths = getWallPostPath(userId)
        //convert video file
        val convertVp8 = xuggler.convert_vp8(file.ref.file, paths.filename, paths.rootPath, paths.localVideosPath)
        convertVp8.map {
          vp8 =>
            //clean temp file
            val clean = file.ref.clean
            //update file
            wallpost.updateWallPostVp8(vp8, postId)
        }.recover{
          case NonFatal(e) =>
            Logger.error(s"Exception converting vp8 file: ", e)
            //clean temp file
            file.ref.clean
        }

      case None => Logger.debug("VP8 conversion missing file")
    }
  }

  def getWallPostPath(userId: Int): WallPostPath = {
    val rootPath = current.configuration.getString("root.images.path").getOrElse("")
    val imagesPath = current.configuration.getString("wall.images.path").getOrElse("")
    val videosPath = current.configuration.getString("wall.videos.path").getOrElse("")

    val localImagesPath = "%s%s".format(imagesPath, userId)
    val localVideosPath = "%s%s".format(videosPath, userId)

    val basename = Random.alphanumeric.take(8).mkString
    val filename = "%s%s".format(basename, System.currentTimeMillis() / 1000)

    WallPostPath(rootPath, localImagesPath, localVideosPath, filename)
  }

  def loadWallPostFile(postId: Long, extension: String, imgType: String): Future[Option[WallPostFile]] = {
    wallpost.loadWallPostFile(postId).map {
      case Some(file) =>
        Logger.debug(s"Found PostFile: $file")

        //compute download file
        val (wallPostFile, contentType) = extension match {
          case "mp4" =>
            (getWallPostDiskFile( file.webVideoX264), "video/mp4")

          case "webm" =>
            (getWallPostDiskFile(file.webVideoVp8), "video/webm")

          case "jpg" | "jpeg" =>
            if(imgType.equals("big"))
              (getWallPostDiskFile(file.webBig), "image/jpeg")
            else
              (getWallPostDiskFile(file.webImg), "image/jpeg")

          case _ => (new File("FILE_NOT_FOUND"), "application/octet-stream")
        }

        if(wallPostFile.exists) Some(WallPostFile(wallPostFile, contentType)) else None

      case None => None
    }
  }

  def getWallPostDiskFile(localPath: Option[String]): File = {
    val rootPath = current.configuration.getString("root.images.path").getOrElse("")
    new File("%s%s".format(rootPath, localPath.getOrElse("/FILE_NOT_FOUND")))
  }

  def listUserPosts(userId: Long, skip:Int, size: Int, email: String): Future[IndexedSeq[WallPostResponse]] = {
    wallpost.listUserPosts(userId, skip, size, email)
  }

  def getPost(postId: Long, email: String): Future[Option[WallPostResponse]] = {
    wallpost.getPostById(postId, email)
  }

  def deleteWallPost(postId: Long, email: String):Future[Option[Long]] = {
    wallpost.deleteWallPost(postId, email)
  }

  def addComment(email: String, comment: CommentPostRequest): Future[Long] = {
    wallpost.addComment(email, comment).map{
      commentId =>
        //send notifications
        notification.notifyUsersForComment(email, comment)
        commentId
    }
  }

  def listPostComments(postId: Long, skip: Int, size: Int, email: String):Future[IndexedSeq[PostCommentResponse]] = {
    wallpost.listPostComments(postId, skip, size, email)
  }

  def deleteComment(email: String, commentId: Long):Future[Boolean] = {
    wallpost.deleteComment(email, commentId)
  }

  def listPostCommentsOwners(postId: Long, email: String): Future[IndexedSeq[CommentOwner]] = {
    wallpost.listPostCommentsOwners(postId, email)
  }

  def listPostTreats(postId: Long, skip: Int, size: Int): Future[IndexedSeq[PostTreatResponse]] = {
    wallpost.listPostTreats(postId, skip, size)
  }

  def addPostTreat(postId: Long, email: String):Future[Long] = {
    wallpost.addPostTreat(postId, email).map{
      treatId =>
        Logger.debug(s"Added treatId $treatId for postId: $postId")
        //send notifications
        notification.notifyUserForTreat(email, postId)
        treatId
    }
  }

  def removePostTreat(postId: Long, email: String):Future[Long] = {
    import models.NotificationModel._
    wallpost.removePostTreat(postId, email).map{
      rowsAffected =>
        if(rowsAffected > 0) {
          //remove notification
          notification.removeNotification(email, NOTIF_TYPE_TREAT, None, Some(postId))
        }
        rowsAffected
    }
  }
}

case class WallPostPath(rootPath: String, localImagesPath: String, localVideosPath: String, filename: String)
case class WallPostFile(file: File, contentType: String)
