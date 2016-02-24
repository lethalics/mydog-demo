package controllers

import javax.inject.{Inject, Singleton}

import models._
import models.ProfileMyDogModel._
import play.api.Logger
import play.api.Play._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.mvc.{Result, SimpleResult, ResponseHeader, Action}
import securesocial.core.{BasicProfile, RuntimeEnvironment}

import _root_.util.{DoubleFormat}
import services.WallPostService

import scala.concurrent.Future
import scala.util.control.NonFatal


@Singleton
class WallPosts @Inject() (override implicit val env: RuntimeEnvironment[BasicProfile], wallPostService: WallPostService) extends securesocial.core.SecureSocial[BasicProfile]  {

  import WallPostModel._

  val uploadForm = Form(
    mapping("text" -> nonEmptyText, "exifOrientation" -> optional(number), "lng" -> optional(DoubleFormat.double), "lat" -> optional(DoubleFormat.double), "address" -> optional(text))(WallPostRequest.apply)(WallPostRequest.unapply)
  )

  //add new post
  def newPost = SecuredAction.async(parse.multipartFormData) { implicit request =>
    val user = request.user.asInstanceOf[BasicProfile]

    val p : Option[WallPostRequest] = uploadForm.bindFromRequest().fold(
      errFrm => None,
      post => Some(post)
    )

    request.body.file("file").map{file =>
      p.map{msg =>
        Logger.debug(s"Trying to save post: $msg")
        //save post
        wallPostService.addWallPost(user.email.get, Some(file), msg).map {
            wallpost =>
              Logger.debug(s"Post saved successful $wallpost")
              Created(Json.toJson(wallpost))
        }.recover {
          case e:UserNotValidatedException => Forbidden(Json.obj("status" -> e.getMessage))
          case e:IllegalArgumentException => BadRequest(Json.obj("status" -> e.getMessage))
          case NonFatal(e) =>
            Logger.error(s"Exception adding new post: ", e)
            InternalServerError(Json.obj("status" -> "Post not saved"))
        }

      }.getOrElse(Future.successful(BadRequest(Json.obj("status" -> "message text is missing"))))
    }.getOrElse(Future.successful(BadRequest(Json.obj("status" -> "message file is missing"))))

  }
  //return post's attached file
  def getPostFile(id: Long, extension: String, imgType: String) = Action.async(parse.anyContent){ implicit request =>
    wallPostService.loadWallPostFile(id, extension, imgType).map {
      case Some(downloadFile) =>
        Logger.debug(s"Trying to download $downloadFile")

        val fileContent: Enumerator[Array[Byte]] = Enumerator.fromFile(downloadFile.file)
        Result(
          header = ResponseHeader(200, Map(
            CONTENT_LENGTH -> downloadFile.file.length.toString,
            CONTENT_RANGE -> s"bytes */${downloadFile.file.length.toString}",
            ACCEPT_RANGES -> "bytes",
            CONTENT_TYPE -> downloadFile.contentType,
            PRAGMA -> "public",
            CONTENT_TRANSFER_ENCODING -> "binary"
            //CONTENT_DISPOSITION -> "attachment"
          )),
          body = fileContent
        )

      //file not found
      case None => NotFound
    }
  }

  //list wall posts
  def listPosts(skip: Int, size: Int) = SecuredAction.async(parse.anyContent){ implicit  request =>
    val user = request.user.asInstanceOf[BasicProfile]

    wallPostService.listWallPosts(skip, size, user.email.get).map{
      posts => Ok(Json.toJson(posts))
    }
  }

  //list user's posts
  def listUserPosts(userId: Long, skip: Int, size: Int) = SecuredAction.async(parse.anyContent) { implicit request =>
    val user = request.user.asInstanceOf[BasicProfile]
    wallPostService.listUserPosts(userId, skip, size, user.email.get).map {
      posts => Ok(Json.toJson(posts))
    }
  }

  def getPost(id: Long) = SecuredAction.async(parse.anyContent) { implicit request =>
    val user = request.user.asInstanceOf[BasicProfile]
    wallPostService.getPost(id, user.email.get).map{
      case Some(post) => Ok(Json.toJson(post))
      case None => NotFound
    }
  }

  def deletePost(postId: Long) = SecuredAction.async(parse.anyContent) { implicit request =>
    val user = request.user.asInstanceOf[BasicProfile]
    wallPostService.deleteWallPost(postId, user.email.get).map{
      case Some(postId) => Ok(Json.obj("status" -> "Success"))
      case None => NotFound
    }
  }
  //add new comment to a post
  def addComment = SecuredAction.async(parse.json) { implicit request =>
    implicit val commentPostFormat = Json.format[CommentPostRequest]
    val user = request.user.asInstanceOf[BasicProfile]

    request.body.validate[CommentPostRequest].map {
      req =>
        wallPostService.addComment(user.email.get, req).map{
          commentId =>
            Ok(Json.obj("status" -> "Success"))
        }.recover{
          case t:UserNotValidatedException => Forbidden(Json.obj("status" -> t.getMessage))
        }

    }.getOrElse(Future.successful(BadRequest(Json.obj("status" -> "invalid json format"))))
  }

  def listComments(postId: Long, skip: Int, size: Int) = SecuredAction.async(parse.anyContent) { implicit request =>
    implicit val postCommentResponseFormat = Json.format[PostCommentResponse]
    val user = request.user.asInstanceOf[BasicProfile]

    wallPostService.listPostComments(postId, skip, size, user.email.get).map{
      comments => Ok(Json.toJson(comments))
    }
  }

  def deleteComment(commentId: Long) = SecuredAction.async(parse.anyContent) { implicit request =>
    val user = request.user.asInstanceOf[BasicProfile]
    wallPostService.deleteComment(user.email.get, commentId).map {
      case true => Ok(Json.obj("status" -> "Success"))
      case false => NotFound
    }
  }

  def listCommentsOwners(postId: Long) = SecuredAction.async(parse.anyContent) { implicit request =>
    implicit val commentsOwnersFormat = Json.format[CommentOwner]
    val user = request.user.asInstanceOf[BasicProfile]

    wallPostService.listPostCommentsOwners(postId, user.email.get).map{
      owners => Ok(Json.toJson(owners))
    }
  }

  def listTreats(postId: Long, skip: Int, size: Int) = SecuredAction.async(parse.anyContent) { implicit request =>
    implicit val treatsCommentsResponseFormat = Json.format[PostTreatResponse]
    wallPostService.listPostTreats(postId, skip, size).map{
      treats => Ok(Json.toJson(treats))
    }
  }

  def addTreat(postId: Long) = SecuredAction.async(parse.anyContent) { implicit request =>
    val user = request.user.asInstanceOf[BasicProfile]
    wallPostService.addPostTreat(postId, user.email.get).map {
      res => Ok(Json.obj("status" -> "Success"))
    }
  }

  def removeTreat(postId: Long) = SecuredAction.async(parse.anyContent) { implicit request =>
    val user = request.user.asInstanceOf[BasicProfile]
    wallPostService.removePostTreat(postId, user.email.get).map {
      res => Ok(Json.obj("status" -> "Success"))
    }
  }

}

case class WallPostRequest(body: String, exifOrientation: Option[Int], lng:Option[Double], lat: Option[Double], address: Option[String])
case class CommentPostRequest(postId: Long, comment: String)
