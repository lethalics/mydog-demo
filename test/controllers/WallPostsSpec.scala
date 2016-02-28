package controllers

import java.io.File
import java.nio.file.{StandardCopyOption, Files}

import models.WallPostModel.WallPostResponse
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.specs2.matcher.JsonMatchers
import play.api.Logger

import play.api.mvc.MultipartFormData.FilePart
import play.api.test._
import play.api.test.Helpers._
import org.scalatestplus.play._
import play.api.mvc.MultipartFormData
import play.api.libs.Files.TemporaryFile
import util.MultipartFormDataWritable.anyContentAsMultipartFormWritable



@RunWith(classOf[JUnitRunner])
class WallPostsSpec extends PlaySpec with OneServerPerSuite with JsonMatchers {

  val fakeApp = FakeApplication(additionalConfiguration = Map("logger.application" -> "INFO"))
  val authToken = "38c1bc0f92466c2605a23f634810af5706df1d598c8ba25e984e162daafe6500113b6522451f39c2b53e63ccf0c0ddc8c4a8f24e6fab3daa4d26108b7bf1739994459346ddf3bce305fd3cb2c1e15c79b36b9478c6d16de2657d8cf33b874e0a5b679e0b859f5012e8a5345f4fa0f53828b7e4c292c1e43ac2553932239940e3"

  implicit override lazy val app: FakeApplication =
    FakeApplication(
      additionalConfiguration = Map("logger.application" -> "INFO", "logger.securesocial" -> "INFO")
    )



  "WallPosts should" must {

    //list wallposts
    "return authentication required on GET /1/posts" in {
      val Some(response) = route(FakeRequest(GET, "/1/posts"))
      status(response) mustBe (UNAUTHORIZED)
    }

    "return a list of two posts on /1/posts?size=2" in {
      val Some(response) = route(FakeRequest(GET, "/1/posts?size=2").withHeaders(("X-Auth-Token", authToken)))
      status(response) must equal(OK)
      contentType(response) mustBe Some("application/json")
      val jsonResponse = contentAsJson(response).as[Seq[WallPostResponse]]
      jsonResponse.size must equal(2)
    }
    //get post by id
    "return authentication required on GET /1/posts/:id" in {
      val Some(response) = route(FakeRequest(GET, "/1/posts/666"))
      status(response) mustBe (UNAUTHORIZED)
    }

    "return wallpost with specified id on /1/posts/:id" in {
      val Some(wallposts) = route(FakeRequest(GET, "/1/posts?size=1").withHeaders(("X-Auth-Token", authToken)))
      val wallpostsResponse = contentAsJson(wallposts).as[Seq[WallPostResponse]]
      val postId = wallpostsResponse(0).id

      val Some(response) = route(FakeRequest(GET, "/1/posts/%d".format(postId)).withHeaders(("X-Auth-Token", authToken)))
      status(response) must equal(OK)
      contentType(response) mustBe Some("application/json")
      val jsonResponse = contentAsJson(response).as[WallPostResponse]

      jsonResponse.id must equal(postId)
    }

    //get user's posts
    "return authentication required on GET /1/users/:id/posts" in {
      val Some(response) = route(FakeRequest(GET, "/1/users/1773/posts"))
      status(response) mustBe (UNAUTHORIZED)
    }

    "return a list of user's wallposts on /1/users/:id/posts" in {
      val Some(response) = route(FakeRequest(GET, "/1/users/1773/posts?size=2").withHeaders(("X-Auth-Token", authToken)))
      status(response) must equal(OK)
      contentType(response) mustBe Some("application/json")
      val jsonResponse = contentAsJson(response).as[Seq[WallPostResponse]]
      jsonResponse.size must equal(2)
    }

    //get wallpost's files
    "return file content on /1/posts/files/:id" in {
      val Some(wallposts) = route(FakeRequest(GET, "/1/posts?size=1").withHeaders(("X-Auth-Token", authToken)))
      val wallpostsResponse = contentAsJson(wallposts).as[Seq[WallPostResponse]]
      val imgWebUrl = wallpostsResponse(0).webImgUrl
      val imgBigUrl = wallpostsResponse(0).bigImgUrl
      val videoX264Url = wallpostsResponse(0).webVideoX264Url
      val videoVP8Url = wallpostsResponse(0).webVideoVp8Url

      //check web image
      if(!imgWebUrl.isEmpty){
        val Some(imgWebResponse) = route(FakeRequest(GET, imgWebUrl))
        status(imgWebResponse) must equal(OK)
        contentType(imgWebResponse) mustBe Some("image/jpeg")
        contentAsBytes(imgWebResponse).length must be >(0)
      }

      //check big image
      if(!imgBigUrl.isEmpty) {
        val Some(imgBigResponse) = route(FakeRequest(GET, imgBigUrl))
        status(imgBigResponse) must equal(OK)
        contentType(imgBigResponse) mustBe Some("image/jpeg")
        contentAsBytes(imgBigResponse).length must be >(0)
      }

      //check video x264 format
      if(!videoX264Url.isEmpty){
        val Some(videoX264Response) = route(FakeRequest(GET, videoX264Url))
        status(videoX264Response) must equal(OK)
        contentType(videoX264Response) mustBe Some("video/mp4")
        contentAsBytes(videoX264Response).length must be >(0)
      }

      if(!videoVP8Url.isEmpty) {
        val Some(videoVP8Response) = route(FakeRequest(GET, videoVP8Url))
        status(videoVP8Response) must equal(OK)
        contentType(videoVP8Response) mustBe Some("video/webm")
        contentAsBytes(videoVP8Response).length must be >(0)
      }
    }

    "return authentication required on POST /1/posts" in {
      val tempFile = File.createTempFile("test", "jpg")
      val data:MultipartFormData[TemporaryFile] = MultipartFormData(Map[String,Seq[String]](),
        List(
          FilePart("file","test.jpg",Some("Content-Type: multipart/form-data"),TemporaryFile(tempFile))
        ),
        List(),
        List())
      val Some(response) = route(FakeRequest(POST, "/1/posts").withMultipartFormDataBody(data))
      status(response) mustBe (UNAUTHORIZED)
    }

    //add a new image post
    "add a new image post" in {
      val basePath = app.path.getCanonicalPath
      val imageFile = new File(basePath + "/test/resources/test.jpg")
      val tempFile = File.createTempFile("test", "jpg")
      Files.copy(imageFile.toPath, tempFile.toPath, StandardCopyOption.REPLACE_EXISTING)
      val data:MultipartFormData[TemporaryFile] = MultipartFormData(Map[String,Seq[String]](),
        List(
          FilePart("file","test.jpg",Some("Content-Type: multipart/form-data"),TemporaryFile(tempFile))
        ),
        List(),
        List())

      val Some(response) = route(FakeRequest(POST, "/1/posts?text=Automated%20integration%20test%20image")
        .withMultipartFormDataBody(data)
        .withHeaders(("X-Auth-Token", authToken)))

      val jsonResponse = contentAsJson(response).as[WallPostResponse]
      status(response) must equal(CREATED)
      contentType(response) mustBe Some("application/json")
      jsonResponse mustBe a [WallPostResponse]
    }



    //add a new video post
    "add a new video post" in {
      val basePath = app.path.getCanonicalPath
      val videoFile = new File(basePath + "/test/resources/test.mp4")
      val tempFile = File.createTempFile("test", "mp4")
      Files.copy(videoFile.toPath, tempFile.toPath, StandardCopyOption.REPLACE_EXISTING)

      val data:MultipartFormData[TemporaryFile] = MultipartFormData(Map[String,Seq[String]](),
        List(
          FilePart("file","test.mp4",Some("Content-Type: multipart/form-data"),TemporaryFile(tempFile))
        ),
        List(),
        List()
      )

      val Some(response) = route(FakeRequest(POST, "/1/posts?text=Automated%20integration%20test%20video")
        .withMultipartFormDataBody(data)
        .withHeaders(("X-Auth-Token", authToken)))

      val jsonResponse = contentAsJson(response).as[WallPostResponse]
      status(response) must equal(CREATED)
      contentType(response) mustBe Some("application/json")
      jsonResponse mustBe a [WallPostResponse]
    }
  }


}
