package services

import java.awt.image.BufferedImage
import java.io.File


import controllers.WallPostRequest
import models.WallPostModel._
import models.{WallPost, User, Notification}

import org.specs2.mock._
import org.specs2.mutable.Specification
import play.api.libs.Files
import play.api.mvc.MultipartFormData
import play.api.test.PlaySpecification
import util.ImageUtils._


import util.{XugglerUtils, ImageUtils}

import scala.concurrent.Future


class WallPostServiceSpec extends PlaySpecification with Mockito{

  val wallpostMock = mock[WallPost]
  val userMock = mock[User]
  val notificationMock = mock[Notification]
  val xugglerMock = mock[XugglerUtils]
  val imageMock = mock[ImageUtils]
  val fileMock = mock[File]
  fileMock.exists returns true


  object service extends WallPostService(wallpostMock, userMock, notificationMock, xugglerMock, imageMock){
    override def getWallPostDiskFile(localPath: Option[String]): File = {
      return fileMock
    }

    override def getWallPostPath(userId: Int): WallPostPath = {
      WallPostPath("rootPath", "localImagesPath", "localVideosPath", "filename")
    }
  }

  sequential

  "WallPostService should" >> {
    "return a list of WallPostResponse on listWallPosts" in {
      val postMock = mock[WallPostResponse]
      val wallposts = IndexedSeq(postMock)

      wallpostMock.listWallPosts(any[Int], any[Int], any[String]) returns Future.successful(wallposts)

      val result = await(service.listWallPosts(0, 10, "alex@emai.com") )

      result should be equalTo(wallposts)
    }

    "return a WallPostFile on loadWallPostFile" in {
      val postFileMock = mock[PostFile]

      wallpostMock.loadWallPostFile(any[Int]) returns Future.successful(Some(postFileMock))

      val result1 = await(service.loadWallPostFile(1, "jpg", ""))
      val result2 = await(service.loadWallPostFile(1, "mpeg", ""))

      result1 should be equalTo(Some(WallPostFile(fileMock, "image/jpeg")))
      result2 should be equalTo(None)
    }

    "return a PostFile on saveWallPostFile" in {
      val mockWallPostRequest = mock[WallPostRequest]
      mockWallPostRequest.exifOrientation returns None

      val mockFile = mock[File]
      val mockTemporaryFile = mock[Files.TemporaryFile]
      mockTemporaryFile.file returns mockFile

      val mockMultipartDataMp4 = mock[MultipartFormData.FilePart[Files.TemporaryFile]]
      mockMultipartDataMp4.filename returns "mockfile.mp4"
      mockMultipartDataMp4.ref returns mockTemporaryFile

      val mockMultipartDataJpg = mock[MultipartFormData.FilePart[Files.TemporaryFile]]
      mockMultipartDataJpg.filename returns "mockfile.jpg"
      mockMultipartDataJpg.ref returns mockTemporaryFile

      val mockMultipartDataMpeg = mock[MultipartFormData.FilePart[Files.TemporaryFile]]
      mockMultipartDataMpeg.filename returns "mockfile.mpeg"
      mockMultipartDataMpeg.ref returns mockTemporaryFile

      val mockBufferedImage = mock[BufferedImage]


      xugglerMock.convert_x264(any[File], any[String], any[String], any[String]) returns Future.successful("x264.mp4")
      xugglerMock.extract_frame(any[File], any[String], any[String], any[String]) returns Future.successful("frame.jpg")

      imageMock.rotate(any[File], any[Option[Int]]) returns Future.successful(mockBufferedImage)
      imageMock.saveImage(any[String], any[String], any[String], any[String], any[BufferedImage], ===(IMAGE_WEB_WIDTH), ===(IMAGE_WEB_HEIGHT)) returns Future.successful(Some("web.jpg"))
      imageMock.saveImage(any[String], any[String], any[String], any[String], any[BufferedImage], ===(IMAGE_BIG_WIDTH), ===(IMAGE_BIG_HEIGHT)) returns Future.successful(Some("big.jpg"))

      val expectedMp4Result = PostFile(Some("frame.jpg"), None, Some("x264.mp4"), None, WALL_POST_EMBEDDED_VIDEO)
      val expectedJpgResult = PostFile(Some("web.jpg"), Some("big.jpg"), None, None, WALL_POST_PHOTO)

      await(service.saveWallPostFile(Some(mockMultipartDataMp4), 1, mockWallPostRequest)) should be equalTo(expectedMp4Result)
      await(service.saveWallPostFile(Some(mockMultipartDataJpg), 1, mockWallPostRequest)) should be equalTo(expectedJpgResult)
      await(service.saveWallPostFile(Some(mockMultipartDataMpeg), 1, mockWallPostRequest)) must throwAn[IllegalArgumentException]

    }
  }


}
