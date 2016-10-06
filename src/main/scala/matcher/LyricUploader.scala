package matcher

import scala.collection.JavaConversions._
import java.io.File
import scala.sys.process._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.slick.driver.PostgresDriver.simple._
import com.mchange.v2.c3p0.ComboPooledDataSource

object LyricUploader {
	val LyricsPath = List("data/kasi_time", "data/uta_net")

	def main(args: Array[String]) = {
		DB.withSession { implicit session =>
			val t = TableQuery[Lyrics]
			t.ddl.create
		}
		val targets = getTargetFiles()
		val lyricsTargets = targets.filter(_.getAbsolutePath.takeRight(6) == ".lyric") //.take(100)

		val start = System.currentTimeMillis
		lyricsTargets.par.foreach(new UploadTask(_).process())
		//		lyricsTargets.foreach(new UploadTask(_).process())
		println("Time taken: " + (System.currentTimeMillis() - start) / 1000)
	}

	def getTargetFiles() = getFileTree(new File("/home/user/workspace/lyric-matcher/data")).toArray

	def getFileTree(f: File): Stream[File] =
		f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
		else Stream.empty)
}

class UploadTask(val file: File) {
	def process() = {
		val tokens = file.getAbsolutePath.split('/')
		val source = tokens.takeRight(2)(0)
		val fileId = tokens.last.split("\\.")(0).toInt

		val fileHandle = io.Source.fromFile(file)
		val lines = fileHandle.getLines.map(unescapeLine).toArray
		fileHandle.close()

		//		println(file.getAbsolutePath())

		val title = lines(0)
		var singer = lines(2)
		if (!lines(3).startsWith("作詞："))
			singer += lines(3).trim()
		assert(singer.startsWith("歌手："))
		singer = singer.substring(3)

		val startLine = lines.zipWithIndex.slice(2, 10).find(_._1 == "").get._2 + 2
		val lyrics = lines.drop(startLine).filter(_.length > 0).mkString("\n")

		val id = source match {
			case "kasi_time" => 10000000 + fileId
			case "uta_net" => 20000000 + fileId
		}

		//		println=("\n\n==================\n")
		//		println(source)
		//		println(id)
		//		println(title)
		//		println(singer)
		//		println(lyrics)
		//		println("\n\n==================\n")

		DB.withSession { implicit session =>
			TableQuery[Lyrics] += Lyric(id, source, title, singer, lyrics)
		}
	}

	def unescapeLine(line: String) = {
		//		println("working on " + line)
		var lineout = line
		val pairs = Map(
			"&quot;" -> "\"",
			"&amp;" -> "&",
			"&#039;" -> "'",
			"&gt;" -> ">",
			"&lt;" -> ">")
		pairs.foreach(pair => lineout = lineout.replace(pair._1, pair._2))
		assert(!(lineout.contains("&") && lineout.contains("&amp")))
		lineout
	}
}

case class Lyric(id: Int, source: String, title: String, singer: String, lyrics: String)

class Lyrics(tag: Tag) extends Table[Lyric](tag, "Lyrics") {
	def id = column[Int]("id", O.PrimaryKey)
	def source = column[String]("source")
	def title = column[String]("title")
	def singer = column[String]("singer")
	def lyrics = column[String]("lyrics", O.DBType("VARCHAR(10000)"))
	def * = (id, source, title, singer, lyrics) <> (Lyric.tupled, Lyric.unapply)
}

//val users = TableQuery[Users]

object DB {
	val datasource = new ComboPooledDataSource()
	datasource.setDriverClass("org.postgresql.Driver")
	datasource.setJdbcUrl("jdbc:postgresql://server-home2/database")
	datasource.setUser("scala_generator")
	datasource.setPassword("asdf")

	final val db = Database.forURL("jdbc:postgresql://server-home2/database",
		driver = "org.postgresql.Driver",
		user = "scala_generator",
		password = "asdf")

	def withSession[T](f: Session => T): T = {
		//connection.withSession(f)
		Database.forDataSource(datasource) withSession(f)
	}
}
