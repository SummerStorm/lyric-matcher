package matcher

import scala.collection.JavaConversions._
import java.io.File
import scala.sys.process._
import scala.concurrent._
import ExecutionContext.Implicits.global
import matcher._
import scala.slick.driver.PostgresDriver.simple._

object LyricQuery {
	def main(args: Array[String]) = {
		val targets = getTargetFiles()
		val lyricsTargets = targets.filter(_.getName.takeRight(4) == ".mp3").take(2)

		val start = System.currentTimeMillis
		//		lyricsTargets.par.foreach(new UploadTask(_).process())
		lyricsTargets.foreach(new QueryTask(_).process())
		println("Time taken: " + (System.currentTimeMillis() - start) / 1000)
	}

	def getTargetFiles() = getFileTree(new File("/home/user/Music")).toArray

	def getFileTree(f: File): Stream[File] =
		f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
		else Stream.empty)
}

class QueryTask(val file: File) {
	def process(): Unit = {
		val tokens = file.getName().split("\\.")(0).split(" - ")
		val artist = tokens(0)
		val title = tokens(1)
		println
		println(artist)
		println(title)

		DB.withSession { implicit session =>
			val query = for {
				lyric <- TableQuery[Lyrics] if lyric.singer === artist //&& lyric.title === title 
			} yield (lyric)

			query.foreach(lyric => println(lyric.title))
		}
	}
}