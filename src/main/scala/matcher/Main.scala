package matcher

import scala.collection.JavaConversions._
import java.io.File

object Main {
	val MusicPath = "/home/user/Music"
	val LyricsPath = "/home/user/Raw Lyrics"

	def main(args: Array[String]) = {
		val mp3List = getFileTree(new File(MusicPath)).filter(_.getName().endsWith(".mp3")).map(stripOffExtension).toSet
		val assList = getFileTree(new File(LyricsPath)).filter(file => file.getName().endsWith(".ass") || file.getName().endsWith(".lrc")).map(stripOffExtension).toSet
		val mp3MinusLrc = mp3List -- assList
		val lrcMinusMp3 = assList -- mp3List
		println(mp3MinusLrc.size)
		println(mp3MinusLrc.mkString("\n"))

		println(lrcMinusMp3.size)
		println(lrcMinusMp3.mkString("\n"))

		println(lrcMinusMp3 == mp3MinusLrc)
	}

	def stripOffExtension(file: File) = file.getName().split("\\.(?=[^\\.]+$)")(0)

	def getFileTree(f: File): Stream[File] =
		f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
		else Stream.empty)
}