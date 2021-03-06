package matcher

import java.io.File
import java.io.FileWriter
import scala.collection.JavaConversions._
import scala.io.Source
import java.io.PrintWriter
import scala.sys.process._

object LyricConverter {
	val SourceExtension = ".lrc"
	val SourceExtension2 = ".ass"
	val OutputExtension = ".lrc"
	val Path = "/home/user/Lyrics/Raw Lyrics/"

	def main(args: Array[String]) = {
		val targets = getTargetFiles(Path).toSeq
		targets map println

		targets.foreach(new ConvertTask(_).process())

		val outputTargets = getTargetFiles(Path).par
		println("Removing Chinese.")
		outputTargets.foreach(new RemoveChineseTask(_).process())
		println("Removing Furigana.")
		outputTargets.foreach(new RemoveFuriganaTask(_).process())
		println("Fixing spaces.")
		outputTargets.foreach(new FixSpaceTask(_).process())
	}

	def getBaseName(file: File) = file.getName().split("\\.(?=[^\\.]+$)")(0)
	def getExtension(file: File) = file.getName().split("\\.(?=[^\\.]+$)")(1)

	def getTargetFiles(targetPath: String) = {
		val source = getFileTree(new File(targetPath))
			.filter(file =>
				file.isFile() &&
					(file.getName().endsWith(SourceExtension)
						|| file.getName().endsWith(SourceExtension2)))
		source
	}

	def getFileTree(f: File): Stream[File] =
		f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
		else Stream.empty)

	class ConvertTask(val file: File) {
		def process(): Unit = {
			val tokens = getBaseName(file).split(" - ")
			val artist = tokens(0)
			val title = tokens(1)

			val lyricsSource = Source.fromFile(file) //("UTF-8")
			val input = lyricsSource.getLines().toIndexedSeq
			lyricsSource.close

			val lyrics = getExtension(file) match {
				case "lrc" => LrcParser(artist, title, input)
				case "ass" => AssParser(input)
				case _ => throw new Exception("Unepxcted extension on " + file.getAbsolutePath())
			}
			val writer = new PrintWriter(new File(Path, getBaseName(file) + OutputExtension))
			writer.write("[offset:0]\n")
			writer.write(lyrics.toString)
			writer.close()
		}
	}

	class RemoveChineseTask(val file: File) {
		def process(): Unit = {
			val lyricsSource = Source.fromFile(file)
			val input = lyricsSource.getLines().toIndexedSeq
			lyricsSource.close()

			val seperator = getSeperator(input)
			if (seperator.isEmpty)
				return

			val output = processLines(seperator.get, input).mkString("\n")

			val writer = new PrintWriter(file)
			writer.write(output)
			writer.close()
		}

		def processLines(seperator: String, input: Seq[String]) = {
			input.map(_.split(seperator)(0).trim())
		}

		val seperators = List("【", "〖", "/", "『")
		def getSeperator(lines: Seq[String]): Option[String] = {
			val seperatorsFound = seperators.map(sep => lines.filter(_.contains(sep)).length > lines.length / 4)
			val numberOfSeperators = seperatorsFound.count(_ == true)
			numberOfSeperators match {
				case 0 => None
				case 1 => Some(seperatorsFound.zip(seperators).filter(pair => pair._1 == true)(0)._2)
				case x if x > 1 => throw new Exception("Fatal error! Multiple seperators found on " + lines.mkString("\n"))
			}
		}
	}

	class RemoveFuriganaTask(val file: File) {
		def process(): Unit = {
			val lyricsSource = Source.fromFile(file)
			val input = lyricsSource.getLines().toIndexedSeq
			lyricsSource.close()

			if (!isTarget(input))
				return

			val output = input.map(processLines).mkString("\n")

			val writer = new PrintWriter(file)
			writer.write(output)
			writer.close()
		}

		def processLines(input: String) = {
			val temp = processLinesHelper(input, "\\(", "\\)")
			processLinesHelper(temp, brackets2(0), brackets2(1))
		}

		def processLinesHelper(input: String, start: String, end: String) = {
			val firstSplit = input.split(start)
			val head = firstSplit(0)
			val tail = firstSplit.drop(1).map {
				_.split(end) match {
					case Array(_, x) => x
					case Array(_) => ""
				}
			}
			(head ++ tail).mkString("")
		}

		val brackets1 = List("(", ")")
		val brackets2 = List("（", "）")
		val allBrackets = List(brackets1, brackets2)
		def isTarget(lines: Seq[String]) = {
			val count = lines.filter(line => brackets1.forall(line.contains(_)) || brackets2.forall(line.contains(_))).size
			count > lines.size / 2
		}
	}

	class FixSpaceTask(val file: File) {
		def process(): Unit = {
			val lyricsSource = Source.fromFile(file)
			val input = lyricsSource.getLines().toIndexedSeq
			lyricsSource.close()

			val output = input.map(processLines).mkString("\n")

			val writer = new PrintWriter(file)
			writer.write(output)
			writer.close()
		}

		def processLines(input: String) = {
			val result = new StringBuilder()
			input.split(" ").foreach {
				case x if x.isEmpty() =>
				case token =>
					result.append(token)
					if (token.last.isASCIILetter)
						result.append(" ")
					else
						result.append("\u3000") //IDEOGRAPHIC SPACE
			}
			val resultString = result.toString
			if (resultString.last.isSpaceChar || resultString.last == "\u3000")
				resultString.dropRight(1)
			else
				resultString
		}
	}

	implicit class CharProperties(val ch: Char) extends AnyVal {
		def isASCIILetter: Boolean =
			(ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
	}
}

