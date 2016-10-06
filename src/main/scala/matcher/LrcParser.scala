package matcher

import java.io.PrintWriter
import java.io.File
import org.scalatest.time.Milliseconds

abstract class LyricFile(val timedLines: Seq[(Int, String)]) {
	override def toString = timedLines.map(formatLine(_)).mkString("\n")

	private def formatLine(pair: (Int, String)) = {
		val milliseconds = pair._1
		val centiseconds = milliseconds % 1000 / 10
		val minutes = milliseconds / 60000
		val seconds = milliseconds / 1000 % 60
		assert(minutes < 100)
		assert(seconds < 60)
		assert(centiseconds < 100)
		"[" + "%02d".format(minutes) + ":" + "%02d".format(seconds) + "." + "%02d".format(centiseconds) + "]" + pair._2
	}
}

class LrcParser(timedLines: Seq[(Int, String)]) extends LyricFile(timedLines) {}
class AssParser(timedLines: Seq[(Int, String)]) extends LyricFile(timedLines) {}

object LrcParser {
	def apply(artist: String, title: String, lines: Seq[String]): LrcParser = {
		val lyricStart = lines.zipWithIndex.find(_._1.startsWith("[0")).get._2
		val (headers, rawLyrics) = lines.splitAt(lyricStart)
		val headerPairs = headers.map(_.drop(1).dropRight(1).split(":")).map(
			array =>
				array.length match {
					case 1 => (array(0), "")
					case 2 => (array(0), array(1))
					case _ => (array(0), array.drop(1).mkString(":"))
				}).toMap

		val offset = headerPairs.getOrElse("offset", "0").toInt

		//		val startLine = rawLyrics.zipWithIndex.find(_._1.endsWith("]")).get._2
		//		val endLine = rawLyrics.zipWithIndex.reverse.find(_._1.endsWith("]")).get._2
		//		val lyrics = rawLyrics.slice(startLine + 1, endLine).map(_.drop(1).split("]"))
		val lyrics = rawLyrics.filter(_.startsWith("[")).map(_.drop(1).split("]"))
		val timedLines = lyrics.map {
			array =>
				if (array.length == 1)
					(parseTime(array(0)) + offset, "")
				else if (array.length == 2)
					(parseTime(array(0)) + offset, array(1).trim())
				else
					throw new Exception("Unexpected array length at " + array.length)
		}
		//		timedLines map println
		val filteredLines = removeTitles(artist, title, timedLines)
		new LrcParser(filteredLines)
	}

	private def removeTitles(artist: String, title: String, timedLines: Seq[(Int, String)]) = {
		val (times, lines) = timedLines.unzip
		val removalTargets = List("TVアニメ", "www.", "lrc by", "Vo.", "制作", "编辑", "作/編曲", "PCゲーム", "ゲーム", "唄", "作曲", "編曲", "编曲", "作词", "作詞", "Make By", "Made by", "歌")
		//		val removalTargets = List("www.", "lrc by", "Vo.", "制作", "编辑", "作/編曲", "PCゲーム", "ゲーム", "唄", "作曲", "編曲", "编曲", "作词", "作詞", "Make By", "Made by", "歌")
		def isTarget(line: String) = removalTargets.exists(line.startsWith(_) || line.toLowerCase() == artist.toLowerCase() || line.toLowerCase() == title.toLowerCase())
		val (_, targetIndex) = lines.zipWithIndex.filter(pair => isTarget(pair._1)).unzip
		val (_, blankLinesIndex) = lines.zipWithIndex.filter(_._1 == "").unzip
		val blankLinesBlockEnd = getBlankLinesBlockEnd(blankLinesIndex)
		val lyricIndex = (0 to lines.length).toSet -- blankLinesIndex -- targetIndex
		val lyricStart = lyricIndex.min
		var score = 0
		//		println("\n")
		(blankLinesBlockEnd to blankLinesBlockEnd + 10).map { i =>
			//			println(i + " " + (blankLinesIndex.contains(i) || targetIndex.contains(i)) + " " + lines(i))
			if (blankLinesIndex.contains(i) || targetIndex.contains(i))
				score += 1
		}

		if (score > 2 && lyricStart != 0) {
			//			println("/home/user/Raw Lyrics/" + artist + "/" + artist + " - " + title + ".lrc")
			(blankLinesBlockEnd to blankLinesBlockEnd + 10).map { i =>
				//				println(i + " " + (blankLinesIndex.contains(i) || targetIndex.contains(i)) + " " + lines(i))
			}

			//			println("\nTitle found! Skipping the first " + lyricStart + " lines.")
			//			timedLines.drop(lyricStart).take(5).map(pair => println(pair._2))
			timedLines.drop(lyricStart)
		} else
			timedLines
	}

	private def getBlankLinesBlockEnd(index: Seq[Int]): Int = {
		var highest = 0
		for (i <- 0 to 10)
			if (index.contains(i))
				highest = i
			else
				return highest
		return 0
	}

	private def parseTime(string: String) = {
		val tokens = string.split(":|\\.")
		val minutes = tokens(0).toInt
		val seconds = tokens(1).toInt
		var milliseconds = tokens(2).toInt
		if (milliseconds < 100)
			milliseconds *= 10
		assert(seconds < 60)
		(minutes * 60 + seconds) * 1000 + milliseconds
	}
}

object AssParser {
	def apply(lines: Seq[String]): AssParser = {
		val lyricStart = lines.zipWithIndex.find(_._1 == "[Events]").get._2 + 2
		val (headers, rawLyrics) = lines.splitAt(lyricStart)

		val lyrics = rawLyrics.map { line =>
			val tokens = line.split(",")
			val timeText = tokens(1)
			val lyricText = if (line.last == ',')
				""
			else
				tokens.last
			val time = parseTime(timeText)
			(time, lyricText)
		}
		new AssParser(lyrics)
	}

	private def parseTime(string: String) = {
		val tokens = string.split(":|\\.")
		val hours = tokens(0).toInt
		val minutes = tokens(1).toInt
		val seconds = tokens(2).toInt
		val milliseconds = tokens(3).toInt
		assert(milliseconds < 100)
		assert(seconds < 60)
		((hours * 60 + minutes) * 60 + seconds) * 1000 + milliseconds
	}
}