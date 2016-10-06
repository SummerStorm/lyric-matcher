package matcher

import java.io.File
import java.io.FileWriter
import scala.collection.JavaConversions._
import scala.io.Source
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import scala.io.Codec
import java.nio.charset.CodingErrorAction
import java.net.URLEncoder

object LyricDownloader {
	val SourceExtension = ".lrc"
	val SourceExtension2 = ".ass"
	val OutputExtension = ".mp3"
	val SourcePath = "/home/user/Raw Lyrics"
	val OutputPath = "/home/user/Music"
	val ManualSourcePath = "/home/user/Manual Lyrics"

	def main(args: Array[String]) = {
		Task.log("""<head><meta http-equiv="Content-Type" content="text/html; charset=utf-8"></head>\n""")

		getFileTree(new File(SourcePath)).toIndexedSeq.map(println)

		//		val targets = getTargetFiles() //.filter(_.getName().contains("Clover"))
		//		targets map println
		//		println(targets.size + " targets found.")

		//		val start = System.currentTimeMillis
		//		targets.foreach(new Task(_).process())
		//		println("Time taken: " + (System.currentTimeMillis() - start) / 1000)
		println
	}

	def stripOffExtension(file: File) = file.getAbsolutePath().split("\\.(?=[^\\.]+$)")(0)
	def getBaseName(file: File) = file.getName().split("\\.(?=[^\\.]+$)")(0)

	def getTargetFiles() = {
		val source = getFileTree(new File(SourcePath)).filter(file => file.isFile() && file.getName().endsWith(SourceExtension)).map(stripOffExtension).toSet
		//		source map println
		//		getFileTree(new File(SourcePath)).toIndexedSeq.map(println)
		val source2 = getFileTree(new File(SourcePath)).filter(file => file.isFile() && file.getName().endsWith(SourceExtension2)).map(stripOffExtension).toSet
		val output = getFileTree(new File(OutputPath)).filter(file => file.isFile() && file.getName().endsWith(OutputExtension)).map(stripOffExtension).toSet
		val basenames = source2.map(_.substring(SourcePath.length())) ++ source.map(_.substring(SourcePath.length())) -- output.map(_.substring(OutputPath.length()))
		basenames.map(name => new File(name + SourceExtension))
	}

	def getFileTree(f: File): Stream[File] =
		f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree)
		else Stream.empty)

	class Task(val file: File) {
		def process(): Unit = {
			//			Thread.sleep(2000)
			val tokens = getBaseName(file).split(" - ")
			val artist = tokens(0)
			val title = tokens(1)
			val dir = checkDirectory(OutputPath, artist)
			println("processing : " + artist + " - " + title)

			//			val lyricUrl = smartSearch(artist, title)
			val lyricUrl: Option[String] = None
			if (lyricUrl.isEmpty) {
				println("Failed!!")

				val artistUrl = "http://lrc.feiyes.net/?ar=" + URLEncoder.encode(artist, "gbk")
				val songUrl = "http://lrc.feiyes.net/?ti=" + URLEncoder.encode("%" + title.take(6) + "%", "gbk")

				val line = artist + " - " + title + "  , " + "<a href=" + artistUrl + ">" + artist + "</a>" + "     " + "<a href=" + songUrl + ">" + title + "</a>  ,  <br>\n"
				Task.log(line)

				val dir = checkDirectory(ManualSourcePath, artist)
				val fw = new FileWriter(new File(dir, artist + " - " + title + OutputExtension))
				fw.write(artist + " " + title + "   LRC歌词\n")
				fw.close()

				return
			}

			println("fetching url " + lyricUrl)

			val lyricsSource = Source.fromURL(lyricUrl.get)(Codec("gbk"))
			val fw = new FileWriter(new File(dir, artist + " - " + title + OutputExtension))

			try {
				fw.write(lyricsSource.getLines.mkString("\n"))
			} catch {
				case ex: java.nio.charset.MalformedInputException => {
					val lyricsSource = Source.fromURL(lyricUrl.get)(Codec("utf-8"))
					fw.write(lyricsSource.getLines.mkString("\n"))
					lyricsSource.close()
				}
			}
			lyricsSource.close()
			fw.close
		}

		def checkDirectory(root: String, artist: String) = {
			val dir = new File(root, artist)
			if (!dir.exists())
				dir.mkdirs()
			dir
		}

		def smartSearch(artist: String, title: String): Option[String] =
			search(title, artist) match {
				//				case None => {
				//					println("First try failed! Using backup with title only.")
				//					search(title) // If the first try got us nothing, go with a broader search
				//				}
				case result => result
			}

		def search(title: String, artist: String = ""): Option[String] = {
			//http://lrc.feiyes.net/?ar=artist&al=&ti=title&x=22&y=11
			//		val url = "http://lrc.feiyes.net/?ar=" + artist.getBytes("gbk") + "&ti=" + title.getBytes("gbk")
			val url = if (artist == "")
				"http://lrc.feiyes.net/?ti=" + URLEncoder.encode("%" + title + "%", "gbk")
			else
				"http://lrc.feiyes.net/?ar=" + URLEncoder.encode("%" + artist.take(4) + "%", "gbk") + "&ti=" + URLEncoder.encode("%" + title.take(8) + "%", "gbk")
			println(url)

			val doc = Jsoup.connect(url)
				//			.userAgent("Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; .NET CLR 1.0.3705;)")
				.get()

			val notFound = doc.select("span.bigtext:nth-child(1)").first()
			if (notFound != null)
				return None

			val downloadBar = doc.select("#down").first()
			val downloadLinks = downloadBar.select("a[href]").map(_.attr("href")).filter(_.endsWith("&t=lrc&ac=dl"))
			Some("http://lrc.feiyes.net/" + downloadLinks(0))
		}

		def smartGoogle(artist: String, title: String) =
			google(artist, title) match {
				case Seq() => google(title) // If the first try got us nothing, go with a broader search
				case result => result
			}

		def google(args: String*): Seq[String] = {
			val url = "https://www.google.com/search?q=site:lrc.feiyes.net+" + args.mkString("+")
			println(url)

			val doc = Jsoup.connect(url)
				.userAgent("Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.2; .NET CLR 1.0.3705;)")
				.get()

			//		println(doc.toString())
			// get all links
			//		val links = doc.select("a[href]");
			//		links.foreach(link => println(link.attr("href") + "  with text " + link.text()))

			val searchResults = doc.select(".g").map(result => result.select(".r").select("a[href]")(0)).map(link => link.attr("href"))
			println(searchResults.length)
			searchResults
		}
	}

	object Task {
		def log(line: String) {
			val fw = new FileWriter("log.html", true);
			fw.write(line)
			fw.close()
		}
	}
}
