package controllers

import play.api._
import play.api.mvc._

import org.xml.sax.InputSource
import org.htmlcleaner._
import scala.collection.mutable.ListBuffer
import scala.xml._
import java.net._
import parsing._
import scala.io.Source
import java.net.URL
import scala.util.matching.Regex
import scala.collection.mutable.ArrayBuffer
import scala.io._
import java.io._

object Application extends Controller {

    // Call index.scala.html
  	def index = Action {
		val result = grabURLBack(getRealIndex("http://www.ptt.cc/bbs/BikerShop/index.html", 1),10)
  		Ok(views.html.index("Your new application is not ready.", result.toArray))
  	}

  	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
        val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
	}    

    def grabURLArray(urlArray: ArrayBuffer[String]) = {
        var posts_result = ArrayBuffer[String]()
        for (url <- urlArray) {
            val posts = new post(url)
            posts_result.append(url)
            posts_result.appendAll( posts.urlToResult(url) )
            posts_result.append("=====")

        }
        posts_result
    }
    
    def getRealIndex(index_url: String, op: Int):String = {
        var real_index = ""
        var html = Source.fromURL(index_url)
        var sourceString = html.mkString
        var perLineString = sourceString.split("\n")
        var linesWithArrow = perLineString.filter(_.contains("上頁"))
        if(op!=1){
            linesWithArrow = perLineString.filter(_.contains("下頁"))
        }

        for (row <- linesWithArrow) {
            var rowSplit = row.split("\"")
            for (r <- rowSplit) {
                if(r.contains("/bbs/BikerShop")) {
                    var r2 = "http://www.ptt.cc" + r
                    // println(r2)
                    real_index = r2
                }
            }
        }

        if(op!=1)
            real_index
        else{
            real_index = getRealIndex(real_index, 2)
            real_index
        }

    }

    def grabURLBack(url: String, amount : Int)  = {
        val start = url.substring(37,41).toInt
        var result = ArrayBuffer[String]()
        for( k <- 0 to amount) {
            var postListResult = new postList("")
            println("http://www.ptt.cc/bbs/BikerShop/index"+(start-k)+".html")

            oncePerSecond()
            result.appendAll(grabURLArray(postListResult.urlToResult("http://www.ptt.cc/bbs/BikerShop/index"+(start-k)+".html")))
            // result.appendAll(grabURLArray(postListResult.urlToResult("http://www.ptt.cc/bbs/BikerShop/index"+(start-k)+".html")))
        }
        
        result
    }

    def oncePerSecond() {
        Thread sleep 1000
    }

}

class post(url: String){

    def parseURLAndSplitPerLine(theURL: String) : Array[String] = {
        val html = Source.fromURL(theURL)
        val sourceString = html.mkString

        val perLineString = sourceString.split("\n")
        val clean_perLineString = perLineString.drop(50)

        clean_perLineString
        // val pattern = new Regex("《.....")
        // val str = "《Item》→光陽KYMCO三冠王125"
    }

    def findArrows(stringArray: Array[String]) : Array[String] = {

        val linesWithArrow = stringArray.filter(_.contains("\u300A"))
        val linesWithoutContent = linesWithArrow.filter(!_.contains("content"))
        // kk.foreach(_.println)
        val linesWithArrow2 = linesWithArrow.drop(2)
        linesWithArrow
        // println(kd)
    }

    def getNameAndPrice(stringArray: Array[String]) : ArrayBuffer[String] = {
        var resultArray = ArrayBuffer[String]()
        for (row <- stringArray) {
            if(!row.contains("必填")&&(row.contains("價格")||row.contains("名稱"))){
                val rowSplit = row.split("\u2192")
                for (r <- rowSplit) {
                    if(!r.contains("\u300A")||(r.contains("價格")&& r.length > 10)||(r.contains("名稱")&& r.length > 10)) {
                        if(r.contains("\u300A")){
                            for( i <- 0 to r.length-1) { 
                                if(r.substring(i,i+1) == "\u300B"){ 
                                    val cleanR = r.substring(i+1,r.length-1) 
                                    println(cleanR) 
                                    resultArray += cleanR 
                                } 
                            } 
                        } 
                        else { 
                            println(r) 
                            resultArray += r 
                        }

                        // println(r)
                        // resultArray += r
                    }
                }
            }
        }
        resultArray
    }

    def urlToResult(url: String) : ArrayBuffer[String] = {
        getNameAndPrice(findArrows(parseURLAndSplitPerLine(url)))
    }
}


class postList(url: String) {

    def parseURLAndSplitPerLine(theURL: String) : Array[String] = {
        val html = Source.fromURL(theURL)
        val sourceString = html.mkString
        val perLineString = sourceString.split("\n")
        perLineString
    }

    //<a href="/bbs/BikerShop/M.1402146694.A.E0E.html">[舊@新店:排氣管] 吉村R77碳纖維管For K-XCT300i</a>

    def findArrows(stringArray: Array[String]) : Array[String] = {
        val linesWithArrow = stringArray.filter(_.contains("舊"))
        val linesWithBike = linesWithArrow.filter(_.contains("機車"))
        linesWithBike
    }

    def getURLforPost(stringArray: Array[String]) : ArrayBuffer[String] = {
        var resultArray = ArrayBuffer[String]()
        for (row <- stringArray) {
            val rowSplit = row.split("\"")
            for (r <- rowSplit) {
                if(r.contains("/bbs/BikerShop")) {
                    val r2 = "http://www.ptt.cc" + r
                    println(r2)
                    resultArray += r2
                }
            }
        }
        resultArray
    }


    def urlToResult(url: String) : ArrayBuffer[String] = {
        getURLforPost(findArrows(parseURLAndSplitPerLine(url)))
    }

}