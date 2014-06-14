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
import play.api.db._
import play.api.Play.current 

object Application extends Controller {

    // Call index.scala.html
  	def index = Action {
		val result = grabURLBack(getRealIndex("http://www.ptt.cc/bbs/BikerShop/index.html", 1),20)

  		Ok(views.html.index("Your new application is not ready.", result.toArray))
  	}

  	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
        val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
	}    

    def grabURLArray(urlArray: ArrayBuffer[String]) = {

        val posts_result = ArrayBuffer[String]()

        @annotation.tailrec
        def go(in: ArrayBuffer[String], result: ArrayBuffer[String]): ArrayBuffer[String] = {
            if(in.isEmpty)
                result
            else{
                println(in(0))
                val posts = new post(in(0))
                val resultArray  = result += in(0)
                val resultArray_1 = resultArray ++ posts.urlToResult(in(0))
                val resultArray_2 = resultArray_1 +=  "====="
                go(in.tail, resultArray_2)
            }
        }

        go(urlArray, posts_result)
        
        // for (url <- urlArray) {
        //     val posts = new post(url)
        //     posts_result.append(url)
        //     posts_result.appendAll( posts.urlToResult(url) )
        //     posts_result.append("=====")

        // }
        // posts_result
    }
    
    def getRealIndex(index_url: String, op: Int):String = {

        val html = Source.fromURL(index_url)
        val sourceString = html.mkString
        val perLineString = sourceString.split("\n")

        var linesWithArrow = perLineString.filter(_.contains("上頁"))

        if(op!=1){
            linesWithArrow = perLineString.filter(_.contains("下頁"))
        }

        // val linesWithArrow = if(op!=1){}else[}]

        @annotation.tailrec
        def go(in: Array[String], result: String):String = {
            if(in.isEmpty)
                result
            else{
                val row = in(0)
                val rowSplit = row.split("\"")
                val resultArray = gogo(rowSplit)
                go(in.tail, resultArray)
            }
        }

        @annotation.tailrec
        def gogo(in: Array[String]): String = {
            if(in(0).contains("/bbs/BikerShop")){
                val result = "http://www.ptt.cc" + in(0)
                result
            }else{
                gogo(in.tail)
            }
        }

        // for (row <- linesWithArrow) {
        //     val rowSplit = row.split("\"")
        //     for (r <- rowSplit) {
        //         if(r.contains("/bbs/BikerShop")) {
        //             var r2 = "http://www.ptt.cc" + r
        //             // println(r2)
        //             real_index = r2
        //         }
        //     }
        // }

        val real_index = go(linesWithArrow, "")
        if(op!=1)
            real_index
        else{
            getRealIndex(real_index, 2)
        }

    }

    def grabURLBack(url: String, amount : Int)  = {
        val start = url.substring(37,41).toInt
        // val result = ArrayBuffer[String]()

        // 
        val resultArray = ArrayBuffer[String]()

        @annotation.tailrec
        def grabURL(start: Int, count: Int, amount: Int, result: ArrayBuffer[String]): ArrayBuffer[String] = {
            if(count>=amount){
                result
            }else{
                oncePerSecond()
                val postListResult = new postList("")
                val resultArray = result ++ (grabURLArray(postListResult.urlToResult("http://www.ptt.cc/bbs/BikerShop/index"+(start-count)+".html")))
                // println(resultArray)
                grabURL(start, count+1, amount, resultArray)
            }
        }

        val result = grabURL(start, 0, amount, resultArray)
        // 

        // for( k <- 0 to amount) {
        //     var postListResult = new postList("")
        //     // println("http://www.ptt.cc/bbs/BikerShop/index"+(start-k)+".html")

        //     oncePerSecond()
        //     result.appendAll(grabURLArray(postListResult.urlToResult("http://www.ptt.cc/bbs/BikerShop/index"+(start-k)+".html")))
        // }


        val cleanResult = ArrayBuffer[String]()
        val cleanResult2 = ArrayBuffer[String]()


        @annotation.tailrec
        def cleanSpace(result: ArrayBuffer[String], cleanResult: ArrayBuffer[String]):ArrayBuffer[String] = {
            if(result.isEmpty){
                cleanResult
            }else{
                val item = result(0)
                if(item != Nil && item.toString != "" && item.length >= 2 && item.toString.take(5)!="<span"){
                    val cleanResult_1 = cleanResult += item
                    cleanSpace(result.tail, cleanResult_1)
                }else{
                    cleanSpace(result.tail, cleanResult)
                }
            }
            
        }

        // for(item <- result) {
        //     if(item != Nil && item.toString != "" && item.length >= 2 && item.toString.take(5)!="<span"){
        //         cleanResult.append(item)
        //     }
        // }

        val cleanResult_1 = cleanSpace(result, cleanResult)

        val result_length = cleanResult_1.length

        @annotation.tailrec
        def checkSet(count: Int, length: Int, result: ArrayBuffer[String], cleanSet: ArrayBuffer[String]):ArrayBuffer[String] = {
            if(count>=length-4){
                cleanSet
            }else{
                // println(result.length)
                // println(count)
                if((count+3)>=result.length){
                    println(result)
                }
                if(result(count).toString.takeRight(4) == "html" && result(count+3).toString == "====="){
                    val cleanSet_1 = cleanSet ++ ArrayBuffer[String](result(count), result(count+1), result(count+2), result(count+3))
                    checkSet(count+1, length, result, cleanSet_1)
                }else{
                    checkSet(count+1, length, result, cleanSet)
                }
            }
        }

        checkSet(0, result_length, cleanResult_1, cleanResult2)

        // for(i <- 0 to cleanResult_1.length-4) {
        //     if(cleanResult_1(i).toString.takeRight(4) == "html" && cleanResult_1(i+3).toString == "====="){
        //         cleanResult2.append(cleanResult_1(i))
        //         cleanResult2.append(cleanResult_1(i+1))
        //         cleanResult2.append(cleanResult_1(i+2))
        //         cleanResult2.append(cleanResult_1(i+3))
        //     }    
        // }

        // for(item <- cleanResult2) {
        //     println(item)
        // }
        // cleanResult2
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
        var rowSplitArray = ArrayBuffer[String]()

        @annotation.tailrec
        def checkFormat(input : Array[String], result: ArrayBuffer[String]) : ArrayBuffer[String] = {
            if (input.isEmpty) result
            else {
                val row = input(0)
                if(!row.contains("必填")&&(row.contains("價格")||row.contains("名稱"))){
                    val rowSplit = row.split("\u2192")
                    addRowToRowSplitArray(rowSplit, result)
                }
                checkFormat(input.tail,result)
            }
        }

        @annotation.tailrec
        def addRowToRowSplitArray(input : Array[String], result: ArrayBuffer[String]) : ArrayBuffer[String] = {
            if(input.isEmpty) result
            else {
                val row = input(0)
                result += row
                addRowToRowSplitArray(input.tail, result)
            }
        }

        val kk = checkFormat(stringArray, rowSplitArray)

        @annotation.tailrec
        def delLabel(in: ArrayBuffer[String], result: ArrayBuffer[String]):ArrayBuffer[String] = {
            if(in.isEmpty){
                result
            }else{
                val r = in(0)
                if(!r.contains("\u300A")||(r.contains("價格")&& r.length > 10)||(r.contains("名稱")&& r.length > 10)) {
                    if(r.contains("\u300A")){
                        delLabel(in.tail, result += inDelLabel(0, r.length, r, ""))
                    }else delLabel(in.tail, result += r)
                }else delLabel(in.tail, result)
            }
        }

        @annotation.tailrec
        def inDelLabel(count: Int, length: Int, in: String, result: String): String = {
            if(count >= length-1){
                result
            }else{
                val r = in
                if(r.substring(count,count+1) == "\u300B"){ 
                    val cleanR = r.substring(count+1,r.length-1) 
                    // println(cleanR) 
                    // result = cleanR 
                    inDelLabel(count+1,length, in, cleanR)
                }else inDelLabel(count+1,length, in, result)
            }
        }

        delLabel(kk, ArrayBuffer[String]())

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
        val resultArray = ArrayBuffer[String]()

        @annotation.tailrec
        def go(in: Array[String], result: ArrayBuffer[String]): ArrayBuffer[String] = {
            if(in.isEmpty)
                result
            else{
                val row = in(0)
                val rowSplit = row.split("\"")
                val resultArray = result ++ gogo(rowSplit)
                go(in.tail, resultArray)
            }
        }

        @annotation.tailrec
        def gogo(in: Array[String]): ArrayBuffer[String] = {
            if(in(0).contains("/bbs/BikerShop")){
                val result = ArrayBuffer[String]("http://www.ptt.cc" + in(0))
                result
            }
            else{
                gogo(in.tail)
            }
        }     

        go(stringArray, resultArray)

        // for (row <- stringArray) {
        //     val rowSplit = row.split("\"")
        //     for (r <- rowSplit) {
        //         if(r.contains("/bbs/BikerShop")) {
        //             val r2 = "http://www.ptt.cc" + r
        //             println(r2)
        //             resultArray += r2
        //         }
        //     }
        // }
        // resultArray
    }


    def urlToResult(url: String) : ArrayBuffer[String] = {
        getURLforPost(findArrows(parseURLAndSplitPerLine(url)))
    }

}