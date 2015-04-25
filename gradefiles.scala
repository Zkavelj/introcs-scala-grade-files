//ZvonkoKavelj
import scala.util.{ Try, Success, Failure }
import scala.math.min
import scala.io._
import scala.util.Sorting
import java.io._
import scala.util._

object Homework2_3 extends App {
    println("Enter course name")
    val courseName = Try(args(0)) getOrElse(readLine())
    
    
    val writer = new PrintWriter(new File(courseName + "_solution.txt"))
    
    val source = scala.io.Source.fromFile("categories_" + courseName + ".txt")
    var categoriesarray = source.getLines.toArray
    var categories = categoriesarray(0)
    var assignmentnumber = categoriesarray(2)
    var weights = categoriesarray(1)
    var headerarray:Array[String] = categories.split(", ")
    var assignmentnumberarray = assignmentnumber.split(", ")
    var weightsArray = weights.split(", ")
    var columns = weightsArray.length
    
    
    val source2 = scala.io.Source.fromFile("students_" + courseName + ".txt")
    var studentarray = source2.getLines.toArray
      
   var linenumber = io.Source.fromFile("students_" + courseName + ".txt").getLines.size
    for (i <- 0 until linenumber) {
        var student = studentarray(i)
        var singlearray = student.split(",")
        var firstname = singlearray(2)
        var lastname = singlearray(1)
        var ID = singlearray(0)
        
        val studentgrades = scala.io.Source.fromFile(ID + courseName + ".data")
        val studentgradesarray:Array[String] = studentgrades.getLines.toArray
        val gradenumber = (studentgradesarray.size)
        
   
        var homeworkx = 0
        var classx = 0
        var examsx = 0
        var labsx = 0
        var projectsx = 0
        
        var homeworkarray = new Array[String](50)
        var classarray = new Array[String](50)
        var examsarray = new Array[String](50)
        var projectsarray = new Array[String](50)
        var labsarray = new Array[String](50)
        
        for (i <- 0 until gradenumber){
            if (studentgradesarray(i).startsWith("H")){  
                homeworkarray(homeworkx) = studentgradesarray(i)
                homeworkx = homeworkx +1
                
          }
            
            else if (studentgradesarray(i).startsWith("L")){
                labsarray(labsx) = studentgradesarray(i)
                labsx = labsx + 1
                
            }
            else if (studentgradesarray(i).startsWith("C")){
                classarray(classx) = studentgradesarray(i)
                classx = classx + 1
               
            }
            else if (studentgradesarray(i).startsWith("E")){
                examsarray(examsx) = studentgradesarray(i)
                examsx = examsx + 1
                
            }
            else if (studentgradesarray(i).startsWith("P")){
                projectsarray(projectsx) = studentgradesarray(i)
                projectsx = projectsx + 1
                
            }
           
        }
        
        
        
        var goodhhomeworkarray = homeworkarray.slice(0, homeworkx)
        var goodprojectsarray = projectsarray.slice(0, projectsx)
        var goodlabarray = labsarray.slice(0, labsx)
        var goodexamsarray = examsarray.slice(0, examsx)
        var goodclassarray = classarray.slice(0, classx)
        
        var hw = (goodhhomeworkarray.mkString(", ")).split(", ")
        var p = (goodprojectsarray.mkString(", ")).split(", ")
        var l = (goodlabarray.mkString(", ")).split(", ")
        var e = (goodexamsarray.mkString(", ")).split(", ")
        var c = (goodclassarray.mkString(", ")).split(", ")
        
        var hwlength = hw.length
        var lablength = l.length
        var projectlength = p.length
        var examlength = e.length
        var classlength = c.length
        
        var hwsum = 0
        var labsum = 0
        var projectsum = 0
        var examsum = 0
        var classsum = 0
        
       
        
        for(i <- 2 until hwlength by 3){
            hwsum = hwsum + hw(i).toInt
        }
        for(i <- 2 until lablength by 3){
            labsum = labsum + l(i).toInt
        }
        for(i <- 2 until projectlength by 3){
            projectsum = projectsum + p(i).toInt
        }
       for(i <- 2 until examlength by 3){
           examsum = examsum + e(i).toInt
        }
        for(i <- 2 until classlength by 3){
            classsum = classsum + c(i).toInt
        }
        
        
        
        
      var totalweight = 0 
      
       for (i <- 0 until columns){
          totalweight = totalweight + weightsArray(i).toInt
       } 
     
        
      var hwweight = 0
      var lweight = 0
     var cweight = 0
     var pweight = 0
      var eweight = 0
        
      var hnumber = 0
      var lnumber = 0
      var cnumber = 0
      var pnumber = 0
      var enumber = 0  
        
      for(i <- 0 until columns){
          if(headerarray(i).startsWith("H")){
              hwweight = weightsArray(i).toInt + hwweight
              hnumber = assignmentnumberarray(i).toInt + hnumber
          }
          if(headerarray(i).startsWith("L")){
              lweight = weightsArray(i).toInt + lweight
              lnumber = assignmentnumberarray(i).toInt + lnumber
          }
          if(headerarray(i).startsWith("C")){
              cweight = weightsArray(i).toInt +cweight
              cnumber = assignmentnumberarray(i).toInt + cnumber
          }
          if(headerarray(i).startsWith("P")){
              pweight = weightsArray(i).toInt + pweight
              pnumber = assignmentnumberarray(i).toInt + pnumber
          }
          if(headerarray(i).startsWith("E")){
              eweight = weightsArray(i).toInt + eweight
              enumber = assignmentnumberarray(i).toInt + enumber
          }
      }  
        
       var finalhomework:Double = 0
       var finallab:Double = 0
       var finalproject:Double = 0
       var finalexam:Double = 0
       var finalclass:Double = 0
       if (homeworkx != 0){
            finalhomework = hwsum.toDouble/hnumber.toDouble}
       if (labsx != 0){
            finallab = labsum.toDouble/lnumber.toDouble}
       if (projectsx != 0){
            finalproject = projectsum.toDouble/pnumber.toDouble}
       if (examsx != 0){
            finalexam = examsum.toDouble/enumber.toDouble}
       if (classx != 0){
            finalclass = classsum.toDouble/cnumber.toDouble}
      
          
        
     var fh = (hwweight.toDouble/totalweight.toDouble)*finalhomework.toDouble   
     var fl = (lweight.toDouble/totalweight.toDouble)*finallab.toDouble
     var fc = (cweight.toDouble/totalweight.toDouble)*finalclass.toDouble
     var fp = (pweight.toDouble/totalweight.toDouble)*finalproject.toDouble
     var fe = (eweight.toDouble/totalweight.toDouble)*finalexam.toDouble
        
     var finalgrade = fh+fl+fc+fp+fe   
             
      
        var grade:String = ""
        
  if (finalgrade >= 93.0) {
   grade = finalgrade.toString + " A"         
        }
  else if (finalgrade >= 90.0)
  grade = finalgrade.toString + " A-"
  else if (finalgrade >= 87.0)
  grade = finalgrade.toString + " B+"
  else if (finalgrade >= 83.0)
 grade = finalgrade.toString + " B"
  else if (finalgrade >= 80.0)
 grade = finalgrade.toString + " B-"
  else if (finalgrade >= 77.0)
  grade = finalgrade.toString + " C+"
  else if (finalgrade >= 73.0)
  grade = finalgrade.toString + " C"
  else if (finalgrade >= 70.0)
  grade = finalgrade.toString + " C-"
  else if (finalgrade >= 67.0)
  grade = finalgrade.toString + " D+"
  else if (finalgrade >= 63.0)
  grade = finalgrade.toString + " D"
  else if (finalgrade >= 60.0)
  grade = finalgrade.toString + " D-"
  else if (finalgrade < 60.0)
  grade = finalgrade.toString + " F"
      
        println(lastname + ", " + firstname + ": " + grade) 
        
        writer.write(lastname + ", " + firstname + ": " + grade + "\n")
    }
   
    writer.close()
    
}

 
