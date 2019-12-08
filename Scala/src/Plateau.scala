import scala.collection.mutable._
import scala.io.Source
class LectureDoc{

}

class Tondeuse(var x: Int,var y: Int, var Direction: Char){
  def print(): Unit ={
    println(x+" "+y+" "+Direction)
  }
}

class Plateau(long:Int, larg:Int, tondeuses: ArrayBuffer[Tondeuse]=ArrayBuffer[Tondeuse]()){


  /* prends en entrée la l'orientation et la position de la tondeuse dans le Set*/


  def deplacement(y:Int, orient: Char): Unit={

    var tondeuse: Tondeuse = this.tondeuses.apply(y)
    orient match {
      case 'A' =>
        tondeuse.Direction match {
          case 'N' => if (tondeuse.y < larg) {
            tondeuse.y += 1
          }
          case 'E' => if (tondeuse.x < larg) {
            tondeuse.x += 1
          }
          case 'W' => if (tondeuse.x > 0) {
            tondeuse.x -= 1
          }
          case 'S' => if (tondeuse.y > 0) {
            tondeuse.y -= 1
          }
        }
      case 'D' =>
        tondeuse.Direction match {
          case 'N' => tondeuse.Direction = 'E'
          case 'E' => tondeuse.Direction = 'S'
          case 'W' => tondeuse.Direction = 'N'
          case 'S' => tondeuse.Direction = 'W'
        }
      case 'G' =>
        tondeuse.Direction match {
          case 'N' => tondeuse.Direction = 'W'
          case 'E' => tondeuse.Direction = 'N'
          case 'W' => tondeuse.Direction = 'S'
          case 'S' => tondeuse.Direction = 'E'
        }
    }
  }
  def liste_deplacement(y: Int, commandes:String): Unit ={
    val listcom = commandes.toCharArray()
    for (a<-listcom){
      deplacement(y,a)
    }
  }
  def add_tondeuse(tondeuse: Tondeuse): Unit={
    tondeuses += tondeuse
  }

  def print_plateau():Unit={
    val n = tondeuses.length
    for(i<-0 to n-1){
      printf("Le résultat de la tondeuse %d est: ",i+1)
      tondeuses(i).print()
    }
  }


}



object Lecture{
  def lecture(string: String):String={
    val lect = scala.io.Source.fromFile(string).mkString
    return lect
  }

  def Lines(str: String):Array[String] = {
    val lines = str.split("\r\n|\r|\n")
    return lines
  }

  def deplacement(input: Array[String]):Plateau={
    val long = input(0)(0).toString().toInt
    val larg = input(0)(2).toString().toInt
    val plateau = new Plateau(long,larg)
    for(i<-0 until((input.length-1)/2)) {
      var tondeuse = new Tondeuse(input(2*i+1)(0).toString().toInt, input(2*i+1)(2).toString().toInt, input(2*i+1)(4).toChar)
      plateau.add_tondeuse(tondeuse)
      plateau.liste_deplacement(i,input(2*(i+1)))
    }
    return plateau
  }




}


object main extends App{
    val tab = Lecture.lecture(("src/test.txt"))
    val commandes = Lecture.Lines(tab)
    Lecture.deplacement(commandes).print_plateau()
}
