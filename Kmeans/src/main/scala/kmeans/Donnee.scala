package kmeans
import scala.math.pow
import scala.math.sqrt

class Donnee(private val tabPoint:Array[Double])
{
  override def toString:String = //méthode toString
  {
    var s="["
    for(i<-0 until tabPoint.length)
    {
      s+= tabPoint(i) +","
    }
    s+="]"
    return s
  }


  def dist(d:Donnee):Double= //retourne la distance entre deux points de n dimensions
  {
    var total:Double=0
    for(i<-0 until this.tabPoint.length)
    {
      total+=pow(this.tabPoint(i)-d.tabPoint(i),2)
    }
    return sqrt(total)
  }
  def getCarac(i:Int):Double= tabPoint(i) //renvoie les caracteristiques d'une donnée

  def length:Int= tabPoint.length // renvoie la longueur d'une donnée

}
