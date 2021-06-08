package kmeans

class MatriceDonnee(private val donnes:Array[Donnee])
{
  override def toString: String = //méthode toString
  {
    var s:String=""
    for(i<-0 until donnes.length)
    {
      s+= donnes(i).toString+"\n"
    }
    return s
  }
  def getDonnees(i:Int):Donnee= this.donnes(i) //retourne une donnée de la matrice

  def length:Int= donnes.length //retourne la taille de la matrice
}
