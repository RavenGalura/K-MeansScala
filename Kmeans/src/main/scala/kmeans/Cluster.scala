package kmeans

class Cluster(private var tabId:Array[Int], private var centroide:Donnee)
{
  override def toString: String = //méthode toString
  {
    var s="id : "
    for (i<-0 until tabId.length)
    {
      s+= tabId(i)+" "
    }
    s+= s"\n centroide: ${centroide.toString}"
    return s
  }

  def getCentroide():Donnee= this.centroide //retourne le centroide

  def getTabId():Array[Int]= this.tabId //retourne le tableau d'identifiant

  def setTabId(tab:Array[Int])= this.tabId=tab //permet de modifier le tableau d'identifiant

  def setCentroide(c:Donnee)=this.centroide=c //permet de modifier le centroide

  def addId(i:Int)= this.tabId=this.tabId :+ i //permet de concatener des identifiants dans le tableau d'identifiant
}
