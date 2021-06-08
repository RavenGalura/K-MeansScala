package kmeans
import scala.io.Source
import scala.math.pow
import scala.math.sqrt
import scala.util.Random.nextInt
class KMeans()
{
  private var matriceDonnee:MatriceDonnee= _ //matrice sur lequel on va appliquer le Kmeans

  override def toString: String = matriceDonnee.toString +"\n" + matriceDonnee.length +"\n" +matriceDonnee.getDonnees(0).length //méthode toString

  def lireDonnees(file: String): Unit = //permet de lire un fichier
  {
    var m:Array[Donnee]=Array()
    for (line <- Source.fromResource(file).getLines())
    {
      if (line != "")
      {
        val sep = line.split(",")
        m = m :+ new Donnee(sep.dropRight(1).map(_.toDouble))
      }
    }
    this.matriceDonnee =new MatriceDonnee(m)
  }


  def moyenne(mat:MatriceDonnee):Donnee= //retourne la moyenne d'une matrice
  {
    var r:Array[Double]=Array()
    for (j<-0 until mat.getDonnees(0).length)
    {
      var somme:Double=0
      for(i<-0 until mat.length)
      {
        somme+= mat.getDonnees(i).getCarac(j)
      }
      r= r:+ somme/mat.length
    }
    return new Donnee(r)
  }
  def variance(mat:MatriceDonnee):Donnee= //retourne la variance d'une matrice
  {
    var r:Array[Double]=Array()
    var moy:Donnee=this.moyenne(mat)
    for(j<-0 until mat.getDonnees(0).length)
    {
      var somme:Double=0
      for(i<-0 until mat.length)
      {
        somme+= pow(mat.getDonnees(i).getCarac(j)-moy.getCarac(j),2)
      }
      r= r:+somme/mat.length
    }
    return new Donnee(r)
  }
  def ecartType(mat:MatriceDonnee):Donnee= //retourne l'écart type d'une matrice
  {
    var r:Array[Double]=Array()
    var variance:Donnee=this.variance(mat)
    for(i<-0 until variance.length)
    {
      r= r:+ sqrt(variance.getCarac(i))
    }
    return new Donnee(r)
  }

  def matCluster(tabId:Array[Int]):MatriceDonnee= //retourne les lignes de la matrice associé au tableau d'identifiant
  {
    var tab:Array[Donnee]=Array()
    for (i<-0 until tabId.length)
    {
      tab=tab:+ matriceDonnee.getDonnees(tabId(i))
    }
    return new MatriceDonnee(tab)
  }


  def tabMin(tab:Array[Double]):Int= //retourne la position du minimum dans un tableau
  {
    var c:Double=tab(0)
    var pos:Int=0
    for (i<-1 until tab.length)
    {
      if (c>tab(i))
      {
        c=tab(i)
        pos=i
      }
    }
    return pos
  }


  def runKmeans(k:Int):Unit= //algorithme du kmeans
  {
    var tabCluster:Array[Cluster]=Array() //initialise un tableau de cluster
    for (i<-0 until k)
    {
      tabCluster= tabCluster :+ new Cluster(Array(),matriceDonnee.getDonnees(nextInt(matriceDonnee.length - 1)))
    }
    for (n<-0 until 50) //boucle pour le kmeans
    {
      for (m<-0 until tabCluster.length) //initialise les clusters à vide
      {
        tabCluster(m).setTabId(Array())
      }
      for (i<-0 until matriceDonnee.length) //parcours des lignes de la matrice
      {
        var dist:Array[Double]=Array()
        for (j<-0 until tabCluster.length) //parcours des colonnes de la matrice
        {
          dist=dist:+ tabCluster(j).getCentroide().dist(matriceDonnee.getDonnees(i)) //rajoute les distances entre chaque donnee dans un tableau
        }
        tabCluster(tabMin(dist)).addId(i) //rajoute la donnée dans le cluster correspondant
      }
      for (l<-0 until tabCluster.length) //actualise les centroides
      {
        tabCluster(l).setCentroide(moyenne(matCluster(tabCluster(l).getTabId())))
      }
    }
    for (o<-0 until tabCluster.length) //affichage des clusters
    {
      println(s"Cluster ${o+1} :")
      println(tabCluster(o))
    }

  }

}
