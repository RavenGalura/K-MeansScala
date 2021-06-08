import kmeans.{Donnee, KMeans, MatriceDonnee}

object Main extends App
{
  val k= new KMeans()
  k.lireDonnees("iris.data")
  k.runKmeans(3)

}
