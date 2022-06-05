import scala.annotation.tailrec
import scala.io.Source
object Thesaurus {

  val defaultEncoding = "ISO8859-1"

  def load(filename: String): Map[String, Set[String]] = {
    val linesNum = Source.fromFile(filename)(defaultEncoding).getLines().size
    val lines = Source.fromFile(filename)(defaultEncoding).getLines()
    lines.next()

    @tailrec
    def loadFile(ctdown: Int, G: Map[String, Set[String]]): Map[String, Set[String]] = {
      if ctdown < 3 then G
      else {
        val wordArr = lines.next().split('|').map(x => x.toLowerCase())
        def loadGroup(groups: Int): Set[String] =  {
          if groups == 0 then Set()
          else {
            val synGroup = lines.next().split('|').tail.toSet
            synGroup.map(_.trim().toLowerCase()) ++ loadGroup(groups - 1)
          }          
        }

        val groupLst = loadGroup(wordArr(1).toInt)
        loadFile(ctdown - 1 - wordArr(1).toInt, G + (wordArr(0) -> groupLst))
      }
    }

    loadFile(linesNum - 1, Map())
  }

  def linkage(thesaurusFile: String): String => String => Option[List[String]] = ???
}
