package ch.usi.si.msde.edsl.assignment_03.model

enum URLScheme:
  case HTTP, HTTPS

case class DomainAndPath(pathElements: List[String]):
  require(pathElements.nonEmpty)

  override lazy val toString = pathElements.mkString("/")
  def append(pathElement: String): DomainAndPath = DomainAndPath(
    pathElements :+ pathElement
  )
end DomainAndPath

case class QueryString(keyValuePairs: List[KeyValuePair]):
  require(keyValuePairs.nonEmpty)
  override lazy val toString = s"?${keyValuePairs.mkString("&")}"

case class KeyValuePair(key: String, value: String):
  override lazy val toString = s"$key=$value"

/** Boxes a Url.
  */
case class URL(
    scheme: URLScheme,
    domainAndPath: DomainAndPath,
    queryString: Option[QueryString] = None
):

  override lazy val toString =
    val schemeRep = scheme.toString().toLowerCase
    val queryStringRep = queryString.map { _.toString }.getOrElse("")
    s"$schemeRep://$domainAndPath$queryStringRep"

end URL
