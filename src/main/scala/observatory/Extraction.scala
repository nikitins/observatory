package observatory

import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
object Extraction {
  def toDouble(s: String): Double = {
    val x = if (s.startsWith("+")) s.substring(1) else s
    x.toDouble
  }

  def toCelsius(f: Double): Double = (f.toDouble - 32.0) / 1.8

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = scala.io.Source.fromInputStream(getClass.getResourceAsStream(stationsFile))
      .getLines()
      .map(_.split(","))
      .filter(x => x.length == 4 && !(x(2).isEmpty || x(3).isEmpty))
      .map(x => ((x(0), x(1)), (x(2), x(3))))
      .toMap

    val temps = scala.io.Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile))
      .getLines()
      .map(_.split(","))
      .filter(x => x.length == 5 && !(x(2).isEmpty || x(3).isEmpty || x(4).isEmpty))
      .map(x => ((x(0), x(1)), (x(2), x(3), x(4))))

    temps
      .filter(x => stations.contains(x._1))
      .map({case (id, (month, day, temperature)) => (LocalDate.of(year, month.toInt, day.toInt),
                                                     Location(toDouble(stations(id)._1), toDouble(stations(id)._2)),
                                                     toCelsius(temperature.toDouble))})
      .toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val ans = records
      .par
      .map({case (_, location, temperature) => (location,  temperature)})
      .groupBy(_._1)
      .map(f => (f._1, f._2.map(_._2).sum / f._2.size)).toList
    ans
  }

}
