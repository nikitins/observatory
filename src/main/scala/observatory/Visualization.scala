package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val power: Double = 6
  val R = 6371.0

  def equal(x: Double, y: Double): Boolean = abs(x - y) <= 1e-9

  def distance(l1: Location, l2: Location): Double = {
    val getSig: Double = {
      if (equal(l1.lat, l2.lat) && equal(l1.lon, l2.lon))
        0
      else if (equal(l1.lat, -l2.lat) && equal(abs(l1.lon - l2.lon), 180))
        PI
      else {
        val f1 = l1.lat * PI / 180
        val f2 = l2.lat * PI / 180
        val a1 = l1.lon * PI / 180
        val a2 = l2.lon * PI / 180
        val da = abs(a1 - a2)
        acos(sin(f1) * sin(f2) + cos(f1) * cos(f2) * cos(da))
      }
    }

    val sigma = getSig
    val ans = sigma * R
    if(ans < 1.0) 0 else ans
  }

  def wi(x: Location, xi: Location): Double = 1.0 / pow(distance(x, xi), power)
  def wi(d: Double): Double = 1.0 / pow(d, power)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val temps = temperatures.map(x => (x._1, x._2, distance(x._1, location)))
    for ((_, t, d) <- temps) {
      if (d < 1.0)
        return t
    }

    var up = 0.0
    var down = 0.0

    for ((_, t, d) <- temps) {
      val w = wi(d)
      up += w * t
      down += w
    }

    up / down
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val arr = points.toArray.sortBy(_._1)
    interpolateColorSorted(arr, value)
  }

  def interpolateColorSorted(points: Seq[(Temperature, Color)], value: Temperature): Color = {
    if (points.head._1 >= value)
      points.head._2
    else if (points.last._1 <= value)
      points.last._2
    else {
      def interpolate(x: Double, x0: Double, y0: Double, x1: Double, y1: Double) = (y0 * (x1 - value) + y1 * (value - x0) ) / (x1 - x0)

      val id = points.lastIndexWhere(p => p._1 < value)

      if (id  == -1) {
        throw new IllegalArgumentException(s"$points value $value")
      }

      val x0 = points(id)._1
      val y0 = points(id)._2
      val x1 = points(id + 1)._1
      val y1 = points(id + 1)._2

      val r = interpolate(value, x0, y0.red, x1, y1.red)
      val g = interpolate(value, x0, y0.green, x1, y1.green)
      val b = interpolate(value, x0, y0.blue, x1, y1.blue)

      Color(r.round.toInt, g.round.toInt, b.round.toInt)
    }
  }

  def visualizeCustomSize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)],
                          h: Int, w: Int, locations: List[(Int, Location)]): Image = {
    val sortedColors = colors.toArray.sortBy(_._1)
    val arr: Array[Pixel] = new Array(h * w)

    locations
      .par
      .foreach({case (id, location) =>
              val t = predictTemperature(temperatures, location)
              val c = interpolateColorSorted(sortedColors, t)
              arr(id) = Pixel(c.red, c.green, c.blue, 127)
    })
    Image(w, h, arr)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val h = 180
    val w = 360
    val h2 = h/2
    val w2 = w/2

    def fieldToLocation(id: Int): Location = {
      val x = id / w
      val y = id % w
      Location(h / 2 - x, -(w / 2) + y)
    }

    val sortedColors = colors.toArray.sortBy(_._1)
    val arr: Array[Pixel] = new Array(h * w)

    arr
      .indices
      .par
      .foreach(id => {
              val t = predictTemperature(temperatures, fieldToLocation(id))
              val c = interpolateColorSorted(sortedColors, t)
              arr(id) = Pixel(c.red, c.green, c.blue, 255)
    })
    Image(w, h, arr)
  }
}

