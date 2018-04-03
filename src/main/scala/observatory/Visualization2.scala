package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Interaction._
import observatory.Visualization.{interpolateColorSorted, predictTemperature}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    assert(point.y >= 0)
    assert(point.y <= 1)
    assert(point.x >= 0)
    assert(point.x <= 1)
    val x = point.x
    val y = point.y
    val fxy1 = (1.0 - x) * d00 + x * d10
    val fxy2 = (1.0 - x) * d01 + x * d11
    (1.0 - y) * fxy1 + y * fxy2
  }

  def getBase(location: Location): (Int, Int) = {
    val x = location.lon.toInt
    val y = location.lat.toInt
    val (ansx, ansy) = (if(location.lon < 0) x - 1 else x, if (location.lat > 0) y + 1 else y)
    assert(ansy > -89)
    assert(ansy <= 90)
    assert(ansx >= -180)
    assert(ansx < 179)
    (ansx, ansy)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val zoom = 8
    val size = 1 << 8
    val sortedColors = colors.toArray.sortBy(_._1)
    val subLocations = getSubLocations(tile, 8)

    val arr: Array[Pixel] = new Array(size * size)

    subLocations
      .par
      .foreach({case (id, location) =>
        val (x, y) = getBase(location)
        val t = bilinearInterpolation(CellPoint(location.lon - x, y - location.lat),
          grid(GridLocation(y, x)), grid(GridLocation(y - 1, x)), grid(GridLocation(y, x + 1)) , grid(GridLocation(y - 1, x + 1)))
        val c = interpolateColorSorted(sortedColors, t)
        arr(id) = Pixel(c.red, c.green, c.blue, 255)
      })
    Image(size, size, arr)
  }

}
