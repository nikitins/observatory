package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {
  def getNextSubTiles(tile: Tile): List[Tile] = {
    val x = tile.x
    val y = tile.y
    val z = tile.zoom
    List(Tile(x + x, y + y, z + 1), Tile(x + x + 1, y + y, z + 1), Tile(x + x, y + y + 1, z + 1), Tile(x + x + 1, y + y + 1, z + 1))
  }

  def allTiles(tile: Tile, zoom: Int): List[Tile] = {
    if (zoom == 0)
      Nil
    else {
      val ans = for (t <- getNextSubTiles(tile)) yield allTiles(t, zoom - 1)
      tile :: ans.flatten
    }
  }

  def getSubLocations(tile: Tile, zoom: Int): List[(Int, Location)] = {
    def fun(t: Tile, z: Int, zeroTile: Tile): List[(Int, Location)] = {
      if(z == 0)
        List((zeroTile.y * (1 << zeroTile.zoom) + zeroTile.x, tileLocation(t)))
      else {
        val ans = for((subTile, subZeroTile) <- getNextSubTiles(t).zip(getNextSubTiles(zeroTile)))
          yield fun(subTile, z - 1, subZeroTile)
        ans.flatten
      }
    }

    fun(tile, zoom, Tile(0, 0, 0))
  }

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(
      toDegrees(atan(sinh(PI * (1.0 - 2.0 * tile.y.toDouble / (1 << tile.zoom))))),
      tile.x.toDouble / (1 << tile.zoom) * 360.0 - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val size = 8
    val locations = getSubLocations(tile, size)
    Visualization.visualizeCustomSize(temperatures, colors, 1 << size, 1 << size, locations)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    val tiles = allTiles(Tile(0, 0, 0), 4)

    for {(year, data) <- yearlyData.par
         tile <- tiles} {
      generateImage(year, tile, data)
    }
  }

}
