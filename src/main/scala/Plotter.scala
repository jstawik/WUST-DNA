import java.io.File

import com.cibo.evilplot.colors.{Color, ScaledColorBar}
import com.cibo.evilplot.geometry.Extent
import com.cibo.evilplot.plot.Heatmap
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.typesafe.scalalogging.Logger


object Plotter extends {
  val logger :Logger = Logger(s"Plotter")
  def makeHeatMap(data: Seq[Seq[Double]], title: String): Unit = {
    val ts = System.currentTimeMillis().toString
    val path = "generated/"
    val fileName = new File(path+ts+"_"+title+".png")
    Heatmap(data, colorBar=ScaledColorBar(Color.getGradientSeq(data.size, 0, 180).map(_.darken(15)),0.0,5.0))
    // val plot =  Heatmap(data, colorBar=ScaledColorBar(Color.getDefaultPaletteSeq(data.size),0.0,5.0))
      .frame()
      //.xAxis()
      //.yAxis()
      .xGrid()
      .yGrid()
      .rightLegend()
      .title(title, Some(50.0))
      .render(Extent(1000, 1000))
      .write(fileName)
      logger.debug(s"Saved file as: ${fileName.getAbsolutePath}")
  }

}
