import java.io.File

import com.cibo.evilplot.colors.{Color, RGB, RGBA, ScaledColorBar}
import com.cibo.evilplot.geometry.Extent
import com.cibo.evilplot.numeric.Point
import com.cibo.evilplot.plot.{Heatmap, LinePlot, ScatterPlot}
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.renderers.PointRenderer
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
  def makeTrajectory(data: Seq[Evaluation], title: String): Unit = {
    val ts = System.currentTimeMillis().toString
    val path = "generated/"
    val fileName = new File(path+ts+"_"+title+".png")
    val xs: Range = data.indices
    var actual_points: Seq[Point] = Seq.empty
    var max_points: Seq[Point] = Seq.empty
    var min_points: Seq[Point] = Seq.empty
    var avg_points: Seq[Point] = Seq.empty
    for(x <- xs){
     actual_points = actual_points appended Point(x, data(x).actual)
     max_points = max_points appended Point(x, data(x).max)
     min_points = min_points appended Point(x, data(x).min)
     avg_points = avg_points appended Point(x, data(x).avg)
    }
    LinePlot.series(actual_points, "Actual", RGBA(20, 20, 200, 0.5))
      .overlay(LinePlot.series(max_points, "Max", RGBA(200, 20, 20, 0.5)))
      .overlay(LinePlot.series(min_points, "Min", RGBA(20, 200, 20, 0.5)))
      .overlay(LinePlot.series(avg_points, "Avg", RGBA(200, 200, 20, 0.5)))
      .frame()
      .xGrid()
      .yGrid()
      .xAxis()
      .xLabel("Steps")
      .yLabel("Value")
      .yAxis()
      .rightLegend()
      .title(title, Some(50.0))
      .render(Extent(1000, 1000))
      .write(fileName)
    logger.debug(s"Saved file as: ${fileName.getAbsolutePath}")
  }

}
