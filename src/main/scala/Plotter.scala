import java.io.File

import com.cibo.evilplot.colors.{Color, RGBA, ScaledColorBar}
import com.cibo.evilplot.geometry.Extent
import com.cibo.evilplot.numeric.Point
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.{Heatmap, LinePlot}
import com.typesafe.scalalogging.Logger

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Plotter {
  val logger: Logger = Logger(s"Plotter")
  val ts: () => String = () =>  System.currentTimeMillis().toString
  def makeHeatMap(frame: Int, data: Seq[Seq[Double]], title: String): Unit = {
    val path = "generated/"
    val fileName = new File(path+frame+"_"+ts()+"_"+title+".png")
    Heatmap(data, colorBar=ScaledColorBar(Color.getGradientSeq(60.min(data.size), 0, 240).map(_.darken(15)),0.0,5.0))  // .getAnalogousSeq(depth = 60.min(data.size)
    // val plot =  Heatmap(data, colorBar=ScaledColorBar(Color.getDefaultPaletteSeq(data.size),0.0,5.0))
      .frame()
      //.xAxis()
      //.yAxis()
      //.xGrid()
      //.yGrid()
      .rightLegend()
      .title(title+s" step $frame", Some(50.0))
      .render(Extent(1000, 1000))
      .write(fileName)
    logger.debug(s"Saved file as: ${fileName.getAbsolutePath}")
  }
  def makeTrajectory[T](rawData: Seq[Evaluation[T]], title: String, key: Acc[T] => Double): Future[Unit] = Future {
    val path = "generated/"
    val sortedData = rawData.sortWith(_.frame < _.frame)
    val fileName = new File(path+ts()+"_"+title+".png")
    var actual_points: Seq[Point] = Seq.empty
    var max_points: Seq[Point] = Seq.empty
    var min_points: Seq[Point] = Seq.empty
    var avg_points: Seq[Point] = Seq.empty
    for(x <- sortedData.indices){
     actual_points = actual_points appended Point(x, key(sortedData(x).actual))
     max_points = max_points appended Point(x, sortedData(x).max)
     min_points = min_points appended Point(x, sortedData(x).min)
     avg_points = avg_points appended Point(x, sortedData(x).avg)
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
