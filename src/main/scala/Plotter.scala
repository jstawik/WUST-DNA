import java.io.File

import BirdOperator.Pipe
import com.cibo.evilplot.colors.{Color, RGBA, ScaledColorBar}
import com.cibo.evilplot.geometry.Extent
import com.cibo.evilplot.numeric.Point
import com.cibo.evilplot.plot.aesthetics.DefaultTheme._
import com.cibo.evilplot.plot.{Heatmap, LinePlot}
import com.typesafe.scalalogging.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future}

object Plotter {
  val logger: Logger = Logger(s"Plotter")
  //implicit val executionContext: ExecutionContext = Simulator.system.dispatchers.lookup("plotter-dispatcher")
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
  def makeTrajectory(data: Seq[Evaluation], title: String, key: Double => Double): Future[Unit] = Future {
    val path = "generated/"
    val fileName = new File(path+ts()+"_"+title+".png")
    var actualPoints: Seq[Point] = Seq.empty
    var maxPoints: Seq[Point] = Seq.empty
    var minPoints: Seq[Point] = Seq.empty
    var avgPoints: Seq[Point] = Seq.empty
    val timeout = Duration(60, SECONDS)
    for(x <- data.indices){
      //actualPoints = actualPoints appended Point(data(x).frame, key(data(x).actuals.map(a => Await.result(a, timeout)).sum))
      actualPoints = actualPoints appended Point(data(x).frame, data(x).actuals.map(a => Await.result(a, timeout)).sum|>key)
      maxPoints = maxPoints appended Point(data(x).frame, data(x).results.map(a => Await.result(a, timeout)).max)
      minPoints = minPoints appended Point(data(x).frame, data(x).results.map(a => Await.result(a, timeout)).min)
      avgPoints = avgPoints appended Point(data(x).frame, data(x).results.map(a => Await.result(a, timeout)).sum/data(x).results.size)
    }
    LinePlot.series(actualPoints, "Actual Value", RGBA(20, 20, 200, 0.5))
      .overlay(LinePlot.series(maxPoints, "Maximum Result", RGBA(200, 20, 20, 0.5)))
      .overlay(LinePlot.series(minPoints, "Minimum Result", RGBA(20, 200, 20, 0.5)))
      .overlay(LinePlot.series(avgPoints, "Average Result", RGBA(200, 200, 20, 0.5)))
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
