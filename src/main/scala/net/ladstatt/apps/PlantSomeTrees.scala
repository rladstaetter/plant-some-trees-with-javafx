package net.ladstatt.apps

import scala.collection.JavaConversions.seqAsJavaList
import scala.math.Pi
import scala.math.cos
import scala.math.sin
import scala.util.Random

import javafx.application.Application
import javafx.event.EventHandler
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.effect.Glow
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.scene.paint.CycleMethod
import javafx.scene.paint.LinearGradient
import javafx.scene.paint.Paint
import javafx.scene.paint.Stop
import javafx.scene.shape.Line
import javafx.scene.shape.Rectangle
import javafx.stage.Stage

object PlantSomeTrees {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[PlantSomeTrees], args: _*)
  }

}

class PlantSomeTrees extends javafx.application.Application {

  val canvasWidth = 800
  val canvasHeight = 600
  val treeDepth = 6
  val (minTreeSize, maxTreeSize) = (10, 35)
  val treeColor = Color.CHOCOLATE
  val initialDirection = 3 * Pi / 2
  val (minDegree, maxDegree) = (Pi / 16, Pi / 4)

  override def start(stage: Stage): Unit = {
    stage.setTitle("A forrest")
    val root = new Group()
    val background = new Rectangle(0, 0, canvasWidth, canvasHeight)
    val stops = List(new Stop(0, Color.BLACK), new Stop(1, Color.WHITESMOKE))
    val g = new LinearGradient(0.0, 1.0, 0.0, 0.0, true, CycleMethod.NO_CYCLE, stops)
    background.setFill(g)
    root.getChildren.add(background)

    root.addEventHandler(MouseEvent.MOUSE_CLICKED, new EventHandler[MouseEvent] {
      def handle(event: MouseEvent) {
        root.getChildren().addAll(mkTree(event.getX, event.getY, minTreeSize + (maxTreeSize - minTreeSize) * Random.nextDouble, initialDirection, treeDepth, treeColor))
      }
    })

    stage.setScene(new Scene(root, canvasWidth, canvasHeight))
    stage.show()
  }

  def mkRand = {
    val r = Random.nextDouble
    if (r < 0.5) r + 0.3 else r
  }

  def mkRandDegree = (maxDegree - minDegree) * Random.nextDouble

  def mkTree(x: Double, y: Double, length: Double, degree: Double, ord: Int, color: Color): Group = {

    def mkTree0(x00: Double, y00: Double, length: Double, d0: Double, ord: Int, color: Color): List[Line] = {
      ord match {
        case 0 => Nil
        case _ => {

          val (dc0, ds0) = (cos(d0), sin(d0))
          val (x01, y01) = (x00 + length * dc0, y00 + length * ds0) // endpoint first stroke

          val l0 = length * 0.7 * (1 + Random.nextDouble)
          val l1 = length * 0.7 * (1 + Random.nextDouble)
          val l2 = length * 0.7 * (1 + Random.nextDouble)

          val (x10, y10) = (x00 + l0 * dc0, y00 + l0 * ds0) // startpoint second stroke

          val d1 = d0 + mkRandDegree
          val (dc1, ds1) = (cos(d1), sin(d1))
          val (x11, y11) = (x10 + l1 * dc1, y10 + l1 * ds1) // endpoint second stroke

          val d2 = d0 - mkRandDegree

          val (dc2, ds2) = (cos(d2), sin(d2))
          val (x12, y12) = (x10 + l2 * dc2, y10 + l2 * ds2) // endpoint third stroke

          List(mkLine(x00, y00, x01, y01, ord, color),
            mkLine(x10, y10, x11, y11, ord, color),
            mkLine(x10, y10, x12, y12, ord, color)) ++
            mkTree0(x01, y01, l0 * 0.6 * (1 + Random.nextDouble), d0, ord - 1, color.darker) ++
            mkTree0(x11, y11, l1 * 0.5 * (1 + Random.nextDouble), d1, ord - 1, color.darker) ++
            mkTree0(x12, y12, l2 * 0.5 * (1 + Random.nextDouble), d2, ord - 1, color.darker)
        }
      }
    }

    val g = new Group
    g.getChildren().addAll(mkTree0(x, y, length, degree, ord, color))
    g.addEventHandler(MouseEvent.MOUSE_ENTERED, new EventHandler[MouseEvent] {
      def handle(event: MouseEvent) {
        g.setEffect(new Glow(0.8))
      }
    })
    g.addEventHandler(MouseEvent.MOUSE_EXITED, new EventHandler[MouseEvent] {
      def handle(event: MouseEvent) {
        g.setEffect(new Glow(0))
      }
    })
    g
  }

  def mkLine(x1: Double, y1: Double, x2: Double, y2: Double, width: Int, paint: Paint) = {
    val l = new Line(x1, y1, x2, y2)
    l.setStrokeWidth(width)
    l.setStroke(paint)
    l
  }

}

