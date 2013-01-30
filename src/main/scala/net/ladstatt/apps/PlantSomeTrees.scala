package net.ladstatt.apps

import java.io.File
import java.io.PrintWriter
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList
import scala.math.Pi
import scala.math.cos
import scala.math.sin
import scala.util.Random
import javafx.animation.Animation
import javafx.animation.KeyFrame
import javafx.animation.Timeline
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.effect.Glow
import javafx.scene.input.ClipboardContent
import javafx.scene.input.DragEvent
import javafx.scene.input.MouseEvent
import javafx.scene.input.TransferMode
import javafx.scene.paint.Color
import javafx.scene.paint.CycleMethod
import javafx.scene.paint.LinearGradient
import javafx.scene.paint.Paint
import javafx.scene.paint.Stop
import javafx.scene.shape.Line
import javafx.scene.shape.Rectangle
import javafx.scene.shape.Shape
import javafx.stage.Stage
import javafx.util.Duration
import javafx.scene.effect.GaussianBlur

/**
 * See video and some comments on http://ladstatt.blogspot.com/
 */
object PlantSomeTrees {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[PlantSomeTrees], args: _*)
  }

}

class PlantSomeTrees extends javafx.application.Application with LineUtils {

  val canvasWidth = 800
  val canvasHeight = 600
  val treeDepth = 5
  val trunkWidth = 5

  val curDetail = 2
  val displace = 10

  val (minTreeSize, maxTreeSize) = (50, 150)
  def treeColor = mkRandColor
  val (minDegree, maxDegree) = (0, Pi / 4)
  val growingSpeed = 96
  val branchSlices = 10

  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] { def handle(e: E) = f(e) }

  def write2File(file: File, s: String) = {
    val out = new PrintWriter(file, "UTF-8")
    try { out.print(s) } finally { out.close }
  }

  def mkSvg(source: Group): String = {
    val treeAsSvg = (for (s <- source.getChildren()) yield s match {
      case line: Line => {
        val rgb = line.getStroke match {
          case c: Color => "rgb(%s,%s,%s)".format((c.getRed() * 255).toInt, (c.getGreen * 255).toInt, (c.getBlue * 255).toInt)
          case _ => sys.error("unsupported")
        }

        """ <line x1="%s" y1="%s" x2="%s" y2="%s" style="stroke:%s;stroke-width:%s" />"""
          .format(line.getStartX.toInt, line.getStartY.toInt, line.getEndX().toInt, line.getEndY.toInt, rgb, line.getStrokeWidth.toInt)
      }
      case x => sys.error("unsupported: %s".format(x.getClass))
    }).toList.mkString(System.getProperty("line.separator"))

    """<!DOCTYPE html>
<html>
<body>

<svg xmlns="http://www.w3.org/2000/svg" version="1.1">
%s
</svg>

</body>
</html>
""".format(treeAsSvg)
  }

  def initDragNDrop(tree: Group) = {
    tree.setOnDragDetected(mkEventHandler((event: MouseEvent) => {
      val db = tree.startDragAndDrop(TransferMode.ANY: _*)
      val content = new ClipboardContent()
      val tmpFile = File.createTempFile("Tree", ".html")
      write2File(tmpFile, mkSvg(tree))
      content.putHtml(mkSvg(tree))
      content.putFiles(Vector(tmpFile))
      db.setContent(content)
      event.consume()
    }))

    tree.setOnDragDone(mkEventHandler((event: DragEvent) => {
      event.consume()
    }))
  }

  override def start(stage: Stage): Unit = {
    stage.setTitle("A forest with midpoint replacement")

    val drawingArea = new Group()
    val background = {
      val b = new Rectangle(0, 0, canvasWidth, canvasHeight)
      val stops = List(new Stop(0, Color.BLACK), new Stop(1, Color.WHITESMOKE))
      val g = new LinearGradient(0.0, 1.0, 0.0, 0.0, true, CycleMethod.NO_CYCLE, stops)
      b.setFill(g)
      b
    }
    drawingArea.getChildren.add(background)

    drawingArea.addEventHandler(MouseEvent.MOUSE_CLICKED, new EventHandler[MouseEvent] {
      def handle(event: MouseEvent) {
        val start = Vec(event.getX, event.getY)
        val dest = start + Vec(0, ((maxTreeSize - minTreeSize)))
        val broot = Branch(dest, start, treeColor, trunkWidth, treeDepth)
        var lines2Paint = traverse(mkRandomTree(broot)).toList
        val tree = new Group()

        initDragNDrop(tree)

        tree.addEventHandler(MouseEvent.MOUSE_ENTERED, new EventHandler[MouseEvent] {
          def handle(event: MouseEvent) {
            tree.setEffect(new Glow(1.0))
          }
        })
        tree.addEventHandler(MouseEvent.MOUSE_EXITED, new EventHandler[MouseEvent] {
          def handle(event: MouseEvent) {
            tree.setEffect(new Glow(0))
          }
        })

        drawingArea.getChildren.add(tree)
        val growTimeline = new Timeline
        growTimeline.setRate(growingSpeed)
        growTimeline.setCycleCount(Animation.INDEFINITE)
        growTimeline.getKeyFrames().add(
          new KeyFrame(Duration.seconds(1),
            new EventHandler[ActionEvent]() {
              def handle(event: ActionEvent) {
                if (!lines2Paint.isEmpty) {
                  val (hd :: tail) = lines2Paint
                  tree.getChildren.add(hd)
                  lines2Paint = tail
                } else {
                  growTimeline.stop
                }
              }
            }))
        growTimeline.play()

      }
    })

    stage.setScene(new Scene(drawingArea, canvasWidth, canvasHeight))
    stage.show()
  }

  def mkRandDegree = (maxDegree - minDegree) * Random.nextDouble
  def mkRandColor = {
    def randInt = (Random.nextFloat * 255).toInt
    Color.rgb(randInt, randInt, randInt)
  }

  sealed trait ATree
  case class Branch(source: Vec, dest: Vec, color: Color, width: Int, ord: Int) extends ATree
  case class SubTree(center: ATree, left: ATree, right: ATree) extends ATree

  def mkRandomTree(root: Branch): ATree = {

    def mkRandTree(tree: ATree): ATree =
      tree match {
        case Branch(start, dest, color, width, ord) => {
          ord match {
            case 0 => tree
            case _ => {
              val dir = (start - dest).onedir
              val length = (start - dest).length

              val l1 = length * (1 - Random.nextDouble * 0.7)
              val l2 = length * (1 - Random.nextDouble * 0.7)

              // startpoint of left and right branch
              val midRoot = start + dir * length

              mkRandTree(SubTree(
                Branch(start, start + (dir * length), color, width, ord - 1), // trunk
                Branch(midRoot, midRoot + (dir.spin(mkRandDegree) * l1), color.darker, width - 1, ord - 1), // leftbranch
                Branch(midRoot, midRoot + (dir.spin(-mkRandDegree) * l2), color.darker, width - 1, ord - 1))) // rightbranch
            }
          }
        }
        case SubTree(center, left, right) => SubTree(mkRandTree(center), mkRandTree(left), mkRandTree(right))

      }

    mkRandTree(root)
  }

  def traverse(tree: ATree): List[Shape] = {
    tree match {
      case Branch(start, dest, c, width, ord) => {
        mkMidPointReplacement(start, dest, displace, curDetail).map {
          case (start, dest) => mkLine(start, dest, if (width < 1) 1 else width, c)
        }
        //        List(mkLine(start, dest, if (width < 1) 1 else width, c))
      }
      case SubTree(center, left, right) => traverse(center) ++ traverse(left) ++ traverse(right)
    }
  }

}

trait LineUtils {

  def mkMidPointReplacement(source: Vec,
    dest: Vec, displace: Double, curDetail: Double): List[(Vec, Vec)] = {
    if (displace < curDetail) {
      List((source, dest))
    } else {
      val displacedCenter = source.center(dest).displace(displace)
      mkMidPointReplacement(source, displacedCenter, displace / 2, curDetail) ++
        mkMidPointReplacement(displacedCenter, dest, displace / 2, curDetail)
    }
  }

  case class Vec(x: Double, y: Double) {
    def -(that: Vec) = Vec(that.x - x, that.y - y)
    def +(that: Vec) = Vec(x + that.x, y + that.y)
    def *(factor: Double) = Vec(factor * x, factor * y)
    def /(l: Double) = if (l != 0) Vec(x / l, y / l) else sys.error("div.0")
    def length = scala.math.sqrt(x * x + y * y)
    def displace(f: Double) =
      Vec(x + (Random.nextDouble - 0.5) * f, y + (Random.nextDouble - 0.5) * f)
    def onedir = this / length
    def normal = Vec(-y, x)
    def center(other: Vec) = Vec((other.x + x) / 2, (other.y + y) / 2)
    def spin(phi: Double) =
      Vec((x * cos(phi)) - (y * sin(phi)), (x * sin(phi)) + (y * cos(phi)))
  }

  def mkLine(source: Vec, dest: Vec, width: Double, color: Color): Line = {
    val line = new Line(source.x, source.y, dest.x, dest.y)
    line.setStroke(color)
    line.setStrokeWidth(width)
    line
  }

}