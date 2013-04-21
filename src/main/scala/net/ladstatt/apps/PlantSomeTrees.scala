package net.ladstatt.apps

import scala.collection.JavaConversions.seqAsJavaList
import scala.math.Pi
import scala.math.cos
import scala.math.sin
import scala.util.Random
import scala.util.Random.nextBoolean
import scala.util.Random.nextDouble

import javafx.animation.Animation
import javafx.animation.KeyFrame
import javafx.animation.Timeline
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.event.Event
import javafx.event.EventHandler
import javafx.geometry.Point3D
import javafx.scene.Group
import javafx.scene.Node
import javafx.scene.PerspectiveCamera
import javafx.scene.PointLight
import javafx.scene.Scene
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.scene.paint.CycleMethod
import javafx.scene.paint.LinearGradient
import javafx.scene.paint.Material
import javafx.scene.paint.PhongMaterial
import javafx.scene.paint.Stop
import javafx.scene.shape.Cylinder
import javafx.scene.shape.Shape3D
import javafx.scene.shape.Sphere
import javafx.stage.Stage
import javafx.util.Duration

/**
 * starting with scala stuff, defintion of implicits such that i can continue to use
 * a nicer syntax for calculating the 3d geometry
 */
object Types {

  implicit def toSPoint3D(point3D: Point3D): SPoint3D = {
    SPoint3D(point3D.getX, point3D.getY, point3D.getZ)
  }
  implicit def toPoint3D(spoint3D: SPoint3D): Point3D = spoint3D.toPoint3D
}

case class SPoint3D(x: Double, y: Double, z: Double) {

  def -(that: SPoint3D) = SPoint3D(that.x - x, that.y - y, that.z - z)
  def +(that: SPoint3D) = SPoint3D(x + that.x, y + that.y, that.z + z)
  def *(factor: Double) = SPoint3D(factor * x, factor * y, factor * z)
  def /(l: Double) = if (l != 0) SPoint3D(x / l, y / l, z / l) else sys.error("div.0")
  def length = scala.math.sqrt(x * x + y * y + z * z)
  def displace(f: Double) =
    SPoint3D(x + (Random.nextDouble - 0.5) * f, y + (Random.nextDouble - 0.5) * f, +z + (Random.nextDouble - 0.5) * f)

  def onedir = this / length
  def normal: SPoint3D = SPoint3D(-y, x, z)
  def midpoint(other: SPoint3D) = SPoint3D((other.x + x) / 2, (other.y + y) / 2, (other.z + z) / 2)
  def spin(phi: Double): SPoint3D =
    SPoint3D((x * cos(phi)) - (y * sin(phi)), (x * sin(phi)) + (y * cos(phi)), z
      + (if (nextBoolean) nextDouble else (-nextDouble)))

  def degrees: (Double, Double) = (if (x != 0) scala.math.atan(y / x) * 180 / scala.math.Pi else 90,
    if (x != 0) scala.math.atan(z / x) * 180 / scala.math.Pi else 90)

  def toPoint3D = new Point3D(x, y, z)
  override def toString: String = s"[$x/$y/$z]"
}

trait ShapeUtils {
  import Types._
  def mkMidPointReplacement(source: SPoint3D,
    dest: SPoint3D, displace: Double, curDetail: Double): List[(SPoint3D, SPoint3D)] = {
    if (displace < curDetail) {
      List((source, dest))
    } else {
      val displacedCenter = source.midpoint(dest).displace(displace)
      mkMidPointReplacement(source, displacedCenter, displace / 2, curDetail) ++
        mkMidPointReplacement(displacedCenter, dest, displace / 2, curDetail)
    }
  }

  def mkCylinder(source: SPoint3D, dest: SPoint3D, radius: Double, material: Material): Cylinder = {
    val rotation = (dest - source)
    val height = (dest - source).length
    val c = new Cylinder(radius, height)
    c.setTranslateX(source.x)
    c.setTranslateY(source.y)
    c.setTranslateZ(source.z)
    c.setRotationAxis(rotation)
    c.setRotate(rotation.degrees._1)
    c.setMaterial(material)
    c
  }

  def mkSphere(p: SPoint3D, radius: Double, material: Material): Sphere = {

    val c = new Sphere()
    c.setTranslateX(p.x)
    c.setTranslateY(p.y)
    c.setTranslateZ(p.z)
    c.setRadius(radius)
    c.setMaterial(material)
    c
  }

}

trait JfxUtils {
  def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] { def handle(e: E) = f(e) }
}

trait Spinner extends JfxUtils {

  var anchorX: Double = _
  var anchorY: Double = _
  var anchorAngle: Double = _

  def initSpinner(scene: Scene, drawingArea: Node): Unit = {
    scene.setOnMousePressed(mkEventHandler(event => {
      anchorX = event.getSceneX()
      anchorAngle = drawingArea.getRotate()
    }))

    scene.setOnMouseDragged(mkEventHandler(event => {
      drawingArea.setRotate(anchorAngle + anchorX - event.getSceneX())
    }))
  }

}

/**
 * See video and some comments on http://ladstatt.blogspot.com/
 */
object PlantSomeTrees {

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[PlantSomeTrees], args: _*)
  }

}

class PlantSomeTrees extends javafx.application.Application with ShapeUtils with Spinner {

  import Types._
  val canvasWidth = 800
  val canvasHeight = 600
  val treeDepth = 5
  val trunkWidth = 30
  val trunkColor = Color.BURLYWOOD
  val curDetail = 2
  val displace = 40
  val treeSize = 400
  val (minDegree, maxDegree) = (Pi / 4, Pi * 3 / 8)
  val growingSpeed = 196

  val spectatorLight = {
    val l = new PointLight(Color.WHITE)
    l.setTranslateZ(-500)
    l
  }

  val l2 = {
    val l = new PointLight(Color.WHITE)
    l.setTranslateX(canvasWidth / 2)
    l.setTranslateY(canvasHeight / 2)
    l.setTranslateZ(500)
    l
  }

  val colors = {
    var initColor = trunkColor
    for (i <- 0 to treeDepth) yield {
      initColor = initColor.brighter().brighter.brighter
      initColor
    }
  }

  def mkMaterial(color: Color) = {
    val m = new PhongMaterial()
    m.setDiffuseColor(color)
    m
  }

  def treeMaterials = for (c <- colors) yield mkMaterial(c)

  def leafMaterial = {
    val m = new PhongMaterial()
    m.setDiffuseColor(Color.GREEN)
    m
  }

  def growTree(drawingArea: Group, start: SPoint3D) = {
    val dest = start + SPoint3D(0, treeSize, 0)
    val broot = Branch(dest, start, treeMaterials(treeDepth), trunkWidth, treeDepth)
    var cylinders2Paint = traverse(mkRandomTree(broot)).toList

    val tree = new Group()

    drawingArea.getChildren.add(tree)
    val growTimeline = new Timeline
    growTimeline.setRate(growingSpeed)
    growTimeline.setCycleCount(Animation.INDEFINITE)
    growTimeline.getKeyFrames().add(
      new KeyFrame(Duration.seconds(1), mkEventHandler((event: ActionEvent) => {
        if (!cylinders2Paint.isEmpty) {
          val (hd :: tail) = cylinders2Paint
          tree.getChildren.add(hd)
          cylinders2Paint = tail
        } else {
          growTimeline.stop
        }
      })))
    growTimeline.play()
  }

  override def start(stage: Stage): Unit = {
    stage.setTitle("A 3D forest")

    //    val backgroundFill = new LinearGradient(0.0, 1.0, 0.0, 0.0, true, CycleMethod.NO_CYCLE, List(new Stop(0, Color.BLACK), new Stop(1, Color.WHITESMOKE)))
    val backgroundFill = Color.WHITE
    val drawingArea = new Group(spectatorLight, l2)
    drawingArea.setTranslateZ(1500)
    drawingArea.setRotationAxis(SPoint3D(0, 1, 0))

    val scene = new Scene(drawingArea, canvasWidth, canvasHeight)

    val start = SPoint3D(canvasWidth / 2, canvasHeight * 3 / 4, 0)
    growTree(drawingArea, start)
    scene.setFill(backgroundFill)
    stage.setScene(scene)
    val perspectiveCamera = new PerspectiveCamera(false)
    scene.setCamera(perspectiveCamera)
    initSpinner(scene, drawingArea)
    stage.show()
  }

  def mkRandDegree = (maxDegree - minDegree) * Random.nextDouble
  def mkRandColor = {
    def randInt = (Random.nextFloat * 255).toInt
    Color.rgb(randInt, randInt, randInt)
  }

  sealed trait ATree
  case class Branch(source: SPoint3D, dest: SPoint3D, material: Material, width: Int, ord: Int) extends ATree
  case class SubTree(center: ATree, left: ATree, right: ATree) extends ATree

  def mkRandomTree(root: Branch): ATree = {

    def mkRandTree(tree: ATree): ATree =
      tree match {
        case Branch(start, dest, material, width, ord) => {
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
                Branch(start, start + (dir * length), treeMaterials(ord), width, ord - 1), // trunk
                Branch(midRoot, midRoot + (dir.spin(mkRandDegree) * l1), treeMaterials(ord - 1), if (width > 1) width / 2 else 1, ord - 1), // leftbranch
                Branch(midRoot, midRoot + (dir.spin(-mkRandDegree) * l2), treeMaterials(ord - 1), if (width > 1) width / 2 else 1, ord - 1))) // rightbranch
            }
          }
        }
        case SubTree(center, left, right) => SubTree(mkRandTree(center), mkRandTree(left), mkRandTree(right))

      }

    mkRandTree(root)
  }

  def mkLeaves(start: SPoint3D, dest: SPoint3D, material: Material): List[Shape3D] = {
    val n = (dest - start).normal
    val p1 = dest + n
    val p2 = dest + (n * -1)
    List(mkSphere(p1, n.length * scala.util.Random.nextDouble, material), mkSphere(p2, n.length * scala.util.Random.nextDouble, material))
  }

  def traverse(tree: ATree): List[Shape3D] = {
    tree match {
      case Branch(start, dest, m, width, ord) => {
        val points = mkMidPointReplacement(start, dest, displace, curDetail)
        width match {
          case 1 => {
            points.map {
              case (start, dest) => {
                mkCylinder(start, dest, if (width > 0) width else 1, m)
              }
            }
            //            ++ // leaves, triggers exception after a while
            //              (points.map {
            //                case (start, dest) => mkLeaves(start, dest, leafMaterial)
            //              }).flatten

          }
          case _ => points.map {
            case (start, dest) => {
              //              println(start + " -> " + dest)
              mkCylinder(start, dest, width, m)
            }
          }
        }
      }
      case SubTree(center, left, right) => traverse(center) ++ traverse(left) ++ traverse(right)
    }
  }

}

