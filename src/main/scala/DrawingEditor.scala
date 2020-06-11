

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.{ImageData, document, html}
import org.scalajs.dom.raw

import scala.collection.mutable.ListBuffer
import scala.util.Random


// drawing editor app
@JSExportTopLevel("DrawingEditor")
object DrawingEditor {

  // click path arrays
  val clickX : ListBuffer[Double] = ListBuffer()
  val clickY : ListBuffer[Double] = ListBuffer()
  val clickDrag : ListBuffer[Boolean] = ListBuffer()

  // current color
  var curColor = "black"

  // prev color - this is used in case of eraser
  // which changes color to white
  var prevColor = "black"

  // line thickness
  var curLineSize = 5

  // draw with solid marker
  var curTool = "Marker"

  // drawing with shape
  var curShapeSet = false
  var curShape = "None"

  def getCanvas(): html.Canvas ={
    document.getElementById("editor-canvas").asInstanceOf[html.Canvas]
  }

  def getCanvasContext(): dom.CanvasRenderingContext2D ={
    val canvas: html.Canvas = getCanvas
    canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
  }

  @JSExport
  def main(): Unit = {

    // when document is loaded setup UI
    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      setupUI()

      val ctx = getCanvasContext()


      val clientRect = ctx.canvas.getBoundingClientRect()
      var paint = false

      ctx.canvas.onmousedown =
        (e: dom.MouseEvent) => {
          var mouseX = e.pageX - clientRect.left
          var mouseY = e.pageY - clientRect.top

          paint = true
          addClick(mouseX, mouseY, false)
          if (curShapeSet) {

            oldImgData = ctx.getImageData(0,0, clientRect.width, clientRect.height)
            redrawShapes(ctx)
          } else {
            redraw(ctx)
          }
        }

      ctx.canvas.onmousemove =
        (e: dom.MouseEvent) => {

          var mouseX = e.pageX - clientRect.left
          var mouseY = e.pageY - clientRect.top

          if (paint) {
            addClick(mouseX, mouseY, true);
            if (curShapeSet) {
              redrawShapes(ctx)
            } else {
              redraw(ctx)
            }

          }
        }

      ctx.canvas.onmouseup =
        (e: dom.MouseEvent) => {
          paint = false
          clearClicks()
        }

      ctx.canvas.onmouseleave =
        (e: dom.MouseEvent) => {
          paint = false
          clearClicks()
        }
    })
  }

  def clearCanvas(): Unit ={
    val canvas: html.Canvas = document.getElementById("editor-canvas").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.clearRect(0,0, ctx.canvas.width, ctx.canvas.height)
    clearClicks()
  }

  var oldImgData : ImageData = null
  def redrawShapes(ctx : dom.CanvasRenderingContext2D): Unit ={
    if(clickX.length > 1) {

      if (curShape == "Line"){


        if (clickX.length > 1) {

          ctx.putImageData(oldImgData, 0, 0)

          //draw new line
          ctx.beginPath
          println("new line startx "+clickX(0)+" starty "+ clickY(0)+" endX "+clickX(clickX.length -1)+ " endY "+clickY(clickY.length - 1))
          ctx.moveTo(clickX(0), clickY(0))
          ctx.lineTo(clickX(clickX.length - 1), clickY(clickY.length - 1))
          ctx.closePath
          ctx.strokeStyle = curColor
          ctx.lineWidth = curLineSize
          ctx.stroke
        }

      } else if (curShape == "Rectangle"){
        ctx.putImageData(oldImgData, 0, 0)
        ctx.strokeStyle = curColor
        ctx.lineWidth = curLineSize
        // need to be able to draw rectangles right to left
        var x = clickX(0)
        var y = clickY(0)
        var width = clickX(clickX.length - 1) - clickX(0)
        var height = clickY(clickY.length - 1) - clickY(0)

        if (clickX(0) > clickX(clickX.length-1)){
          x = clickX(clickX.length - 1)
          width = Math.abs(width)
        }
        if (clickY(0) > clickY(clickY.length - 1)){
          y = clickY(clickY.length - 1)
          height = Math.abs(height)
        }
        ctx.strokeRect(x, y, width, height)
      } else if (curShape == "Ellipse"){
        ctx.putImageData(oldImgData, 0, 0)
        ctx.strokeStyle = curColor
        ctx.lineWidth = curLineSize
        // need to be able to draw ellipses right to left
        val x = clickX(0)
        val y = clickY(0)
        val x2 = clickX(clickX.length - 1)
        val y2 = clickY(clickY.length - 1)

        ctx.beginPath


        val len = Math.sqrt(((x - x2) * (x - x2)) + ((y- y2) * (y-y2)))

        ctx.arc(x, y, len, 0, 2 * Math.PI)
        ctx.closePath()
        ctx.stroke()

      } else if (curShape == "Triangle"){

        drawPolygon(ctx, 3)
      } else if (curShape == "Pentagon"){
        drawPolygon(ctx, 5)
      } else if (curShape == "Hexagon"){
        drawPolygon(ctx, 6)
      } else if (curShape == "Heptagon"){
        drawPolygon(ctx, 7)
      } else if (curShape == "Octogon"){
        drawPolygon(ctx, 8)
      } else if (curShape == "Enneagon"){
        drawPolygon(ctx, 9)
      } else if (curShape == "Decagon"){
        drawPolygon(ctx, 10)
      }

    }
  }

  def drawPolygon(ctx: dom.CanvasRenderingContext2D, sides: Int): Unit = {
    ctx.putImageData(oldImgData, 0, 0)
    ctx.strokeStyle = curColor
    ctx.lineWidth = curLineSize

    val x = clickX(0)
    val y = clickY(0)
    val x2 = clickX(clickX.length - 1)
    val y2 = clickY(clickY.length - 1)

    val len = Math.sqrt(((x - x2) * (x - x2)) + ((y- y2) * (y-y2)))

    ctx.beginPath
    ctx.moveTo(x + len * Math.cos(0), y + len * Math.sin(0))

    for (i <- 1 to sides) {
      ctx.lineTo(x + len * Math.cos(i * 2 * Math.PI / sides), y + len * Math.sin(i * 2 * Math.PI / sides))
    }

    ctx.closePath()
    ctx.stroke()
  }

  def redraw(ctx: dom.CanvasRenderingContext2D): Unit = {

    ctx.lineJoin = "round"

    if (curShapeSet){
      if (curShape == "Rectangle"){

      }
    }
    var i = 0
    while ( {
      i < clickX.length
    }) {
      ctx.beginPath
      if (clickDrag(i) ) ctx.moveTo(clickX(i - 1), clickY(i - 1))
      else ctx.moveTo(clickX(i) - 1, clickY(i))
      ctx.lineTo(clickX(i), clickY(i))
      ctx.closePath

      ctx.strokeStyle = curColor
      ctx.lineWidth = curLineSize
      ctx.stroke

      i += 1
    }
  }

  def addClick(x: Double, y: Double, dragging: Boolean): Unit ={
    clickX.addOne(x)
    clickY.addOne(y)
    clickDrag.addOne(dragging)
  }

  def clearClicks(): Unit ={
    clickX.clear()
    clickY.clear()
    clickDrag.clear()

  }

  def changeDrawingColor(color: String){
    curColor = color
  }

  def makeColorButton(color : String): Unit ={
    val colButton = document.createElement("button")
    colButton.setAttribute("id", color+"Button")
    colButton.setAttribute("style", "backgound-color : "+color)
    colButton.textContent = color
    colButton.addEventListener("click", { (e: dom.MouseEvent) =>
      changeDrawingColor(color)
    })
    document.body.appendChild(colButton)
  }

  def makeSizeButton(size: Int, text: String): Unit ={
    val sizeButton = document.createElement("button")
    sizeButton.setAttribute("id", "size"+text+"Button")

    sizeButton.textContent = text
    sizeButton.addEventListener("click", { (e: dom.MouseEvent) =>
      curLineSize = size
    })
    document.body.appendChild(sizeButton)
  }

  def makeToolButton(text: String): Unit ={
    val toolButton = document.createElement("button")
    toolButton.setAttribute("id", text+"Button")

    toolButton.textContent = text
    toolButton.addEventListener("click", { (e: dom.MouseEvent) =>
      curTool = text
      if (text == "Eraser") {
        prevColor = curColor
        curColor = "white"
        curShapeSet = false

      } else if (text == "Crayon"){
        curColor = prevColor
        curShapeSet = false
      } else {
        curColor = prevColor
        curShapeSet = false
      }
    })
    document.body.appendChild(toolButton)
  }


  def makeShapeButton(shape:String): Unit ={
    val shapeButton = document.createElement("button")
    shapeButton.setAttribute("id", shape+"Button")

    shapeButton.textContent = shape
    shapeButton.addEventListener("click", { (e: dom.MouseEvent) =>
      curShapeSet = true
      curShape = shape
    })
    document.body.appendChild(shapeButton)
  }
  def setupUI(): Unit = {

    val clearButton = document.createElement("button")
    clearButton.setAttribute("id", "clearButton")
    clearButton.textContent = "clear"
    clearButton.addEventListener("click", { (e: dom.MouseEvent) =>
      clearCanvas()
    })
    document.body.appendChild(clearButton)

    makeColorButton("red")
    makeColorButton("blue")
    makeColorButton("yellow")
    makeColorButton("green")
    makeColorButton("orange")
    makeColorButton("purple")
    makeColorButton("black")
    makeColorButton("brown")
    makeColorButton("pink")

    document.body.appendChild(document.createElement("br"))

    makeSizeButton(1, "Small")
    makeSizeButton(5,"Normal")
    makeSizeButton(10, "Large")
    makeSizeButton(20, "Extra Large")


    document.body.appendChild(document.createElement("br"))

    makeToolButton("Marker")
    makeToolButton("Eraser")
    document.body.appendChild(document.createElement("br"))

    makeShapeButton("Line")
    makeShapeButton("Rectangle")
    makeShapeButton("Ellipse")
    makeShapeButton("Triangle")
    makeShapeButton("Pentagon")
    makeShapeButton("Hexagon")
    makeShapeButton("Heptagon")
    makeShapeButton("Octogon")
    makeShapeButton("Enneagon")
    makeShapeButton("Decagon")

    document.body.appendChild(document.createElement("br"))


    val canvas = document.createElement("canvas")
    canvas.setAttribute("id", "editor-canvas")
    canvas.setAttribute("width", "600")
    canvas.setAttribute("height", "600")
    canvas.setAttribute("style", "border : solid black 5px")
    document.body.appendChild(canvas)



  }

}
