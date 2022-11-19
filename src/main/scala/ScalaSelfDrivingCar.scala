




import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.{ImageData, Node, document, html, window}
import org.scalajs.dom.raw

import scala.collection.mutable.ListBuffer
import scala.util.Random
import org.scalajs.dom.raw.{HTMLCanvasElement, HTMLImageElement}
import scalatags.JsDom.all._
import scalatags.JsDom.svgTags._

import scala.scalajs.js
import upickle.default._

import scala.collection.mutable





// czec phrasbook app
@JSExportTopLevel("ScalaSelfDrivingCar")
object ScalaSelfDrivingCar {

  //var ctx = Option(dom.CanvasRenderingContext2D)
  //var car = Option(Car)
  var bestCar : Car = null

  @JSExport
  def main(): Unit = {

    // when document is loaded setup UI
    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      setupUI()

    })
  }


  def setupUI(): Unit = {


    val carCanvas = document.createElement("canvas")
    carCanvas.setAttribute("id", "road")

    document.body.appendChild(carCanvas)

    carCanvas.setAttribute("width", "200")
    carCanvas.setAttribute("height", window.innerHeight.toString)

    val verticalButtons = document.createElement("div")
    verticalButtons.setAttribute("id", "verticalButtons")

    document.body.appendChild(verticalButtons)

    val networkCanvas = document.createElement("canvas")
    networkCanvas.setAttribute("id", "network")
    document.body.appendChild(networkCanvas)

    networkCanvas.setAttribute("width", "300")
    networkCanvas.setAttribute("height", window.innerHeight.toString)

    val carCtx = document.getElementById("road").asInstanceOf[html.Canvas].getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val networkCtx = document.getElementById("network").asInstanceOf[html.Canvas].getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]


    val road = new Road(carCanvas.clientWidth/2, carCanvas.clientWidth *0.9)
    val N = 100
    var cars = generateCars(N, road)


    bestCar = cars(0)

    val saveButton = button("Save").render

    saveButton.onclick = (e: dom.Event) => { save(/*bestCar*/)}

    verticalButtons.appendChild(saveButton)

    val discardButton = button("Discard").render
    discardButton.onclick = (e: dom.Event) => { remove()}

    verticalButtons.appendChild(discardButton)


    if (window.localStorage.getItem("bestBrain") != null){
      println("bestBrain != null")
      bestCar = cars(0)
      var a = 0
      for (a <- 0 until cars.length){
        cars(a).brain = retrieve(bestCar)
        if (a != 0){
          NeuralNetwork.mutate(cars(a).brain, 0.12)
        }
      }
    }

    //if (window.localStorage.getItem("bestBrain") != null){
    //  bestCar.brain = JSON.parse("bestBrain", window.localStorage.getItem("bestBrain"))
    //}
    //val car = new Car(road.getLaneCenter(1), 100.0, 30.0, 50.0, "AI", 2 )*/

    val traffic = new ListBuffer[Car] ()
    traffic += new Car(road.getLaneCenter(1), -100.0, 30.0, 50.0 , "DUMMY",
      2, Utils.getRandomColor())
    traffic += new Car(road.getLaneCenter(0), -300.0, 30.0, 50.0 , "DUMMY",
      2, Utils.getRandomColor())
    traffic += new Car(road.getLaneCenter(2), -300.0, 30.0, 50.0 , "DUMMY",
      2, Utils.getRandomColor())
    traffic += new Car(road.getLaneCenter(0), -500.0, 30.0, 50.0 , "DUMMY",
      2, Utils.getRandomColor())
    traffic += new Car(road.getLaneCenter(1), -500.0, 30.0, 50.0 , "DUMMY",
      2, Utils.getRandomColor())
    traffic += new Car(road.getLaneCenter(1), -700.0, 30.0, 50.0 , "DUMMY",
      2, Utils.getRandomColor())
    traffic += new Car(road.getLaneCenter(2), -700.0, 30.0, 50.0 , "DUMMY",
      2, Utils.getRandomColor())
    //traffic += new Car(road.getLaneCenter(0), -300.0, 30.0, 50.0 , "DUMMY",  1)
    animate(0, cars,  traffic, road, carCtx, networkCtx)



  }

  def retrieve(bestCar : Car): NeuralNetwork = {
    println("trying to retrive brain ")
   // var best = bestCar
    var inString = window.localStorage.getItem("bestBrain")
    val ujson0 = ujson.read(inString)

    var levels = new ListBuffer[Level]()
    var a = 0
    for (a <- 0 until ujson0("levels").arr.length){

      val inputs = ujson0("levels").arr.map(_.obj)(a)("level"+a).arr.map(_.obj)(0)("inputs")
      val outputs = ujson0("levels").arr.map(_.obj)(a)("level"+a).arr.map(_.obj)(1)("outputs")
      val biases = ujson0("levels").arr.map(_.obj)(a)("level"+a).arr.map(_.obj)(2)("biases")
      val weights = ujson0("levels").arr.map(_.obj)(a)("level"+a).arr.map(_.obj)(3)("weights")

      var ia = 0
      var newInputs : ListBuffer[Double] = new ListBuffer[Double]()
      for (ia <- 0 until inputs.arr.length){
        var doubleVal = inputs(ia).num
        newInputs += doubleVal
      }

      var oa = 0
      var newOutputs : ListBuffer[Double] = new ListBuffer[Double]()
      for (oa <- 0 until outputs.arr.length){
        var doubleVal = outputs(oa).num
        newOutputs += doubleVal
      }
      var ba = 0
      var newBiases : ListBuffer[Double] = new ListBuffer[Double]()
      for (ba <- 0 until biases.arr.length){
        var doubleVal = biases(ba).num
        newBiases += doubleVal
      }
      var wa = 0
      var newWeights : ListBuffer[ListBuffer[Double]] = new ListBuffer[ListBuffer[Double]]()
      for (wa <- 0 until weights.arr.length){
        var la = 0
        var list = new ListBuffer[Double]()
        for (la <- 0 until weights(wa).arr.length){
          var doubleVal = weights(wa)(la).num
          list += doubleVal
        }
        newWeights +=list
      }
      var level = new Level(1,1)
      level.inputs = newInputs
      level.outputs = newOutputs
      level.biases = newBiases
      level.weights = newWeights
      levels += level

    }

    var args = new ListBuffer[Int]()
    args += 5
    args += 6
    args += 4
    var brain = new NeuralNetwork(args)
      brain.levels = levels
    //best.brain.levels = levels
   // println("BEST AFTER RETRIEVE "+best.brain.levels(0))
    return brain
  }

  def save(/*bestCar : Car*/): Unit ={
    println("SAVING CAR "+bestCar.brain.levels(0))
    var a = 0
    var outString = "{\"levels\":["

    for (a <- 0 until bestCar.brain.levels.length){
      outString += bestCar.brain.levels(0).getLevelJSON(a)
      if (a != bestCar.brain.levels.length - 1) {
        outString += ","
      }
    }

    outString +="]}"


    window.localStorage.setItem("bestBrain", outString)
  }

  def remove(): Unit = {
    window.localStorage.removeItem("bestBrain")
  }

  def generateCars(N: Int, road: Road): ListBuffer[Car] ={
    var cars = new ListBuffer[Car]()
    var a = 0
    for (a <- 0 to N){
      cars += new Car(road.getLaneCenter(1), 100.0, 30.0, 50.0, "AI")
    }
    return cars
  }

  def animate(timestamp: Double, cars : ListBuffer[Car],  traffic: ListBuffer[Car], road: Road,
              carCtx: dom.CanvasRenderingContext2D, networkCtx: dom.CanvasRenderingContext2D): Unit ={
    var a = 0
    for (a <- 0 to traffic.length-1){
      traffic(a).update(road.borders, new ListBuffer[Car]())
    }

    for (a <- 0 until cars.length){
      cars(a).update(road.borders, traffic)
    }

    var yVals = cars.map(c=>c.y)
    var yMin = yVals.min

    bestCar = cars.find(c=>c.y==yMin).get
    //car.update(road.borders, traffic)
    document.getElementById("road").setAttribute("height", window.innerHeight.toString)

    carCtx.save()
    carCtx.translate(0, -bestCar.y + document.getElementById("road").clientHeight * 0.7)

    road.draw(carCtx)

    for (a <- 0 to traffic.length-1){
      traffic(a).draw(carCtx, "yellow")
    }

    carCtx.globalAlpha=0.2
    for (a <- 0 until cars.length) {
      cars(a).draw(carCtx, "blue")
    }
    carCtx.globalAlpha = 1
    bestCar.draw(carCtx, "blue", true)

    carCtx.restore()

    networkCtx.lineDashOffset = -timestamp /50
    Visualizer.drawNetwork(networkCtx, bestCar.brain)

    window.requestAnimationFrame(animate(_, cars, traffic, road, carCtx, networkCtx))
  }




}

class Car(var x: Double, var y: Double, var width: Double, var height: Double, controlType: String,
          initSpeed : Double = 3.0, carColor: String = "blue"){

  var sensor : Sensor = null
  var brain : NeuralNetwork = null
  if (controlType != "DUMMY") {
    sensor = new Sensor(this)
    var args = new ListBuffer[Int]()
    args += sensor.rayCount
    args += 6
    args += 4
    brain = new NeuralNetwork(args)
  }
  var controls = new Controls(controlType)
  var speed = 0.0
  var acceleration = 0.2
  var maxSpeed = initSpeed
  var friction = 0.05
  var angle = 0.0
  var polygon = new ListBuffer[Point]()
  var damaged = false
  var useBrain = if (controlType == "AI") true else false
  var image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  image.src = "car.png"

  val maskCanvas = document.createElement("canvas")
  maskCanvas.setAttribute("id", "mask")

  //document.body.appendChild(maskCanvas)

  //var mask = document.getElementById("mask").asInstanceOf[html.Canvas]
  maskCanvas.setAttribute("width", width.toString)
  maskCanvas.setAttribute("height",  height.toString)

  val maskCtx = maskCanvas.asInstanceOf[html.Canvas].getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  image.onload= (e : dom.Event) => {

    maskCtx.fillStyle = carColor
    maskCtx.rect(0,0, width, height)
    maskCtx.fill()

    maskCtx.globalCompositeOperation = "destination-atop"
    maskCtx.drawImage(image, 0, 0, width, height)


  }

  def createPolygon(): ListBuffer[Point] ={
    var points = new ListBuffer[Point]()
    val rad = Math.hypot(width, height)/2
    val alpha = Math.atan2(width, height)
    points += Point(x - Math.sin(angle - alpha) * rad, y - Math.cos(angle - alpha) * rad )
    points += Point(x - Math.sin(angle + alpha) * rad, y - Math.cos(angle + alpha) * rad )
    points += Point(x - Math.sin(Math.PI + angle - alpha) * rad, y - Math.cos(Math.PI + angle - alpha) * rad )
    points += Point(x - Math.sin(Math.PI + angle + alpha) * rad, y - Math.cos(Math.PI + angle + alpha) * rad )

    return points
  }

  def assessDamage(borders:  ListBuffer[ListBuffer[Point]], traffic: ListBuffer[Car]): Boolean = {
    var a = 0
    for (a <- 0 to borders.length - 1){
      if (Utils.polysIntersect(polygon, borders(a))) {
        return true
      }
    }
    for (a <- 0 to traffic.length - 1){
      if (Utils.polysIntersect(polygon, traffic(a).polygon)) {
        return true
      }
    }
    return false
  }

  def draw(ctx: dom.CanvasRenderingContext2D, carColor : String, drawSensor : Boolean = false): Unit ={

    /*if (damaged){
      ctx.fillStyle = "red"
    } else {
      ctx.fillStyle = carColor

    }*/

    ctx.save()
    ctx.translate(x, y)
    ctx.rotate(-angle)
    if (!damaged) {
      ctx.drawImage(maskCanvas.asInstanceOf[html.Canvas],
        -width / 2, -height / 2, width, height)
      ctx.globalCompositeOperation = "multiply"
    }
    ctx.drawImage(image,
      -width/2, -height/2, width, height)
    ctx.restore()


    /*ctx.beginPath()
    ctx.moveTo(polygon(0).x, polygon(0).y)
    var a = 0
    for (a <- 1 to polygon.length - 1){
      ctx.lineTo(polygon(a).x, polygon(a).y)
    }
    ctx.fill()


    //ctx.restore()*/
    if (sensor != null && drawSensor) {
      sensor.draw(ctx)
    }
  }

  def move(): Unit = {
    if (controls.forward){
      speed += acceleration
    }
    if (controls.reverse){
      speed -= acceleration
    }
    if (speed > maxSpeed){
      speed = maxSpeed
    }
    if(speed < - maxSpeed / 2){
      speed = - maxSpeed / 2
    }
    if (speed > 0){
      speed -= friction
    }
    if (speed < 0){
      speed += friction
    }
    if (Math.abs(speed) < friction){
      speed = 0
    }

    if (speed != 0) {
      var flip = -1
      if (speed > 0) flip = 1
      if (controls.left) {
        angle += 0.03 * flip
      }
      if (controls.right) {
        angle -= 0.03 * flip
      }
    }

    x -= Math.sin(angle) * speed
    y -= Math.cos(angle) * speed
  }

  def update(borders: ListBuffer[ListBuffer[Point]], traffic: ListBuffer[Car]): Unit ={
    if (!damaged) {
      move()
      polygon = createPolygon()
      damaged = assessDamage(borders, traffic)
    }
    if (sensor != null) {
      sensor.update(borders, traffic)
      val offsets = sensor.readings.map(s => if (s == null) 0.0 else  1 - s._2)
      val outputs = NeuralNetwork.feedForward(offsets, brain)
      //println("outputs in Car.update: "+outputs)

      if (useBrain){
        controls.forward = if (outputs(0) == 1.0) true else false
        controls.left = if (outputs(1) == 1.0) true else false
        controls.right = if (outputs(2) == 1.0) true else false
        controls.reverse = if (outputs(3) == 1.0) true else false
      }
    }

  }
}

class Controls (controlType: String) {
  var forward = false
  var left = false
  var right = false
  var reverse = false

  controlType match {
  case "KEYS" => {
    addKeyboardListeners()
  }
  case "DUMMY" => {
    forward = true
  }
  case _ => {}
  }


  def addKeyboardListeners(): Unit ={
    document.onkeydown =
      (e: dom.KeyboardEvent) => {
        e.key match {
          case "ArrowLeft" => left = true
          case "ArrowRight" => right = true
          case "ArrowUp" => forward = true
          case "ArrowDown" => reverse = true
          case _ => {}

        }
      }

    document.onkeyup =
      (e: dom.KeyboardEvent) => {
        e.key match {
          case "ArrowLeft" => left = false
          case "ArrowRight" => right = false
          case "ArrowUp" => forward = false
          case "ArrowDown" => reverse = false
          case _ => {}
        }
      }
  }
}

class Road(x: Double, width: Double, laneCount : Int = 3 ) {
  val left = x - width /2
  val right = x + width /2

  val infinity = 1000000.0
  val top = -infinity
  val bottom = infinity

  val topLeft = Point(left, top)
  val topRight = Point(right, top)
  val bottomLeft = Point(left, bottom)
  val bottomRight = Point(right, bottom)
  var borders = new ListBuffer[ListBuffer[Point]]()

  var leftSide = new ListBuffer[Point]()
  leftSide += topLeft
  leftSide += bottomLeft

  borders += leftSide

  var rightSide = new ListBuffer[Point]()
  rightSide += topRight
  rightSide += bottomRight

  borders += rightSide



  def getLaneCenter(laneIndex : Int) : Double = {
    val laneWidth = width / laneCount
    left + laneWidth / 2 + Math.min(laneIndex, laneCount - 1) * laneWidth
  }

  def draw(ctx: dom.CanvasRenderingContext2D): Unit = {
    ctx.lineWidth = 5.0
    ctx.strokeStyle = "white"

    var a = 0
    for (a <- 1 to laneCount-1) {
      var b = Utils.lerp(left, right, a.toDouble / laneCount)

      ctx.setLineDash(js.Array(20.0,20.0))


      ctx.beginPath()
      ctx.moveTo(b, top)
      ctx.lineTo(b, bottom)
      ctx.stroke()
    }
    ctx.setLineDash(js.Array())
    var b = 0
    for (b <- 0 to borders.length-1) {
      ctx.beginPath()
      ctx.moveTo(borders(b)(0).x, borders(b)(0).y)
      ctx.lineTo(borders(b)(1).x, borders(b)(1).y)
      ctx.stroke()

    }

  }
}

class Sensor (car : Car){
  val rayCount = 5
  val rayLength = 150
  val raySpread = Math.PI / 2

  var rays = new ListBuffer[(Point, Point)]()
  var readings = new ListBuffer[(Point, Double)]()


  def castRays(): Unit = {
    rays = new ListBuffer[(Point, Point)]()

    var a = 0
    for (a <- 0 to rayCount-1){
      val rayAngle = Utils.lerp(
        raySpread/2, -raySpread/2, if (rayCount == 1) 0.5 else a.toDouble/(rayCount - 1)
      ) + car.angle

      val start =Point(car.x, car.y)
      val end = Point(car.x - Math.sin(rayAngle) * rayLength, car.y - Math.cos(rayAngle) * rayLength)

      // start = x, y end = x,y
      rays += ((start, end))

    }
  }



  def getReadings(ray : (Point, Point), borders: ListBuffer[ListBuffer[Point]], traffic: ListBuffer[Car]):   (Point, Double) ={
    var touches = new ListBuffer[(Point, Double)]()
    var a = 0
    for (a <- 0 to borders.length - 1){
      val touch = Utils.intersection(ray._1, ray._2, borders(a)(0), borders(a)(1))
      if (touch != null) {
        touches += touch
      }


    }

    for (a <- 0 to traffic.length - 1){
      val poly = traffic(a).polygon
      var b = 0
      for (b <- 0 to poly.length - 1){
        val touch = Utils.intersection(ray._1, ray._2, poly(b), poly((b+1)%poly.length))
        if (touch != null) {
          touches += touch
        }
      }
    }

    if (touches.length == 0){
      return null
    } else {
      val offsets = touches.map(e=> e._2)

      val minOffset = offsets.min
      val touch = touches.find(e => e._2 == minOffset).get
      return touch
    }
  }

  def update(borders: ListBuffer[ListBuffer[Point]], traffic: ListBuffer[Car]): Unit ={
    castRays()
    readings = new ListBuffer[(Point, Double)]()
    var a = 0
    for (a <- 0 to rays.length - 1){
      readings += getReadings((rays(a)._1, rays(a)._2), borders, traffic)
    }



  }

  def draw(ctx: dom.CanvasRenderingContext2D): Unit = {
    var a = 0
    for (a <- 0 to rayCount-1){
      var end = rays(a)._2
      if (readings(a) != null){
        end = readings(a)._1
      }
      ctx.beginPath()
      ctx.lineWidth = 2
      ctx.strokeStyle = "yellow"
      ctx.moveTo(rays(a)._1.x, rays(a)._1.y)
      ctx.lineTo(end.x, end.y)
      ctx.stroke()

      ctx.beginPath()
      ctx.lineWidth = 2
      ctx.strokeStyle = "black"
      ctx.moveTo(rays(a)._2.x, rays(a)._2.y)
      ctx.lineTo(end.x, end.y)
      ctx.stroke()
    }
  }
}

object Utils {
  // should be in utils - not sure how this works in scalajs
  def lerp(A : Double, B: Double, t: Double): Double ={
    val res = A + (B - A) * t
    return res
  }
  // should be in utils
  def intersection(A: Point, B: Point, C: Point,  D: Point) : (Point, Double) = {

    val tTop=(D.x-C.x)*(A.y-C.y)-(D.y-C.y)*(A.x-C.x);
    val uTop=(C.y-A.y)*(A.x-B.x)-(C.x-A.x)*(A.y-B.y);
    val bottom=(D.y-C.y)*(B.x-A.x)-(D.x-C.x)*(B.y-A.y);

    if(bottom!=0){
      val t=tTop/bottom;
      val u=uTop/bottom;
      if(t>=0 && t<=1 && u>=0 && u<=1){
        return (Point(
          lerp(A.x,B.x,t),
          lerp(A.y,B.y,t)),
          t
        )
      }
    }

    return null;
  }


  def polysIntersect(A: ListBuffer[Point], B: ListBuffer[Point]): Boolean ={
    var a = 0
    var b = 0
    for (a <- 0 to A.length - 1){
      for (b <- 0 to B.length - 1){
        val touch = intersection(Point(A(a).x, A(a).y), Point(A((a+1)%A.length).x, A((a+1)%A.length).y),
          Point(B(b).x, B(b).y), Point(B((b+1)%B.length).x, B((b+1)%B.length).y))
        if (touch != null){
          return true
        }
      }
    }
    return false
  }

  def getRandomColor() : String = {
    var hue=290+Math.random()*260
    return "hsl("+hue+", 100%, 60%)";
  }
}

case class Point(x: Double, y: Double)

// network

class NeuralNetwork(neuronCounts : ListBuffer[Int]){

  //implicit def rw: RW[NeuralNetwork] = macroRW

  var levels = new ListBuffer[Level]()
  var a = 0
  for (a <- 0 to neuronCounts.length - 2){

    levels += new Level(neuronCounts(a), neuronCounts(a+1))
  }

}

object NeuralNetwork {



  def feedForward (givenInputs: ListBuffer[Double], network: NeuralNetwork): ListBuffer[Double] ={
    var outputs = Level.feedForward(givenInputs, network.levels(0))

    var a = 0
    for (a <- 1 to network.levels.length - 1){
      outputs = Level.feedForward(outputs, network.levels(a))
    }
    return outputs
  }

  def mutate(network: NeuralNetwork, amount: Double): Unit ={
    var a , b, c, d = 0
    for (a <- 0 until network.levels.length){
      for (b <- 0 until network.levels(a).biases.length){
        network.levels(a).biases(b) = Utils.lerp(network.levels(a).biases(b), Math.random * 2 - 1, amount)
      }
      for (c <- 0 until network.levels(a).weights.length) {
        for (d <- 0 until network.levels(a).weights(c).length){
          network.levels(a).weights(c)(d) = Utils.lerp(network.levels(a).weights(c)(d), Math.random * 2 - 1, amount)
        }
      }
    }
  }

}

class Level(inputCount : Int, outputCount : Int){
  var inputs = ListBuffer[Double]()
  var a = 0
  for (a <- 0 to inputCount-1){
    inputs += 0
  }
  var outputs = ListBuffer[Double]()
  for (a <- 0 to outputCount-1){
    outputs += 0
  }

  var biases = ListBuffer[Double](outputCount)
  for (a <- 0 to outputCount-1){
    biases += 0
  }
  var weights = ListBuffer[ListBuffer[Double]]()

  for (a <- 0 to inputCount - 1){
    val innerWeights = ListBuffer[Double]()
    for (a <- 0 to outputCount - 1){
      innerWeights += 0
    }
    weights += innerWeights
  }

  def replace(inputsIn: ListBuffer[Double], outputsIn : ListBuffer[Double], biasesIn : ListBuffer[Double], weightsIn: ListBuffer[ListBuffer[Double]]): Unit ={
    inputs = inputsIn
    outputs = outputsIn
    biases = biasesIn
    weights = weightsIn
  }

  Level.randomize(this)

  //println("levels build arr "+levels)
  def getLevelJSON(a: Int) : ujson.Obj = {
    val ujson0 = ujson.Obj("level"+a -> ujson.Arr(ujson.Obj("inputs" -> inputs), ujson.Obj("outputs" -> outputs),
      ujson.Obj("biases" -> biases), ujson.Obj("weights" -> weights)))
    return ujson0
  }

  override def toString: String = {
    "" + inputs + outputs + biases
  }

}



object Level {

  def randomize(level : Level): Unit ={

    var a, b = 0
    for (a <- 0 to level.inputs.length - 1){
      for (b <- 0 to level.outputs.length - 1){
        level.weights(a)(b) = Math.random() * 2 - 1
      }

    }
    for (a <- 0 to level.biases.length - 1){
      level.biases(a) = Math.random() * 2 - 1
    }
  }

  def feedForward(givenInputs : ListBuffer[Double], level: Level): ListBuffer[Double] ={
    var a = 0
    for (a <- 0 to level.inputs.length - 1){
      level.inputs(a) = givenInputs(a)
    }

    var b = 0
    for (a <- 0 to level.outputs.length - 1){
      var sum = 0.0
      for (b <- 0 to level.inputs.length - 1){
        sum += level.inputs(b) * level.weights(b)(a)
      }
      if (sum > level.biases(a)){
        level.outputs(a) = 1
      } else {
        level.outputs(a) = 0
      }
    }

    return level.outputs
  }
}

object Visualizer {

  def drawNetwork(ctx: dom.CanvasRenderingContext2D, network: NeuralNetwork): Unit = {
    val margin = 50
    val left = margin
    val top = margin
    val width = ctx.canvas.width - margin * 2
    val height = ctx.canvas.height - margin * 2

    val levelHeight = height.toDouble / network.levels.length

    var a = 0
    for (a <- (0 until network.levels.length).reverse ){

      var levelTop = top + Utils.lerp( height - levelHeight, 0,
        if (network.levels.length == 1) 0.5 else a.toDouble / (network.levels.length - 1))

      ctx.setLineDash(js.Array(7, 3) )
      drawLevel(ctx, network.levels(a), left, levelTop, width, levelHeight,
        if (a == network.levels.length - 1) js.Array("🠉","🠈","🠊","🠋") else js.Array())

    }
  }

  def drawLevel (ctx : dom.CanvasRenderingContext2D, level : Level, left : Int, top : Double, width: Int, height: Double, outputLabels: js.Array[String]) {
    val right = left + width
    val bottom = top + height

    val inputs = level.inputs
    val outputs = level.outputs
    val weights = level.weights
    val biases = level.biases

    var a, b = 0

    val nodeRadius = 18;

    for (a <- 0 to inputs.length - 1) {
      for (b <- 0 to outputs.length - 1) {

        ctx.beginPath()
        ctx.moveTo(getNodeX(inputs, a, left, right), bottom)
        ctx.lineTo(getNodeX(outputs, b, left, right), top)
        ctx.lineWidth = 1
        ctx.strokeStyle = getRGBA(weights(a)(b))
        ctx.stroke()
      }
    }

    for (a <- 0 to inputs.length - 1){

      val x = getNodeX(inputs, a, left, right)
      ctx.beginPath()
      ctx.arc(x, bottom, nodeRadius, 0, Math.PI * 2)
      ctx.fillStyle = "black"
      ctx.fill()
      ctx.beginPath()
      ctx.arc(x, bottom, nodeRadius * 0.6, 0, Math.PI * 2)
      ctx.fillStyle = getRGBA(inputs(a))
      ctx.fill()
    }

    for (a <- 0 until outputs.length) {

      val x = getNodeX(outputs, a, left, right)
      ctx.beginPath()
      ctx.arc(x, top, nodeRadius, 0, Math.PI * 2)
      ctx.fillStyle = "black"
      ctx.fill()

      ctx.beginPath()
      ctx.arc(x, top, nodeRadius * 0.6, 0, Math.PI * 2)
      ctx.fillStyle = getRGBA(outputs(a))
      ctx.fill()

      ctx.beginPath()
      ctx.lineWidth = 2
      ctx.arc(x, top, nodeRadius * 0.8, 0, Math.PI * 2)
      ctx.strokeStyle = getRGBA(biases(a))
      ctx.setLineDash(js.Array(3, 3))
      ctx.stroke()
      ctx.setLineDash(js.Array())

      if (outputLabels.length == outputs.length && outputLabels(a) != null) {
        ctx.beginPath()
        ctx.textAlign = "center"
        ctx.textBaseline = "middle"
        ctx.fillStyle = "black"
        ctx.strokeStyle = "white"
        ctx.font = (nodeRadius * 1.5) + "px Arial"
        ctx.fillText(outputLabels(a), x, top + nodeRadius * 0.1)
        ctx.lineWidth = 0.5
        ctx.strokeText(outputLabels(a), x, top + nodeRadius * 0.1)
      }
    }

  }

  def getNodeX(nodes: ListBuffer[Double], index: Int, left : Double, right: Double) : Double = {
    return Utils.lerp(left, right, if ( nodes.length == 1) 0.5 else index.toDouble / (nodes.length - 1))

  }

  def getRGBA(x: Double): String ={
    val alpha = Math.abs(x)
    val R = if (x < 0 ) 0 else 255
    val G = R
    val B = if (x > 0) 0 else 255
    return "rgba("+R+","+G+","+B+","+alpha+")"

  }
}