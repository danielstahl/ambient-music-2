package net.soundmining

import de.sciss.osc.UDP.Receiver
import de.sciss.osc.{Message, Packet, PacketCodec, UDP}
import net.soundmining.modular.ModularSynth.{lineControl, relativePercControl, staticControl}
import net.soundmining.modular.SynthPlayer
import net.soundmining.synth.{Instrument, SuperColliderClient}
import net.soundmining.synth.SuperColliderClient.loadDir

import java.net.SocketAddress
import scala.annotation.tailrec
import scala.util.Random

object AmbientMusic2 {

  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val synthPlayer = SynthPlayer(soundPlays = Map.empty, numberOfOutputBuses = 64)
  var midiServer: Receiver.Undirected = _
  implicit val random: Random = new Random()

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    synthPlayer.init()


    val cfg = UDP.Config()
    cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts()
    cfg.localPort = 57111
    this.midiServer = UDP.Receiver(cfg)
    this.midiServer.connect()
    this.midiServer.action = midiReply
  }

  val lowNoteChain = MarkovChain(
    Map(
      0 -> Seq((1, 0.5), (2, 0.1), (3, 0.3), (4, 0.2)),
      1 -> Seq((0, 0.1), (2, 0.3), (3, 0.2), (4, 0.4)),
      2 -> Seq((0, 0.1), (1, 0.2), (3, 0.4), (4, 0.3)),
      3 -> Seq((0, 0.2), (1, 0.4), (2, 0.2), (4, 0.2)),
      4 -> Seq((0, 0.1), (1, 0.3), (2, 0.4), (3, 0.2))),
    0)

  // 5, 6, 7, 8, 9, 10, 11, 12

  val middleNotes = MarkovChain(
    Map(
      5 -> Seq((6, 0.1), (7, 0.1), (8, 0.1), (9, 0.1), (10, 0.1), (11, 0.1), (12, 0.1)),
      6 -> Seq((5, 0.1), (7, 0.1), (8, 0.1), (9, 0.1), (10, 0.1), (11, 0.1), (12, 0.1)),
      7 -> Seq((5, 0.1), (6, 0.1), (8, 0.1), (9, 0.1), (10, 0.1), (11, 0.1), (12, 0.1)),
      8 -> Seq((5, 0.1), (6, 0.1), (7, 0.1), (9, 0.1), (10, 0.1), (11, 0.1), (12, 0.1)),
      9 -> Seq((5, 0.1), (6, 0.1), (7, 0.1), (8, 0.1), (10, 0.1), (11, 0.1), (12, 0.1)),
      10 -> Seq((5, 0.1), (6, 0.1), (7, 0.1), (8, 0.1), (9, 0.1), (11, 0.1), (12, 0.1)),
      11 -> Seq((5, 0.1), (6, 0.1), (7, 0.1), (8, 0.1), (9, 0.1), (10, 0.1), (12, 0.1)),
      12 -> Seq((5, 0.1), (6, 0.1), (7, 0.1), (8, 0.1), (9, 0.1), (10, 0.1), (11, 0.1)),
    ), 7)

  val highNotes = MarkovChain(
    Map(
      13 -> Seq((14, 0.1), (15, 0.1), (16, 0.1), (17, 0.1), (18, 0.1), (19, 0.1), (20, 0.1)),
      14 -> Seq((13, 0.1), (15, 0.1), (16, 0.1), (17, 0.1), (18, 0.1), (19, 0.1), (20, 0.1)),
      15 -> Seq((13, 0.1), (14, 0.1), (16, 0.1), (17, 0.1), (18, 0.1), (19, 0.1), (20, 0.1)),
      16 -> Seq((13, 0.1), (14, 0.1), (15, 0.1), (17, 0.1), (18, 0.1), (19, 0.1), (20, 0.1)),
      17 -> Seq((13, 0.1), (14, 0.1), (15, 0.1), (16, 0.1), (18, 0.1), (19, 0.1), (20, 0.1)),
      18 -> Seq((13, 0.1), (14, 0.1), (15, 0.1), (16, 0.1), (17, 0.1), (19, 0.1), (20, 0.1)),
      19 -> Seq((13, 0.1), (14, 0.1), (15, 0.1), (16, 0.1), (17, 0.1), (18, 0.1), (20, 0.1)),
      20 -> Seq((13, 0.1), (14, 0.1), (15, 0.1), (16, 0.1), (17, 0.1), (18, 0.1), (19, 0.1)),
    ), 17)

  val velocities = WeightedRandom(Seq(("low", 0.1), ("middle", 0.8), ("high", 0.1)))

  val spectrum1 = (Note.noteToHertz("c2"), Note.noteToHertz("g3"))
  val spectrum2 = (Note.noteToHertz("d2"), Note.noteToHertz("ciss3"))
  val spectrum3 = (Note.noteToHertz("aiss1"), Note.noteToHertz("diss3"))


  val timeConstant = 1.0

  val totalDuration = 13.0 * 60

  val lengthLimit = 60

  def limitLength(len: Double): Double =
    len match {
      case normal if normal < lengthLimit => normal
      case long if long > 60 =>
        println(s"$long is to long")
        limitLength(long * (0.5 + random.nextDouble() * 0.5) )
    }

  val golden1 = totalDuration * Spectrum.invPhi
  val golden2 = golden1 + ((totalDuration - golden1) * Spectrum.invPhi)

  def piecePoints() =
    Seq(golden1, golden2, totalDuration).map(_ / 60.0)


  def playOne(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    def spectrum(time: Double): (Double, Double) = {
      if(time < golden1) spectrum1
      else if(time < golden2) spectrum2
      else spectrum3
    }

    var lowTime = start
    println(s"low start $lowTime")
    while(lowTime < totalDuration) {
        val velocity = velocities.choose() match {
          case "low" => (random.nextDouble() * 0.2) + 0.1
          case "middle" => (random.nextDouble() * 0.6) + 0.2
          case "high" => (random.nextDouble() * 0.2) + 0.8 - 0.1
        }

        val baseTime = shortLengths.choose()
        val time = limitLength((timeConstant / velocity) * baseTime)
        val baseLen = middleLengths.choose()
        val length = limitLength((timeConstant / velocity) * baseLen)
        val attack = attacks.pick(4)
        val note = lowNoteChain.next
        val pan = (random.nextDouble() * 2) - 1

        oneNote(lowTime, note, velocity, pan, length, attack, spectrum(lowTime))
        lowTime += time
    }

    var middleTime = longLengths.choose() * random.nextDouble()
    println(s"middle start $middleTime")
    while(middleTime < totalDuration) {
        val velocity = velocities.choose() match {
          case "low" => (random.nextDouble() * 0.2) + 0.1
          case "middle" => (random.nextDouble() * 0.6) + 0.2
          case "high" => (random.nextDouble() * 0.2) + 0.8 - 0.1
        }

        val baseTime = middleLengths.choose()
        val time = limitLength((timeConstant / velocity) * baseTime)
        val baseLen = longLengths.choose()
        val length = limitLength((timeConstant / velocity) * baseLen)
        val attack = attacksSmooth.pick(4)
        val note = middleNotes.next
        val pan = (random.nextDouble() * 2) - 1

        oneNote(middleTime, note, velocity, pan, length, attack, spectrum(middleTime))
        middleTime += time
    }

    var highTime = (longLengths.choose() * random.nextDouble()) + (longLengths.choose() * random.nextDouble())
    println(s"high start $highTime")
    while(highTime < totalDuration) {
        val velocity = velocities.choose() match {
          case "low" => (random.nextDouble() * 0.2) + 0.1
          case "middle" => (random.nextDouble() * 0.6) + 0.2
          case "high" => (random.nextDouble() * 0.2) + 0.8 - 0.1
        }

        val baseTime = middleLengths.choose()
        val time = limitLength((timeConstant / velocity) * baseTime)
        val baseLen = longLengths.choose()
        val length = limitLength((timeConstant / velocity) * baseLen)
        val attack = attacksSmooth.pick(4)
        val note = highNotes.next
        val pan = (random.nextDouble() * 2) - 1

        oneNote(highTime, note, velocity, pan, length, attack, spectrum(highTime))
        highTime += time
    }

  }

  val NOTE_NAMES = Seq("c", "ciss", "d", "diss", "e", "f", "fiss", "g", "giss", "a", "aiss", "h")

  val shortLengths = WeightedRandom(Seq((8, 0.2), (5, 0.3), (3, 0.5)))
  val middleLengths = WeightedRandom(Seq((13, 0.1), (8, 0.7), (5, 0.2)))
  val longLengths = WeightedRandom(Seq((21, 0.2), (13, 0.7), (8, 0.1)))

  val attacks = Picker(Seq(0.1, 0.3, 0.4, 0.5, 0.6, 0.7))
  val attacksSmooth = Picker(Seq(0.3, 0.4, 0.5, 0.6, 0.7))

  def noteHandle(key: Int, velocity: Int): Unit = {
    if (client.clockTime <= 0) client.resetClock
    val start = (System.currentTimeMillis() - (client.clockTime + 1900)) / 1000.0
    val baseLen = shortLengths.choose()
    val attack = attacks.pick(4)
    val pan = (random.nextDouble() * 2) - 1
    oneNote(start, key % 24, velocity / 127.0, pan, math.min((127.0 / velocity) * baseLen, 35), attack)
  }

  def midiReply(packet: Packet, socketAddress: SocketAddress): Unit = {
    packet match {
      case Message("/noteOn", key: Int, velocity: Int) =>
        noteHandle(key, velocity)
      case _ =>
    }
  }

  /*
  * Cellular automata
  * markov chain
  * https://www.youtube.com/watch?v=2xadxlm91xc
  * https://en.wikipedia.org/wiki/Markov_chain#Music
  * https://github.com/DarioBalinzo/Markov4s
  * weighted distribution (https://stackoverflow.com/questions/6409652/random-weighted-selection-in-java)
  * https://www.youtube.com/watch?v=RqHKX4BDJ1Q
  * Brian Eno -> Markov Chain
  *
  * Generative music
  * https://www.youtube.com/watch?v=WOl3X3YdjsM
  * */


  case class Picker[T](items: Seq[T]) {
    def pick(size: Int)(implicit random: Random): Seq[T] =
      random.shuffle(items).take(size)
  }

  case class WeightedRandom[T](pairs: Seq[(T, Double)]) {
    private val sortedPairs: Seq[(T, Double)] = sortPairs(pairs)

    private def sortPairs(pairs: Seq[(T, Double)]): Seq[(T, Double)] = {
      val fact = 1.0 / (pairs.map {
        case (value, probability) => probability
      }.sum)

      pairs.map {
        case (value, probability) => (value, probability * fact)
      }.sortBy {
        case (value, probability) => probability
      }.reverse
    }

    @tailrec
    private def chooseValue(weightedRandom: Double, pairsToChoose: Seq[(T, Double)]): T = {
      pairsToChoose match {
        case (value, probability) :: _ if weightedRandom <= probability => value
        case (value, probability) :: xs => chooseValue(weightedRandom - probability, xs)
      }
    }

    def choose()(implicit random: Random): T =
      chooseValue(random.nextDouble(), sortedPairs)
  }

  case class MarkovChain[T](nodes: Map[T, Seq[(T, Double)]], startNode: T) {
    private var currentNode: T = startNode
    private val compiledNodes: Map[T, WeightedRandom[T]] =
      nodes.map {
        case (value, pairs) => (value, WeightedRandom(pairs))
      }

    def current: T = currentNode

    def next: T = {
      currentNode = compiledNodes(currentNode).choose()
      current
    }
  }

  // c2 g3
  // c2 e3
  // d2 ciss3
  def setSpectrum(fund: String, sec: String): Unit = {
    fundamental = Note.noteToHertz(fund)
    second = Note.noteToHertz(sec)
  }

  var fundamental: Double = Note.noteToHertz("c2")
  var second: Double = Note.noteToHertz("g3")

  def oneNote(start: Double, note: Int, velocity: Double, pan: Double, len: Double, attack: Seq[Double], spect: (Double, Double) = (fundamental, second)): Unit = {
    println(s"start $start note $note vel $velocity pan $pan len $len attack $attack spec $spect")

    val (fund, sec) = spect
    val fact = Spectrum.makeFact(fund, sec)

    val spectrum = Spectrum.makeSpectrum2(fund, fact, 50)

    val spectrumNote = spectrum(note)

    synthPlayer()
      .saw(staticControl(spectrumNote), relativePercControl(0.001, velocity, attack.head, Right(Instrument.SINE)))
      .ring(staticControl(spectrumNote * fact))
      .lowPass(staticControl(spectrum(note + 2)))
      .pan(staticControl(pan - 0.1))
      .playWithDuration(start, len)

    synthPlayer()
      .triangle(staticControl(spectrumNote), relativePercControl(0.001, velocity, attack(1), Right(Instrument.SINE)))
      .ring(staticControl(spectrumNote * fact))
      .lowPass(staticControl(spectrum(note + 3)))
      .pan(staticControl(pan + 0.1))
      .playWithDuration(start, len)

    synthPlayer()
      .pulse(staticControl(spectrumNote), relativePercControl(0.001, velocity * 0.5, attack(2), Right(Instrument.SINE)))
      .ring(staticControl(spectrumNote * fact))
      .highPass(staticControl(spectrum(note + 20)))
      .pan(staticControl(pan))
      .playWithDuration(start, len)

    synthPlayer()
      .whiteNoise(relativePercControl(0.001, velocity * 5, attack(3), Right(Instrument.SINE)))
      .bandPass(staticControl(spectrumNote), staticControl(0.1))
      .ring(staticControl(spectrumNote * fact))
      .pan(lineControl(pan - 0.2, pan + 0.2))
      .playWithDuration(start, len)
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop
    this.midiServer.close()
  }
}
