package net.soundmining

import net.soundmining.modular.ModularSynth.staticControl
import net.soundmining.modular.SynthPlayer
import net.soundmining.synth.{Instrument, SuperColliderClient}
import net.soundmining.synth.SuperColliderClient.loadDir

object ModuleMusic6 {

  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val synthPlayer = SynthPlayer(soundPlays = Map.empty)

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    synthPlayer.init()
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
  * */

  def playSine(): Unit = {
    client.resetClock
    synthPlayer()
      .sine(staticControl(440), staticControl(1))
      .pan(staticControl(0))
      .playWithDuration(0, 1)
  }

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop
  }
}
