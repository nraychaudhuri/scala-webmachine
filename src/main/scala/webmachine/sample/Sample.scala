package webmachine.sample

import webmachine._

object Sample {
  
  def main(args: Array[String]) {
     Route("/", new SampleResource)
     WebmachineJetty.start(8080)
  }
}