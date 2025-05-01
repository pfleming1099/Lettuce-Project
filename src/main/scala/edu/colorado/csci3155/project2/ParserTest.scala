package edu.colorado.csci3155.project2

import scala.io.Source




import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.{BorderPanel, Button,  MainFrame, SimpleSwingApplication}
import scala.swing.event.{ButtonClicked, MouseClicked}

object ParserTest extends App {
    val debug= true 
    if (args.length == 0) {
        println("Missing name of file to run")
        
    } else {
        val filename = args(0)
        val outfile = {
            if (args.length == 2) { args(1) } else { "tests/output.png"}
        }
        val fileContents = Source.fromFile(filename).getLines.mkString
        println(fileContents)
        val p: Program = new LettuceParser().parseString(fileContents)
        if (debug) {
            println("--- Debug --- ")
            println(p)
            println("--- Debug ---")
        }
        val v = Interpreter.evalProgram(p)
        if (debug) {
            println(s"Returned value : $v")
        }
        v match {
            case FigValue(c) => c.renderImage(outfile)
            case _ => ()
        }
    }
    
}