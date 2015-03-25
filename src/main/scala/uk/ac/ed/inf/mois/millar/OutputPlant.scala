 /*
  *  MOIS Examples: Arabidopsis Whole-Organism (Forger-Peskin 2014)
  *  Copyright (C) 2014 University of Edinburgh School of Informatics
  *
  *  This program is free software: you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation, either version 3 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program. If not, see <http://www.gnu.org/licenses/>.
  */

package uk.ac.ed.inf.mois.millar

import uk.ac.ed.inf.mois.{StepHandler, Process}
import scala.collection.mutable
import spire.algebra.Rig
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class OutputPlant() extends StepHandler {
  val file = new java.io.File("plant.out")
  val fp = new java.io.PrintWriter(file)
  def init(t: Double, proc: Process) = {
    fp.write("(Leaf number, physiological age, dry weight, zenithal angle, rosette area)")
  }
  def handleStep(t: Double, proc: Process) {
    var Plant = proc.asInstanceOf[FunctionalStructural].Leaves
    for (Leaf <- Plant) {
      fp.write("(" + Leaf.rln + "," + Leaf.n + "," + Leaf.weight + "," + Leaf.a + "," + Leaf.area + ") ")
    }
    fp.write("\n")
  }
  override def reset(t: Double, proc: Process) {
    fp.write("\n")
    handleStep(t, proc)
  }
  override def finish = {
    if (fp.isInstanceOf[java.io.Closeable])
      fp.asInstanceOf[java.io.Closeable].close
  }
}