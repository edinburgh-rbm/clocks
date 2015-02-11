 /* 
  *  MOIS Examples: Mammalian Circadian Clock (Forger-Peskin 2002)
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
package uk.ac.ed.inf.mois.clocks.test

import spire.implicits._
import uk.ac.ed.inf.mois.implicits._

import scala.language.reflectiveCalls
import org.scalatest.{FlatSpec, Matchers}
import org.scalactic.TolerantNumerics

import uk.ac.ed.inf.mois.{Model, ProcessGroup, State}
import uk.ac.ed.inf.mois.ode.{ODE, Apache}

import uk.ac.ed.inf.mois.forgerpeskin._

class MammalianCircadianClockModelTest extends FlatSpec with Matchers {

  // Use approximate equality in `should equal`
  val precision = 1e-3
  implicit val doubleEquality =
    TolerantNumerics.tolerantDoubleEquality(precision)

  import scala.math.abs

  def maxerr(s1: Array[Double], s2: Array[Double]) =
    (0 until s1.size).map( i => abs(1 - s1(i)/s2(i)) )
      .max

  def stateStr(s: State) = s.getMeta[Double].map(
    m => s.getVar[Double](m)).toList


  "forgerpeskin models" should "produce similar results whether basal, modular, or drn" in {

    val base = new MCCModel
    val modular = new MCCModelModularFull1
    val drn = new MCCModelModularFull2

    base.init(0)
    base.run(0, 1, 24)
    base.finish

    modular.init(0)
    modular.run(0, 1, 24)
    modular.finish

    drn.init(0)
    drn.run(0, 1, 24)
    drn.finish

    //println(stateStr(drn.process.state))

    val s1: Array[Double] = base.process.state.get[Double]
    val s2: Array[Double] = modular.process.state.get[Double]
    val s3: Array[Double] = drn.process.state.get[Double]
    //(maxerr(s1, s2) < 0.07) should be (true) // Reordering!
    //(maxerr(s1, s3) < 0.07) should be (true) // Reordering, Nfar and Nfac!
    (maxerr(s2, s3) < 0.07) should be (true) // Nfar and Nfac!
  }

  ignore should "give right answers in the longer term too" in {

    val base = new MCCModel
    val modular = new MCCModelModularFull1
    val drn = new MCCModelModularFull2

    base.process.step(0, 120)
    modular.process.step(0, 120)
    drn.process.step(0, 120)

    (maxerr(base.process.state.get[Double], modular.process.state.get[Double]) < 0.07) should be (true)
  }

}
