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

package uk.ac.ed.inf.mois.forgerpeskin

import uk.ac.ed.inf.mois.{Model, ODE, Process, ProcessGroup, VarCalc, Math}
import uk.ac.ed.inf.mois.sched.NaiveScheduler
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class MCCSecondary extends ODE with VarCalc {

  /* Global variables */

  val nl = Double("c:nl") default(2.31) param()
  nl annotate("description", "Nuclear localisation of PERp and bound proteins")
  nl annotate("units", "1/h")

  val ne = Double("c:ne") default(0.71) param()
  ne annotate("description", "Nuclear export of PER and bound proteins")
  ne annotate("units", "1/h")

  val Nf = Double("c:Nf") default(115.76) param()
  Nf annotate("description", "Ratio of nuclear to cytoplasmic compartment volume")


  /* REVERBa Gene */

  val McRv = Double("c:McRv") default(0.000005)
  McRv annotate("description", "Cytoplasmic REVERBa mRNA")
  McRv annotate("units", "nM")


  /* REVERBa Protein */

  val Rv = Double("c:Rv") default(0.000001)
  Rv annotate("description", "Cytoplasmic REVERBa")
  Rv annotate("units", "nM")

  val Rvn = Double("c:Rvn") default(0)
  Rvn annotate("description", "Nuclear REVERBa")
  Rvn annotate("units", "nM")

  val RvRv = Double("c:RvRv") default(0)
  RvRv annotate("description", "Cytoplasmic REVERBa dimer")
  RvRv annotate("units", "nM")

  val RvnRvn = Double("c:RvnRvn") default(0)
  RvnRvn annotate("description", "Nuclear REVERBa dimer")
  RvnRvn annotate("units", "nM")

  val tlrv = Double("c:tlrv") default(2.53) param()
  tlrv annotate("description", "Translation of REVERBa")
  tlrv annotate("units", "1/h")

  val arv = Double("c:arv") default(0.21) param()
  arv annotate("description", "Dimerisation of REVERBa")
  arv annotate("units", "1/nM 1/h")

  val drv = Double("c:drv") default(3.62) param()
  drv annotate("description", "Undimerisation of REVERBa")
  drv annotate("units", "1/h")

  val uRv = Double("c:uRv") default(16.25) param()
  uRv annotate("description", "Degradation of REVERBa")
  uRv annotate("units", "1/h")


  d(Rv) := (tlrv * McRv) - (2 * arv * Rv * Rv) + (2 * drv * RvRv) - (nl * Rv) + (ne * Rvn) - (uRv * Rv)
  d(Rvn) := - (2 * Nf * arv * Rvn * Rvn) + (2 * drv * RvnRvn) + (nl * Rv) - (ne * Rvn) - (uRv * Rvn)
  d(RvRv) := (arv * Rv * Rv) - (drv * RvRv) - (nl * RvRv) + (ne * RvnRvn) - (2 * uRv * RvRv)
  d(RvnRvn) := (Nf * arv * Rvn * Rvn) - (drv * RvnRvn) + (nl * RvRv) - (ne * RvnRvn) - (2 * uRv * RvnRvn)
}