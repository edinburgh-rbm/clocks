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

class MCMGenes extends ODE with VarCalc {

  /* Global variables */

  val L = Double("c:L") default(0.000339)
  L annotate("description", "Effect of light level on transcription")

  val tmc = Double("c:tmc") default(0.42)
  tmc annotate("description", "Preparation and nuclear export of all mRNA")
  tmc annotate("units", "1/h")

  val umR = Double("c:umR") default(0.3)
  umR annotate("description", "Degradation of CRY1 and CRY2 mRNA")
  umR annotate("units", "1/h")


  /* Probabilities of promoter binding */

  val G = Double("c:G") default(0.884)
  G annotate("description", "Probability of CRY bound to promoter")

  val GRv = Double("c:GRv") default(0)
  GRv annotate("description", "Probability of REVERBa bound to promoter")

  val bin = Double("c:bin") default(1476.52)
  bin annotate("description", "Binding of CRY to promoter in nucleus")
  bin annotate("units", "1/nM 1/h")

  val unbin = Double("c:unbin") default(23.78)
  unbin annotate("description", "Unbinding of CRY from promoter in nucleus")
  unbin annotate("units", "1/h")

  val binRv = Double("c:binRv") default(0.13)
  binRv annotate("description", "Normalised binding of REVERBa to promoter in nucleus")
  binRv annotate("units", "1/nM 1/h")

  val unbinRv = Double("c:unbinRv") default(21.76)
  unbinRv annotate("description", "Normalised unbinding of REVERBa from promoter in nucleus")
  unbinRv annotate("units", "1/h")


  /* PER1 Gene */

  val MnPo = Double("c:MnPo") default(0.039)
  MnPo annotate("description", "Nuclear PER1 mRNA")
  MnPo annotate("units", "nM")

  val McPo = Double("c:McPo") default(0.00264)
  McPo annotate("description", "Cytoplasmic PER1 mRNA")
  McPo annotate("units", "nM")

  val trPo = Double("c:trPo") default(807.4)
  trPo annotate("description", "Transcription of PER1")
  trPo annotate("units", "1/h")

  val umPo = Double("c:umPo") default(6.21)
  umPo annotate("description", "Degradation of PER1 mRNA")
  umPo annotate("units", "1/h")


  /* PER2 Gene */

  val MnPt = Double("c:MnPt") default(0.015)
  MnPt annotate("description", "Nuclear PER2 mRNA")
  MnPt annotate("units", "nM")

  val McPt = Double("c:McPt") default(0.017)
  McPt annotate("description", "Cytoplasmic PER2 mRNA")
  McPt annotate("units", "nM")

  val trPt = Double("c:trPt") default(308.8)
  trPt annotate("description", "Transcription of PER2")
  trPt annotate("units", "1/h")

  val umPt = Double("c:umPt") default(0.38)
  umPt annotate("description", "Degradation of PER2 mRNA")
  umPt annotate("units", "1/h")


  /* CRY1 Gene */

  val MnRo = Double("c:MnRo") default(2.478)
  MnRo annotate("description", "Nuclear CRY1 mRNA")
  MnRo annotate("units", "nM")

  val McRo = Double("c:McRo") default(3.486)
  McRo annotate("description", "Cytoplasmic CRY1 mRNA")
  McRo annotate("units", "nM")

  val trRo = Double("c:trRo") default(9.03)
  trRo annotate("description", "Transcription of CRY1")
  trRo annotate("units", "1/h")


  /* CRY2 Gene */

  val MnRt = Double("c:MnRt") default(2.1)
  MnRt annotate("description", "Nuclear CRY2 mRNA")
  MnRt annotate("units", "nM")

  val McRt = Double("c:McRt") default(2.96)
  McRt annotate("description", "Cytoplasmic CRY2 mRNA")
  McRt annotate("units", "nM")

  val trRt = Double("c:trRt") default(7.66)
  trRt annotate("description", "Transcription of CRY2")
  trRt annotate("units", "1/h")


  /* CRY Proteins */

  val Ron = Double("c:Ron") default(0.0253)
  Ron annotate("description", "Nuclear CRY1")
  Ron annotate("units", "nM")

  val Rtn = Double("c:Rtn") default(0.0124)
  Rtn annotate("description", "Nuclear CRY2")
  Rtn annotate("units", "nM")


  /* REVERBa Gene */

  val MnRv = Double("c:MnRv") default(0.000182)
  MnRv annotate("description", "Nuclear REVERBa mRNA")
  MnRv annotate("units", "nM")

  val McRv = Double("c:McRv") default(0.000005)
  McRv annotate("description", "Cytoplasmic REVERBa mRNA")
  McRv annotate("units", "nM")

  val trRv = Double("c:trRv") default(0.05)
  trRv annotate("description", "Transcription of REVERBa")
  trRv annotate("units", "1/h")

  val umRv = Double("c:umRv") default(15.11)
  umRv annotate("description", "Degradation of REVERBa mRNA")
  umRv annotate("units", "1/h")


  /* REVERBa Protein */

  val RvnRvn = Double("c:RvnRvn") default(0)
  RvnRvn annotate("description", "Nuclear REVERBa dimer")
  RvnRvn annotate("units", "nM")


  /* PER-CRY Complexes */

  val PonpRon = Double("c:PonpRon") default(0.000543)
  PonpRon annotate("description", "Nuclear PER1p-CRY1")
  PonpRon annotate("units", "nM")

  val PtnpRon = Double("c:PtnpRon") default(0.012)
  PtnpRon annotate("description", "Nuclear PER2p-CRY1")
  PtnpRon annotate("units", "nM")

  val PonpCnRon = Double("c:PonpCnRon") default(0.0015)
  PonpCnRon annotate("description", "Nuclear kinase-bound PER1p-CRY1")
  PonpCnRon annotate("units", "nM")

  val PtnpCnRon = Double("c:PtnpCnRon") default(0.0373)
  PtnpCnRon annotate("description", "Nuclear kinase-bound PER2p-CRY1")
  PtnpCnRon annotate("units", "nM")

  val PonpRtn = Double("c:PonpRtn") default(0.00033)
  PonpRtn annotate("description", "Nuclear PER1p-CRY2")
  PonpRtn annotate("units", "nM")

  val PtnpRtn = Double("c:PtnpRtn") default(0.007)
  PtnpRtn annotate("description", "Nuclear PER2p-CRY2")
  PtnpRtn annotate("units", "nM")

  val PonpCnRtn = Double("c:PonpCnRtn") default(0.0009)
  PonpCnRtn annotate("description", "Nuclear kinase-bound PER1p-CRY2")
  PonpCnRtn annotate("units", "nM")

  val PtnpCnRtn = Double("c:PtnpCnRtn") default(0.022)
  PtnpCnRtn annotate("description", "Nuclear kinase-bound PER2p-CRY2")
  PtnpCnRtn annotate("units", "nM")

  val PonppRon = Double("c:PonppRon") default(0.000525)
  PonppRon annotate("description", "Nuclear PER1pp-CRY1")
  PonppRon annotate("units", "nM")

  val PtnppRon = Double("c:PtnppRon") default(0)
  PtnppRon annotate("description", "Nuclear PER2pp-CRY1")
  PtnppRon annotate("units", "nM")

  val PonppCnRon = Double("c:PonppCnRon") default(0.0018)
  PonppCnRon annotate("description", "Nuclear kinase-bound PER1pp-CRY1")
  PonppCnRon annotate("units", "nM")

  val PtnppCnRon = Double("c:PtnppCnRon") default(0)
  PtnppCnRon annotate("description", "Nuclear kinase-bound PER2pp-CRY1")
  PtnppCnRon annotate("units", "nM")

  val PonppRtn = Double("c:PonppRtn") default(0.00031)
  PonppRtn annotate("description", "Nuclear PER1pp-CRY2")
  PonppRtn annotate("units", "nM")

  val PtnppRtn = Double("c:PtnppRtn") default(0)
  PtnppRtn annotate("description", "Nuclear PER2pp-CRY2")
  PtnppRtn annotate("units", "nM")

  val PonppCnRtn = Double("c:PonppCnRtn") default(0.001)
  PonppCnRtn annotate("description", "Nuclear kinase-bound PER1pp-CRY2")
  PonppCnRtn annotate("units", "nM")

  val PtnppCnRtn = Double("c:PtnppCnRtn") default(0)
  PtnppCnRtn annotate("description", "Nuclear kinase-bound PER2pp-CRY2")
  PtnppCnRtn annotate("units", "nM")


  /* Derived quantities */

  val Rn = Double("c:Rn") default(0)
  Rn annotate("description", "Total nuclear CRY")
  calc(Rn) := Ron + PonpRon + PonppRon + PonpCnRon + PonppCnRon + PtnpRon + PtnppRon + PtnpCnRon + PtnppCnRon + Rtn + PonpRtn + PonppRtn + PonpCnRtn + PonppCnRtn + PtnpRtn + PtnppRtn + PtnpCnRtn + PtnppCnRtn



  d(G) := (bin * Rn * (1-G)) - (unbin * G)
  d(GRv) := (binRv * RvnRvn * (1-GRv)) - (unbinRv * GRv)

  d(MnRo) := (trRo * (1-G) * ((1-GRv)**3)) - (tmc * MnRo)
  d(McRo) := (tmc * MnRo) - (umR * McRo)
  d(MnRt) := (trRt * (1-G)) - (tmc * MnRt)
  d(McRt) := (tmc * MnRt) - (umR * McRt)
  d(MnPo) := (trPo * ((1-G)**5) + L) - (tmc * MnPo)
  d(McPo) := (tmc * MnPo) - (umPo * McPo)
  d(MnPt) := (trPt * ((1-G)**5) + L) - (tmc * MnPt)
  d(McPt) := (tmc * MnPt) - (umPt * McPt)
  d(MnRv) := (trRv * ((1-G)**3)) - (tmc * MnRv)
  d(McRv) := (tmc * MnRv) - (umRv * McRv)

}