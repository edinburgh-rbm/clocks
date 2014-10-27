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
import uk.ac.ed.inf.mois.reaction.DeterministicReactionNetwork
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class MCMPrimaryDPN extends DeterministicReactionNetwork with VarCalc {

  /* Global variables */

  val umR = Double("c:umR") default(0.3)
  umR annotate("description", "Degradation of CRY1 and CRY2 mRNA")
  umR annotate("units", "1/h")

  val up = Double("c:up") default(3.39)
  up annotate("description", "Degradation of PERp unbound to CRY")
  up annotate("units", "1/h")

  val ar = Double("c:ar") default(0.27)
  ar annotate("description", "Binding of PER1 and PER2 to CRY1 and CRY2")
  ar annotate("units", "1/nM 1/h")

  val dr = Double("c:dr") default(0.35)
  dr annotate("description", "Unbinding of PER1 and PER2 from CRY1 and CRY2")
  dr annotate("units", "1/h")

  val nl = Double("c:nl") default(2.31)
  nl annotate("description", "Nuclear localisation of PERp and bound proteins")
  nl annotate("units", "1/h")

  val ne = Double("c:ne") default(0.71)
  ne annotate("description", "Nuclear export of PER and bound proteins")
  ne annotate("units", "1/h")

  val Nf = Double("c:Nf") default(115.76)
  Nf annotate("description", "Ratio of nuclear to cytoplasmic compartment volume")


  /* Kinase */

  val Cn = Species("c:Cn") default(0.31)
  Cn annotate("description", "Nuclear kinase")
  Cn annotate("units", "nM")

  val Ct = Species("c:Ct") default(7.71)
  Ct annotate("description", "Total kinase concentration")
  Ct annotate("units", "nM")

  val ac = Double("c:ac") default(0.47)
  ac annotate("description", "Binding of PER1 and PER2 to kinases")
  ac annotate("units", "1/nM 1/h")

  val dc = Double("c:dc") default(5.09)
  dc annotate("description", "Unbinding of PER1 and PER2 from kinases")
  dc annotate("units", "1/h")

  val hoo = Double("c:hoo") default(0.29)
  hoo annotate("description", "Initial phosphorylation of PER1")
  hoo annotate("units", "1/h")

  val hot = Double("c:hot") default(0.09)
  hot annotate("description", "Initial phosphorylation of PER2")
  hot annotate("units", "1/h")

  val hto = Double("c:hto") default(1.45)
  hto annotate("description", "Phosphorylation preventing nuclear localisation of PER1")
  hto annotate("units", "1/h")

  val htt = Double("c:htt") default(0)
  htt annotate("description", "Phosphorylation preventing nuclear localisation of PER2")
  htt annotate("units", "1/h")


  /* PER1 Gene */

  val McPo = Species("c:McPo") default(0.00264)
  McPo annotate("description", "Cytoplasmic PER1 mRNA")
  McPo annotate("units", "nM")


  /* PER2 Gene */

  val McPt = Species("c:McPt") default(0.017)
  McPt annotate("description", "Cytoplasmic PER2 mRNA")
  McPt annotate("units", "nM")


  /* PER Proteins */

  val Po = Species("c:Po") default(0.096)
  Po annotate("description", "Cytoplasmic PER1")
  Po annotate("units", "nM")

  val Pt = Species("c:Pt") default(1.018)
  Pt annotate("description", "Cytoplasmic PER2")
  Pt annotate("units", "nM")

  val PoC = Species("c:PoC") default(0.055)
  PoC annotate("description", "Cytoplasmic kinase-bound PER1")
  PoC annotate("units", "nM")

  val PtC = Species("c:PtC") default(0.604)
  PtC annotate("description", "Cytoplasmic kinase-bound PER2")
  PtC annotate("units", "nM")

  val tlp = Double("c:tlp") default(10)
  tlp annotate("description", "Translation of PER1 and PER2")
  tlp annotate("units", "1/h")

  val upu = Double("c:upu") default(0.08)
  upu annotate("description", "Degradation of unphosphorylated PER")
  upu annotate("units", "1/h")

  val PopC = Species("c:PopC") default(0.0012)
  PopC annotate("description", "Cytoplasmic kinase-bound PER1p")
  PopC annotate("units", "nM")

  val PtpC = Species("c:PtpC") default(0.005)
  PtpC annotate("description", "Cytoplasmic kinase-bound PER2p")
  PtpC annotate("units", "nM")

  val Pop = Species("c:Pop") default(0.000526)
  Pop annotate("description", "Cytoplasmic PER1p")
  Pop annotate("units", "nM")

  val Ptp = Species("c:Ptp") default(0.0027)
  Ptp annotate("description", "Cytoplasmic PER2p")
  Ptp annotate("units", "nM")

  val PonpCn = Species("c:PonpCn") default(0.0006)
  PonpCn annotate("description", "Nuclear kinase-bound PER1p")
  PonpCn annotate("units", "nM")

  val PtnpCn = Species("c:PtnpCn") default(0.0065)
  PtnpCn annotate("description", "Nuclear kinase-bound PER2p")
  PtnpCn annotate("units", "nM")

  val Ponp = Species("c:Ponp") default(0.0002)
  Ponp annotate("description", "Nuclear PER1p")
  Ponp annotate("units", "nM")

  val Ptnp = Species("c:Ptnp") default(0.0021)
  Ptnp annotate("descripion", "Nuclear PER2p")
  Ptnp annotate("units", "nM")

  val PoppC = Species("c:PoppC") default(0.0008)
  PoppC annotate("description", "Cytoplasmic kinase-bound PER1pp")
  PoppC annotate("units", "nM")

  val PtppC = Species("c:PtppC") default(0)
  PtppC annotate("description", "Cytoplasmic kinase-bound PER2pp")
  PtppC annotate("units", "nM")

  val Popp = Species("c:Popp") default(0.0011)
  Popp annotate("description", "Cytoplasmic PER1pp")
  Popp annotate("units", "nM")

  val Ptpp = Species("c:Ptpp") default(0)
  Ptpp annotate("description", "Cytoplasmic PER2pp")
  Ptpp annotate("units", "nM")

  val PonppCn = Species("c:PonppCn") default(0.000323)
  PonppCn annotate("description", "Nuclear kinase-bound PER1pp")
  PonppCn annotate("units", "nM")

  val PtnppCn = Species("c:PtnppCn") default(0)
  PtnppCn annotate("description", "Nuclear kinase-bound PER2pp")
  PtnppCn annotate("units", "nM")

  val Ponpp = Species("c:Ponpp") default(0.000088)
  Ponpp annotate("description", "Nuclear PER1pp")
  Ponpp annotate("units", "nM")

  val Ptnpp = Species("c:Ptnpp") default(0)
  Ptnpp annotate("description", "Nuclear PER2pp")
  Ptnpp annotate("units", "nM")


  /* CRY1 Gene */

  val McRo = Species("c:McRo") default(3.486)
  McRo annotate("description", "Cytoplasmic CRY1 mRNA")
  McRo annotate("units", "nM")


  /* CRY2 Gene */

  val McRt = Species("c:McRt") default(2.96)
  McRt annotate("description", "Cytoplasmic CRY2 mRNA")
  McRt annotate("units", "nM")


  /* CRY Proteins */

  val Ro = Species("c:Ro") default(8.2)
  Ro annotate("description", "Cytoplasmic CRY1")
  Ro annotate("units", "nM")

  val Rt = Species("c:Rt") default(5.18)
  Rt annotate("description", "Cytoplasmic CRY2")
  Rt annotate("units", "nM")

  val Ron = Species("c:Ron") default(0.0253)
  Ron annotate("description", "Nuclear CRY1")
  Ron annotate("units", "nM")

  val Rtn = Species("c:Rtn") default(0.0124)
  Rtn annotate("description", "Nuclear CRY2")
  Rtn annotate("units", "nM")

  val tlr = Double("c:tlr") default(1.031)
  tlr annotate("description", "Translation of CRY1 and CRY2")
  tlr annotate("units", "1/h")

  val uro = Double("c:uro") default(0.44)
  uro annotate("description", "Degradation of CRY1 unbound to PER")
  uro annotate("units", "1/h")

  val urt = Double("c:urt") default(0.59)
  urt annotate("description", "Degradation of CRY2 unbound to PER")
  urt annotate("units", "1/h")


  /* PER-CRY Complexes */

  val PopRo = Species("c:PopRo") default(0.0009)
  PopRo annotate("description", "Cytoplasmic PER1p-CRY1")
  PopRo annotate("units", "nM")

  val PtpRo = Species("c:PtpRo") default(0.01)
  PtpRo annotate("description", "Cytoplasmic PER2p-CRY1")
  PtpRo annotate("units", "nM")

  val PopCRo = Species("c:PopCRo") default(0.0007)
  PopCRo annotate("description", "Cytoplasmic kinase-bound PER1p-CRY1")
  PopCRo annotate("units", "nM")

  val PtpCRo = Species("c:PtpCRo") default(0.009)
  PtpCRo annotate("description", "Cytoplasmic kinase-bound PER2p-CRY1")
  PtpCRo annotate("units", "nM")

  val PonpRon = Species("c:PonpRon") default(0.000543)
  PonpRon annotate("description", "Nuclear PER1p-CRY1")
  PonpRon annotate("units", "nM")

  val PtnpRon = Species("c:PtnpRon") default(0.012)
  PtnpRon annotate("description", "Nuclear PER2p-CRY1")
  PtnpRon annotate("units", "nM")

  val PonpCnRon = Species("c:PonpCnRon") default(0.0015)
  PonpCnRon annotate("description", "Nuclear kinase-bound PER1p-CRY1")
  PonpCnRon annotate("units", "nM")

  val PtnpCnRon = Species("c:PtnpCnRon") default(0.0373)
  PtnpCnRon annotate("description", "Nuclear kinase-bound PER2p-CRY1")
  PtnpCnRon annotate("units", "nM")

  val PopRt = Species("c:PopRt") default(0.00055)
  PopRt annotate("description", "Cytoplasmic PER1p-CRY2")
  PopRt annotate("units", "nM")

  val PtpRt = Species("c:PtpRt") default(0.0064)
  PtpRt annotate("description", "Cytoplasmic PER2p-CRY2")
  PtpRt annotate("units", "nM")

  val PopCRt = Species("c:PopCRt") default(0.0004)
  PopCRt annotate("description", "Cytoplasmic kinase-bound PER1p-CRY2")
  PopCRt annotate("units", "nM")

  val PtpCRt = Species("c:PtpCRt") default(0.0055)
  PtpCRt annotate("description", "Cytoplasmic kinase-bound PER2p-CRY2")
  PtpCRt annotate("units", "nM")

  val PonpRtn = Species("c:PonpRtn") default(0.00033)
  PonpRtn annotate("description", "Nuclear PER1p-CRY2")
  PonpRtn annotate("units", "nM")

  val PtnpRtn = Species("c:PtnpRtn") default(0.007)
  PtnpRtn annotate("description", "Nuclear PER2p-CRY2")
  PtnpRtn annotate("units", "nM")

  val PonpCnRtn = Species("c:PonpCnRtn") default(0.0009)
  PonpCnRtn annotate("description", "Nuclear kinase-bound PER1p-CRY2")
  PonpCnRtn annotate("units", "nM")

  val PtnpCnRtn = Species("c:PtnpCnRtn") default(0.022)
  PtnpCnRtn annotate("description", "Nuclear kinase-bound PER2p-CRY2")
  PtnpCnRtn annotate("units", "nM")

  val PoppRo = Species("c:PoppRo") default(0.0124)
  PoppRo annotate("description", "Cytoplasmic PER1pp-CRY1")
  PoppRo annotate("units", "nM")

  val PtppRo = Species("c:PtppRo") default(0)
  PtppRo annotate("description", "Cytoplasmic PER2pp-CRY1")
  PtppRo annotate("units", "nM")

  val PoppCRo = Species("c:PoppCRo") default(0.008)
  PoppCRo annotate("description", "Cytoplasmic kinase-bound PER1pp-CRY1")
  PoppCRo annotate("units", "nM")

  val PtppCRo = Double("c:PtppCRo") default(0)
  PtppCRo annotate("description", "Cytoplasmic kinase-bound PER2pp-CRY1")
  PtppCRo annotate("units", "nM")

  val PonppRon = Species("c:PonppRon") default(0.000525)
  PonppRon annotate("description", "Nuclear PER1pp-CRY1")
  PonppRon annotate("units", "nM")

  val PtnppRon = Species("c:PtnppRon") default(0)
  PtnppRon annotate("description", "Nuclear PER2pp-CRY1")
  PtnppRon annotate("units", "nM")

  val PonppCnRon = Species("c:PonppCnRon") default(0.0018)
  PonppCnRon annotate("description", "Nuclear kinase-bound PER1pp-CRY1")
  PonppCnRon annotate("units", "nM")

  val PtnppCnRon = Species("c:PtnppCnRon") default(0)
  PtnppCnRon annotate("description", "Nuclear kinase-bound PER2pp-CRY1")
  PtnppCnRon annotate("units", "nM")

  val PoppRt = Species("c:PoppRt") default(0.0077)
  PoppRt annotate("description", "Cytoplasmic PER1pp-CRY2")
  PoppRt annotate("units", "nM")

  val PtppRt = Species("c:PtppRt") default(0)
  PtppRt annotate("description", "Cytoplasmic PER2pp-CRY2")
  PtppRt annotate("units", "nM")

  val PoppCRt = Species("c:PoppCRt") default(0.005)
  PoppCRt annotate("description", "Cytoplasmic PER1pp-CRY2")
  PoppCRt annotate("units", "nM")

  val PtppCRt = Species("c:PtppCRt") default(0)
  PtppCRt annotate("description", "Cytoplasmic PER2pp-CRY2")
  PtppCRt annotate("units", "nM")

  val PonppRtn = Species("c:PonppRtn") default(0.00031)
  PonppRtn annotate("description", "Nuclear PER1pp-CRY2")
  PonppRtn annotate("units", "nM")

  val PtnppRtn = Species("c:PtnppRtn") default(0)
  PtnppRtn annotate("description", "Nuclear PER2pp-CRY2")
  PtnppRtn annotate("units", "nM")

  val PonppCnRtn = Species("c:PonppCnRtn") default(0.001)
  PonppCnRtn annotate("description", "Nuclear kinase-bound PER1pp-CRY2")
  PonppCnRtn annotate("units", "nM")

  val PtnppCnRtn = Species("c:PtnppCnRtn") default(0)
  PtnppCnRtn annotate("description", "Nuclear kinase-bound PER2pp-CRY2")
  PtnppCnRtn annotate("units", "nM")


  /* Derived quantities */

  val C = Species("c:C") default(0)
  /*C annotate("description", "Total unbound cytoplasmic kinase")
   *C annotate("units", "nM")
   *calc(C) := Ct - (PoC + PtC + PopC + PtpC + PoppC + PtppC + PopCRo + PopCRt + PtpCRo + PtpCRt + PoppCRo + PoppCRt + PtppCRo + PtppCRt + PonpCn + PtnpCn + PonppCn + PtnppCn + PonpCnRon + PonpCnRtn + PtnpCnRon + PtnpCnRtn + PonppCnRon + PonppCnRtn + PtnppCnRon + PtnppCnRtn + Cn)
   */

  reactions(

    McPo --> McPo + Po at tlp,
    McPt --> McPt + Pt at tlp,
    McRo --> McRo + Ro at tlr,
    McRt --> McRt + Rt at tlr,

    Po + C --> PoC at ac,
    Pt + C --> PtC at ac,
    Pop + C --> PopC at ac,
    Ptp + C --> PtpC at ac,
    Popp + C --> PoppC at ac,
    Ptpp + C --> PtppC at ac,
    PopRo + C --> PopCRo at ac,
    PtpRo + C --> PtpCRo at ac,
    PopRt + C --> PopCRt at ac,
    PtpRt + C --> PtpCRt at ac,
    PoppRo + C --> PoppCRo at ac,
    PtppRo + C --> PtppCRo at ac,
    PoppRt + C --> PoppCRt at ac,
    PtppRt + C --> PtppCRt at ac,
    Ponp + Cn --> PonpCn at (Nf * ac),
    Ptnp + Cn --> PtnpCn at (Nf.value * ac.value),
    Ponpp + Cn --> PonppCn at (Nf * ac),
    Ptnpp + Cn --> PtnppCn at (Nf * ac),
    PonpRon + Cn --> PonpCnRon at (Nf * ac),
    PtnpRon + Cn --> PtnpCnRon at (Nf * ac),
    PonpRtn + Cn --> PonpCnRtn at (Nf * ac),
    PtnpRtn + Cn --> PtnpCnRtn at (Nf * ac),
    PonppRon + Cn --> PonppCnRon at (Nf * ac),
    PtnppRon + Cn --> PtnppCnRon at (Nf * ac),
    PonppRtn + Cn --> PonppCnRtn at (Nf * ac),
    PtnppRtn + Cn --> PtnppCnRtn at (Nf * ac),

    PoC --> Po + C at dc,
    PtC --> Pt + C at dc,
    PopC --> Pop + C at dc,
    PtpC --> Ptp + C at dc,
    PoppC --> Popp + C at dc,
    PtppC --> Ptpp + C at dc,
    PopCRo --> PopRo + C at dc,
    PtpCRo --> PtpRo + C at dc,
    PopCRt --> PopRt + C at dc,
    PtpCRt --> PtpRt + C at dc,
    PoppCRo --> PoppRo + C at dc,
    PoppCRt --> PoppRt + C at dc,
    PtppCRo --> PtppRo + C at dc,
    PtppCRt --> PtppRt + C at dc,
    PonpCn --> Ponp + Cn at dc,
    PtnpCn --> Ptnp + Cn at dc,
    PonppCn --> Ponpp + Cn at dc,
    PtnppCn --> Ptnpp + Cn at dc,
    PonpCnRon --> PonpRon + Cn at dc,
    PtnpCnRon --> PtnpRon + Cn at dc,
    PonpCnRtn --> PonpRtn + Cn at dc,
    PtnpCnRtn --> PtnpRtn + Cn at dc,
    PonppCnRon --> PonppRon + Cn at dc,
    PtnppCnRon --> PtnppRon + Cn at dc,
    PonppCnRtn --> PonppRtn + Cn at dc,
    PtnppCnRtn --> PtnppRtn + Cn at dc,

    Pop + Ro --> PopRo at ar,
    PopC + Ro --> PopCRo at ar,
    PoppC + Ro --> PoppCRo at ar,
    Popp + Ro --> PoppRo at ar,
    Ptp + Ro --> PtpRo at ar,
    PtpC + Ro --> PtpCRo at ar,
    PtppC + Ro --> PtppCRo at ar,
    Ptpp + Ro --> PtppRo at ar,
    Pop + Rt --> PopRt at ar,
    PopC + Rt --> PopCRt at ar,
    PoppC + Rt --> PoppCRt at ar,
    Popp + Rt --> PoppRt at ar,
    Ptp at Rt --> PtpRt at ar,
    PtpC + Rt --> PtpCRt at ar,
    PtppC + Rt --> PtppCRt at ar,
    Ptpp + Rt --> PtppRt at ar,
    Ponp + Ron --> PonpRon at (Nf * ar),
    PonpCn + Ron --> PonpCnRon at (Nf * ar),
    PonppCn + Ron --> PonppCnRon at (Nf * ar),
    Ponpp + Ron --> PonppRon at (Nf * ar),
    Ptnp + Ron --> PtnpRon at (Nf * ar),
    PtnpCn + Ron --> PtnpCnRon at (Nf * ar),
    PtnppCn + Ron --> PtnppCnRon at (Nf * ar),
    Ptnpp + Ron --> PtnppRon at (Nf * ar),
    Ponp + Rtn --> PonpRtn at (Nf * ar),
    PonpCn + Rtn --> PonpCnRtn at (Nf * ar),
    PonppCn + Rtn --> PonppCnRtn at (Nf * ar),
    Ponpp + Rtn --> PonppRtn at (Nf * ar),
    Ptnp + Rtn --> PtnpRtn at (Nf * ar),
    PtnpCn + Rtn --> PtnpCnRtn at (Nf * ar),
    PtnppCn + Rtn --> PtnppCnRtn at (Nf * ar),
    Ptnpp + Rtn --> PtnppRtn at (Nf * ar),

    PopRo --> Pop + Ro at dr,
    PopCRo --> PopC + Ro at dr,
    PoppCRo --> PoppC + Ro at dr,
    PoppRo --> Popp + Ro at dr,
    PtpRo --> Ptp + Ro at dr,
    PtpCRo --> PtpC + Ro at dr,
    PtppCRo --> PtppC + Ro at dr,
    PtppRo --> Ptpp + Ro at dr,
    PopRt --> Pop + Rt at dr,
    PopCRt --> PopC + Rt at dr,
    PoppCRt --> PoppC + Rt at dr,
    PoppRt --> Popp + Rt at dr,
    PtpRt --> Ptp + Rt at dr,
    PtpCRt --> PtpC + Rt at dr,
    PtppCRt --> PtppC + Rt at dr,
    PtppRt --> Ptpp + Rt at dr,
    PonpRon --> Ponp + Ron at dr,
    PonpCnRon --> PonpCn + Ron at dr,
    PonppCnRon --> PonppCn + Ron at dr,
    PonppRon --> Ponpp + Ron at dr,
    PtnpRon --> Ptnp + Ron at dr,
    PtnpCnRon --> PtnpCn + Ron at dr,
    PtnppCnRon --> PtnppCn + Ron at dr,
    PtnppRon --> Ptnpp + Ron at dr,
    PonpRtn --> Ponp + Rtn at dr,
    PonpCnRtn --> PonpCn + Rtn at dr,
    PonppCnRtn --> PonppCn + Rtn at dr,
    PonppRtn --> Ponpp + Rtn at dr,
    PtnpRtn --> Ptnp + Rtn at dr,
    PtnpCnRtn --> PtnpCn + Rtn at dr,
    PtnppCnRtn --> PtnppCn + Rtn at dr,
    PtnppRtn --> Ptnpp + Rtn at dr,

    PoC --> PopC at hoo,
    PtC --> PtpC at hot,
    PopC --> PoppC at hto,
    PtpC --> PtppC at htt,
    PopCRo --> PoppCRo at hto,
    PtpCRo --> PtppCRo at htt,
    PopCRt --> PoppCRt at hto,
    PtpCRt --> PtppCRt at htt,
    PonpCn --> PonppCn at hto,
    PtnpCn --> PtnppCn at htt,
    PonpCnRon --> PonppCnRon at hto,
    PtnpCnRon --> PtnppCnRon at htt,
    PonpCnRtn --> PonppCnRtn at hto,
    PtnpCnRtn --> PtnppCnRtn at htt,

    Pop --> Ponp at nl,
    PopC --> PonpCn at nl,
    PopRo --> PonpRon at nl,
    PopRt --> PonpRtn at nl,
    PopCRo --> PonpCnRon at nl,
    PopCRt --> PonpCnRtn at nl,
    Ptp --> Ptnp at nl,
    PtpC --> PtnpCn at nl,
    PtpRo --> PtnpRon at nl,
    PtpRt --> PtnpRtn at nl,
    PtpCRo --> PtnpCnRon at nl,
    PtpCRt --> PtnpCnRtn at nl,

    Ponp --> Pop at ne,
    PonpCn --> PopC at ne,
    PonpRon --> PopRo at ne,
    PonpRtn --> PopRt at ne,
    PonpCnRon --> PopCRo at ne,
    PonpCnRtn --> PopCRt at ne,
    Ponpp --> Popp at ne,
    PonppCn --> PoppC at ne,
    PonppRon --> PoppRo at ne,
    PonppRtn --> PoppRt at ne,
    PonppCnRon --> PoppCRo at ne,
    PonppCnRtn --> PoppCRt at ne,
    Ptnp --> Ptp at ne,
    PtnpCn --> PtpC at ne,
    PtnpRon --> PtpRo at ne,
    PtnpRtn --> PtpRt at ne,
    PtnpCnRon --> PtpCRo at ne,
    PtnpCnRtn --> PtpCRt at ne,
    Ptnpp --> Ptpp at ne,
    PtnppCn --> PtppC at ne,
    PtnppRon --> PtppRo at ne,
    PtnppRtn --> PtppRt at ne,
    PtnppCnRon --> PtppCRo at ne,
    PtnppCnRtn --> PtppCRt at ne,

    Po --> () at upu,
    PoC --> C at upu,
    Pt --> () at upu,
    PtC --> C at upu,
    Ro --> () at uro,
    Rt --> () at urt,
    Ron --> () at uro,
    Rtn --> () at urt,

    Pop --> () at up,
    PopC --> C at up,
    Popp --> () at up,
    PoppC --> C at up,
    Ponp --> () at up,
    PonpCn --> Cn at up,
    Ponpp --> () at up,
    PonppCn --> Cn at up,
    Ptp --> () at up,
    PtpC --> C at up,
    Ptpp --> () at up,
    PtppC --> C at up,
    Ptnp --> () at up,
    PtnpCn --> Cn at up,
    Ptnpp --> () at up,
    PtnppCn --> Cn at up
  )

}


class MCCCalc extends VarCalc{

  /* Derived quantities */

  val C = Species("c:C") default(0)
  C annotate("description", "Total unbound cytoplasmic kinase")
  C annotate("units", "nM")
  calc(C) := Ct - (PoC + PtC + PopC + PtpC + PoppC + PtppC + PopCRo + PopCRt + PtpCRo + PtpCRt + PoppCRo + PoppCRt + PtppCRo + PtppCRt + PonpCn + PtnpCn + PonppCn + PtnppCn + PonpCnRon + PonpCnRtn + PtnpCnRon + PtnpCnRtn + PonppCnRon + PonppCnRtn + PtnppCnRon + PtnppCnRtn + Cn)

}
