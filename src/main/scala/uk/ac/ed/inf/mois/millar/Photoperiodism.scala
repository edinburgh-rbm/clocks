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

import uk.ac.ed.inf.mois.{Model, ODE, Process, ProcessGroup, VarCalc, Math}
import uk.ac.ed.inf.mois.sched.NaiveScheduler
import spire.implicits._
import uk.ac.ed.inf.mois.implicits._


class Photoperiodism extends ODE {

  val DaylightHours = Double("c:DaylightHours")
  DaylightHours annotate("description", "Daylight hours")
  DaylightHours annotate("units", "h")

  val v1 = Double("c:v1") default(2.4514) param()
  v1 annotate("description", "Coupling constant of light activation of LHY transcription")
  v1 annotate("units", "1/h")

  val v2 = Double("c:v2") default(5.1694) param()
  v2 annotate("description", "Maximum rate of light-independent LHY transcription")
  v2 annotate("units", "nM/h")

  val v3 = Double("c:v3") default(0.6473) param()
  v3 annotate("description", "Constant of activation by protein X")
  v3 annotate("units", "nM")

  val v4 = Double("c:v4") default(1.5283) param()
  v4 annotate("description", "Maximum rate of LHY mRNA degradation")
  v4 annotate("units", "nM/h")

  val v5 = Double("c:v5") default(1.8170) param()
  v5 annotate("description", "Michaelis constant of LHY mRNA degradation")
  v5 annotate("units", "nM")

  val v6 = Double("c:v6") default(0.8295) param()
  v6 annotate("description", "Rate constant of LHY mRNA translation")
  v6 annotate("units", "1/h")

  val v7 = Double("c:v7") default(16.8363) param()
  v7 annotate("description", "Rate constant of LHY transport into the nucleus")
  v7 annotate("units", "1/h")

  val v8 = Double("c:v8") default(0.1687) param()
  v8 annotate("description", "Rate constant of LHY transport out of the nucleus")
  v8 annotate("units", "1/h")

  val v9 = Double("c:v9") default(20.4400) param()
  v9 annotate("description", "Maximum rate of cytoplasmic LHY degradation")
  v9 annotate("units", "nM/h")

  val v10 = Double("c:V10") default(1.5644) param()
  v10 annotate("description", "Michaelis constant of cytoplasmic LHY degradation")
  v10 annotate("units", "nM")

  val v11 = Double("c:v11") default(3.6888) param()
  v11 annotate("description", "Maximum rate of nuclear LHY degradation")
  v11 annotate("units", "nM/h")

  val v12 = Double("c:v12") default(1.2765) param()
  v12 annotate("description", "Michaelis constant of nuclear LHY degradation")
  v12 annotate("units", "nM")

  val v13 = Double("c:v13") default(1.3956) param()
  v13 annotate("description", "Maximum activation by protein Y")
  v13 annotate("units", "nM/h")

  val v14 = Double("c:v14") default(0.0338) param()
  v14 annotate("description", "Constant of activation by protein Y")
  v14 annotate("units", "nM")

  val v15 = Double("c:v15") default(0.5539) param()
  v15 annotate("description", "Maximum rate of TOC1 transcription")
  v15 annotate("units", "1/nM")

  val v16 = Double("c:v16") default(0.2569) param()
  v16 annotate("description", "Constant of repression by LHY")
  v16 annotate("units", "nM")

  val v17 = Double("c:v17") default(3.8231) param()
  v17 annotate("description", "Maximum rate of TOC1 mRNA degradation")
  v17 annotate("units", "nM/h")

  val v18 = Double("c:v18") default(2.5734) param()
  v18 annotate("description", "Michalis constant of TOC1 mRNA degradation")
  v18 annotate("units", "nM")

  val v19 = Double("c:v19") default(4.3240) param()
  v19 annotate("description", "Rate constant of TOC1 mRNA translation")
  v19 annotate("units", "1/h")

  val v20 = Double("c:v20") default(0.3166) param()
  v20 annotate("description", "Rate constant of TOC1 movement into the nucleus")
  v20 annotate("units", "1/h")

  val v21 = Double("c:v21") default(2.1509) param()
  v21 annotate("description", "Rate constant of TOC1 movement out of the nucleus")
  v21 annotate("units", "1/h")

  val v22 = Double("c:v22") default(0.0013) param()
  v22 annotate("description", "Maximum rate of light-dependent cytoplasmic TOC1 degradation")
  v22 annotate("units", "nM/h")

  val v23 = Double("c:v23") default(3.1741) param()
  v23 annotate("description", "Maximum rate of light-independent cytoplasmic TOC1 degradation")
  v23 annotate("units", "nM/h")

  val v24 = Double("c:v24") default(2.7454) param()
  v24 annotate("description", "Michaelis constant of cytoplasmic TOC1 degradation")
  v24 annotate("units", "nM")

  val v25 = Double("c:v25") default(0.0492) param()
  v25 annotate("description", "Maximum rate of light-dependent nuclear TOC1 degradation")
  v25 annotate("units", "nM/h")

  val v26 = Double("c:v26") default(4.0424) param()
  v26 annotate("description", "Maximum rate of light-independent nuclear TOC1 degradation")
  v26 annotate("units", "nM/h")

  val v27 = Double("c:v27") default(0.4033) param()
  v27 annotate("description", "Michaelis constant of nuclear TOC1 degradation")
  v27 annotate("units", "nM")

  val v28 = Double("c:v28") default(0.2431) param()
  v28 annotate("description", "Maximum transcription rate of protein X")
  v28 annotate("units", "nM/h")

  val v29 = Double("c:v29") default(0.4099) param()
  v29 annotate("description", "Constant of activation by TOC1 protein")
  v29 annotate("units", "nM")

  val v30 = Double("c:v30") default(10.1132) param()
  v30 annotate("description", "Maximum rate of degradation of X mRNA")
  v30 annotate("units", "nM/h")

  val v31 = Double("c:v31") default(6.5585) param()
  v31 annotate("description", "Michaelis constant of X mRNA degradation")
  v31 annotate("units", "nM")

  val v32 = Double("c:v32") default(2.1470) param()
  v32 annotate("description", "Rate constant of X mRNA translation")
  v32 annotate("units", "1/h")

  val v33 = Double("c:v33") default(1.0352) param()
  v33 annotate("description", "Rate constant of protein X movement into the nucleus")
  v33 annotate("units", "1/h")

  val v34 = Double("c:v34") default(3.3017) param()
  v34 annotate("description", "Rate constant of protein X movement out of the nucleus")
  v34 annotate("units", "1/h")

  val v35 = Double("c:v35") default(0.2179) param()
  v35 annotate("description", "Maximum rate of degradation of cytoplasmic protein X")
  v35 annotate("units", "nM/h")

  val v36 = Double("c:v36") default(0.6632) param()
  v36 annotate("description", "Michaelis constant of cytoplasmic protein X degradation")
  v36 annotate("units", "nM")

  val v37 = Double("c:v37") default(3.3442) param()
  v37 annotate("description", "Maximum rate of degradation of nuclear protein X")
  v37 annotate("units", "nM/h")

  val v38 = Double("c:v38") default(17.111) param()
  v38 annotate("description", "Michaelis constant of nuclear protien X degradation")
  v38 annotate("units", "nM")

  val v39 = Double("c:v39") default(1.8272) param()
  v39 annotate("description", "Light-dependent component of Y transcription")
  v39 annotate("units", "nM/h")

  val v40 = Double("c:v40") default(3.5159) param()
  v40 annotate("description", "Light-independent component of Y transcription")
  v40 annotate("units", "nM/h")

  val v41 = Double("c:v41") default(1.8056) param()
  v41 annotate("description", "Constant of repression by TOC1")
  v41 annotate("units", "nM")

  val v42 = Double("c:v42") default(4.2970) param()
  v42 annotate("description", "Maximum rate of degradation of Y mRNA")
  v42 annotate("units", "nM/h")

  val v43 = Double("c:v43") default(1.7303) param()
  v43 annotate("description", "Michaelis constant of Y mRNA degradation")
  v43 annotate("units", "nM")

  val v44 = Double("c:v44") default(0.2485) param()
  v44 annotate("description", "Rate constant of Y mRNA degradation")
  v44 annotate("units", "nM")

  val v45 = Double("c:v45") default(2.2123) param()
  v45 annotate("description", "Rate constant of protein Y movement into the nucleus")
  v45 annotate("units", "1/h")

  val v46 = Double("c:v46") default(0.2002) param()
  v46 annotate("description", "Rate constant of protein Y movement out of the nucleus")
  v46 annotate("units", "1/h")

  val v47 = Double("c:v47") default(0.1347) param()
  v47 annotate("description", "Maximum rate of degradation of cytoplasmic protein Y")
  v47 annotate("units", "nM/h")

  val v48 = Double("c:v48") default(1.8258) param()
  v48 annotate("description", "Michaelis constant of cytoplasmic protein Y degradation")
  v48 annotate("units", "nM")

  val v49 = Double("c:v49") default(0.6114) param()
  v49 annotate("description", "Maximum rate of degradation of nuclear protein Y")
  v49 annotate("units", "nM/h")

  val v50 = Double("c:v50") default(1.8066) param()
  v50 annotate("description", "Michaelis constant of nuclear protein Y degradation")
  v50 annotate("units", "nM")

  val v51 = Double("c:v51") default(0.0051) param()
  v51 annotate("description", "Maximum rate of Y transcription")
  v51 annotate("units", "nM/h")

  val v52 = Double("c:v52") default(0.0604) param()
  v52 annotate("description", "Constant of repression by LHY")
  v52 annotate("units", "nM")

  val v53 = Double("c:v53") default(28.3562) param()
  v53 annotate("description", "Coupling constant of light activation of Y mRNA transcription")
  v53 annotate("units", "1/h")

  val u1 = Double("c:u1") default(3.3064) param()
  u1 annotate("description", "Hill coefficient of activation by protein X")

  val u2 = Double("c:u2") default(1.0258) param()
  u2 annotate("description", "Hill coefficient of activation by protein Y")

  val u3 = Double("c:u3") default(1.4422) param()
  u3 annotate("description", "Hill coefficient of activation by TOC1")

  val u4 = Double("c:u4") default(3.6064) param()
  u4 annotate("description", "Hill coefficient of repression by TOC1")

  val u5 = Double("c:u5") default(1.0237) param()
  u5 annotate("description", "Hill coefficient of repression by LHY")

  val v_COm = Double("c:v_COm") default(1.1452) param()
  v_COm annotate("description", "Rate constant of CO (= TOC1) mRNA translation")
  v_COm annotate("units", "1/h")

  val v_COp = Double("c:v_COp") default(9.2242) param()
  v_COp annotate("description", "Maximum rate of light-dependent CO protein degradation")
  v_COp annotate("units", "nM/h")

  val k_COp = Double("c:k_COp") default(2.0976) param()
  k_COp annotate("description", "Michaelis constant of CO protein degradation")
  k_COp annotate("units", "nM")

  val V_CO = Double("c:V_CO") default(0.5800) param()
  V_CO annotate("description", "Maximum rate of FT activation by CO")
  V_CO annotate("units", "nM/h")

  val K_CO = Double("c:K_CO") default(7.3533) param()
  K_CO annotate("description", "Michaelis constant of FT activation by CO")
  K_CO annotate("units", "nM")

  val v_FT = Double("c:v_FT") default(1.8974) param()
  v_FT annotate("description", "Maximum rate of FT mRNA degradation")
  v_FT annotate("units", "nM/h")

  val k_FT = Double("c:k_FT") default(5.3925) param()
  k_FT annotate("description", "Michaelis constant of FT mRNA degradation")
  k_FT annotate("units", "nM")

  val B_CO = Double("c:B_CO") default(0) param()
  B_CO annotate("description", "Basal rate of FT transcription")
  B_CO annotate("units", "nM/h")


  val P = Double("c:P")
  val LHY = Double("c:LHY")
  val LHY_c = Double("c:LHY_c")
  val LHY_n = Double("c:LHY_n")
  val TOC1 = Double("c:TOC1")
  val TOC1_c = Double("c:TOC1_c")
  val TOC1_n = Double("c:TOC1_n")
  val X = Double("c:X")
  val X_c = Double("c:X_c")
  val X_n = Double("c:X_n")
  val Y = Double("c:Y")
  val Y_c = Double("c:Y_c")
  val Y_n = Double("c:Y_n")
  val CO = Double("c:CO")
  val FT = Double("c:FT")

  /* Assume 12-hour photoperiod, with sunrise at t=0 and sunset at t=12.
   * Again, need easier way of accessing simulation time...
   */
  val Theta_light = Double("c:Theta_light")
  calc(Theta_light) := (1/4) * (1 + tanh(6 * (t - 0))) * (1 - tanh(6 * (t - DaylightHours)))
  

  d(P) := (0.5 * (1 - Theta_light)) - (P * Theta_light) - ((1.2 * P) / (1.2 + P))

  d(LHY) := (Theta_light * v1 * P) + ((v2 * (X_c ** u1)) / (v3 + (X_c ** u1))) - ((v4 * LHY) / (v5 + LHY))
  d(LHY_c) := (v6 * LHY) - (v7 * LHY_c) + (v8 * LHY_n) - ((v9 * LHY_c) / (v10 + LHY_c))
  d(LHY_n) := (v7 * LHY_c) - (v8 * LHY_n) - ((v11 * LHY_n) / (v12 + LHY_n))

  d(TOC1) := (((v13 * (Y_n ** u2)) / (v14 + (Y_n ** u2))) * (v15 / (v16 + (Y_n ** u2)))) - ((v17 * TOC1) / (v18 + TOC1))
  d(TOC1_c) := (v19 * TOC1) - (v20 * TOC1_c) + (v21 * TOC1_n) - ((((v22 * (1 - Theta_light)) + v23) * TOC1_c) / (v24 + TOC1_c))
  d(TOC1_n) := (v20 * TOC1_c) - (v21 * TOC1_n) - ((((v25 * (1 - Theta_light)) + v26) * TOC1_n) / (v27 + TOC1_n))

  d(X) := ((v28 * (TOC1_n ** u3)) / (v29 + (TOC1_n ** u3))) - ((v30 * X) / (v31 + X))
  d(X_c) := (v32 * X) - (v33 * X_c) + (v34 * X_n) - ((v35 * X_c) / (v36 + X_c))
  d(X_n) := (v33 * X_c) - (v34 * X_n) - ((v37 * X_n) / (v38 +  X_n))

  d(Y) := ((Theta_light * v53 * P + (((Theta_light * v39) + v40) / (v41 + (TOC1_n ** u4)))) * (v51 / ((LHY_n ** u5) + v52))) - ((v42 * Y) / (v43 + Y))
  d(Y_c) := (v44 * Y) - (v45 * Y_c) + (v46 * Y_n) - ((v47 * Y_c) / (v48 + Y_c))
  d(Y_n) := (v45 * Y_c) - (v45 * Y_n) - ((v49 * Y_n) / (v50 + Y_n))

  d(CO) := (V_COm * TOC1_n) - (((1 - Theta_light) * V_COp * CO) / (k_COp + CO))
  d(FT) := B_CO + ((V_CO * CO) / (K_CO + CO)) - ((V_FT * FT) / (k_FT + FT))


  /* This value needs to keep track of the total amount of FT accumulated over the previous 24 hours from the hour at hand.
   * How?
   */
  val FTarea = Double("c:FTarea")

}
