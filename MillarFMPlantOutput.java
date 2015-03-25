import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.awt.*;
import java.awt.geom.QuadCurve2D;
import java.awt.image.BufferedImage;

import javax.imageio.ImageIO;
 
public class MillarFMPlantOutput {
 
  public static void main(String[] args) {
 	MillarFMPlantOutput obj = new MillarFMPlantOutput();
	obj.run();
   }
 
  public void run() {
	
	/** Arguments. **/
	//String input = "/Users/John/Desktop/Research/Projects/2014 Cell Models/clocks/planttest.out";
	//String input = "/Users/John/Desktop/Research/Projects/2014 Cell Models/clocks/planttest2.out";
	String input = "/Users/John/Desktop/Research/Projects/2014 Cell Models/clocks/plant.out";
	String output = "/Users/John/Desktop/Research/Projects/2014 Cell Models/clocks/vis/";
	
	/** Parsing constructs for input file. */
	BufferedReader br = null;
	String line = "";
	String cvsSplitBy = " ";
	
	int step = 2;
	int hour = 0;
	
	/** Database structures for leaf coordinates */
	int width = 500;
	int height = 500;
	int x0 = width / 2;
	int y0 = height / 2;
	ArrayList<Double> x1 = new ArrayList<Double>();
	ArrayList<Double> y1 = new ArrayList<Double>();
	ArrayList<Double> x2 = new ArrayList<Double>();
	ArrayList<Double> y2 = new ArrayList<Double>();
	ArrayList<Double> x3 = new ArrayList<Double>();
	ArrayList<Double> y3 = new ArrayList<Double>();
	ArrayList<Double> x4 = new ArrayList<Double>();
	ArrayList<Double> y4 = new ArrayList<Double>();
	ArrayList<Double> x5 = new ArrayList<Double>();
	ArrayList<Double> y5 = new ArrayList<Double>();
	
	try {
		/** For reading in the input data */
		br = new BufferedReader(new FileReader(input));
		
		/** For each timepoint, read in the leaf profiles */
		while ((line = br.readLine()) != null) {
			String[] data = line.split(cvsSplitBy);
			if (step == 0) {
				System.out.println("Hour " + hour + ":");
				for (int x=0; x<data.length; x++) {
					System.out.println(data[x]);
					if (data.length > 1) {
					  String[] leaf = data[x].split(",");
					  int leafnumber = Integer.parseInt(leaf[0].substring(1));
					  double leafage = Double.parseDouble(leaf[1]);
					  double leafweight = Double.parseDouble(leaf[2]);
					  double leafangle = Double.parseDouble(leaf[3]);
					  double leafarea = Double.parseDouble(leaf[4].substring(0, leaf[4].length()-1));
					  //System.out.println("(" + leafnumber + "," + leafage + "," + leafweight + "," + leafangle + "," + leafarea + ")");
					  /** Parse the data for the leaf coordinates. Ignore angle for now. */
					  double offset;
					  if (leafnumber == 0) { offset = 90.0; }
					  else if (leafnumber == 1) { offset = 270.0; }
					  else if (leafnumber == 2) { offset = 180.0; }
					  else if (leafnumber == 3) { offset = 0.0; }
					  else { offset = (((1800 * (leafnumber-3)) / 13) % 360.0); }
					  double leaflength = 2 * (Math.sqrt(leafarea));
					  double leafwidth = 0.5 * (Math.sqrt(leafarea));
					  x1.add(x0 + (10 * (Math.sin(Math.toRadians(offset)))));
					  y1.add(y0 - (10 * (Math.cos(Math.toRadians(offset)))));
					  x2.add(x0 + ((10 + leaflength) * (Math.sin(Math.toRadians(offset)))));
					  y2.add(y0 - ((10 + leaflength) * (Math.cos(Math.toRadians(offset)))));
					  x3.add(x0 + ((10 + (leaflength/3)) * (Math.sin(Math.toRadians(offset)))));
					  y3.add(y0 - ((10 + (leaflength/3)) * (Math.cos(Math.toRadians(offset)))));
					  x4.add(x3.get(leafnumber) + (leafwidth * (Math.sin(Math.toRadians(offset + 90.0)))));
					  y4.add(y3.get(leafnumber) - (leafwidth * (Math.cos(Math.toRadians(offset + 90.0)))));
					  x5.add(x3.get(leafnumber) + (leafwidth * (Math.sin(Math.toRadians(offset - 90.0)))));
					  y5.add(y3.get(leafnumber) - (leafwidth * (Math.cos(Math.toRadians(offset - 90.0)))));
					}
				}
				
				/** Output the graphical image */
				BufferedImage bi = new BufferedImage(width,height,BufferedImage.TYPE_INT_ARGB);
				Graphics2D ig2 = bi.createGraphics();
				ig2.setColor(Color.black);
				ig2.fillRect(0,  0,  bi.getWidth(), bi.getHeight());
				
				for(int y=0; y<x1.size(); y++) {
					ig2.setPaint(Color.green);
					ig2.drawLine(x0, y0, x1.get(y).intValue(), y1.get(y).intValue());
					ig2.setPaint(Color.red);
					ig2.drawLine(x1.get(y).intValue(), y1.get(y).intValue(), x2.get(y).intValue(), y2.get(y).intValue());
//					ig2.setPaint(Color.blue);
//					ig2.drawLine(x3.get(y).intValue(), y3.get(y).intValue(), x4.get(y).intValue(), y4.get(y).intValue());
//					ig2.drawLine(x3.get(y).intValue(), y3.get(y).intValue(), x5.get(y).intValue(), y5.get(y).intValue());
					ig2.setPaint(Color.green);
					QuadCurve2D q1 = new QuadCurve2D.Float();
					q1.setCurve(x1.get(y), y1.get(y), x4.get(y), y4.get(y), x2.get(y), y2.get(y));
					ig2.draw(q1);
					QuadCurve2D q2 = new QuadCurve2D.Float();
					q2.setCurve(x1.get(y), y1.get(y), x5.get(y), y5.get(y), x2.get(y), y2.get(y));
					ig2.draw(q2);
				}
				
				/** Label it with the hour*/
				String hourRep = Integer.toString(hour);
				if (hour < 10) { hourRep = "00" + Integer.toString(hour); }
				else if (hour < 100) { hourRep = "0" + Integer.toString(hour); }
				Font font = new Font("TimesRoman", Font.BOLD, 20);
				ig2.setFont(font);
				FontMetrics fontMetrics = ig2.getFontMetrics();
				int stringWidth = fontMetrics.stringWidth(hourRep);
				int stringHeight = fontMetrics.getAscent();
				ig2.setPaint(Color.white);
				ig2.drawString(hourRep, (width-stringWidth-5), (height-stringHeight-5));
				
				ImageIO.write(bi,"PNG", new File(output+"plantvis"+hourRep+".png"));
				//ImageIO.write(bi,"JPG", new File(output+"plantvis"+hourRep+".jpg"));
			}
			
			/** Housekeeping. */
			step++;
			if (step == 10) {
				step = 0;
				hour++;
			}
			x1.clear();
			y1.clear();
			x2.clear();
			y2.clear();
			x3.clear();
			y3.clear();
			x4.clear();
			y4.clear();
			x5.clear();
			y5.clear();
 		}
	} catch (FileNotFoundException e) {
		e.printStackTrace();
	} catch (IOException e) {
		e.printStackTrace();
	} finally {
		if (br != null) {
			try {
				br.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	/** Output printing. */
//	System.out.println("***");
	System.out.println("Analysis complete!");
	
  }
 
}