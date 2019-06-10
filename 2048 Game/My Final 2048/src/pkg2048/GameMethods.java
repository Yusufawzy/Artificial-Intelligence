/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pkg2048;

import java.awt.Color;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.jpl7.Query;
import org.jpl7.Term;

/**
 *
 * @author Chaos Ruler
 */
public class GameMethods extends Thread {

    public void paintscreen() throws InterruptedException {
        String t1 = "consult('F:\\My Apps\\Done\\Artificial-Intelligence\\2048 Game\\My Final 2048\\gui\\src\\pkg2048\\2048_test_return.pl')";
        Query q1 = new Query(t1);
        System.out.println(q1.hasSolution() ? "works" : "fails");

       // String t2 = "play1(B)";
        String t2 = "start(B)";

        Query q2 = new Query(t2);
        Term[] results = q2.oneSolution().get("B").toTermArray();

        int n = results.length;
        int i = 0;
        for (int j = 0; j < n; j++) {
            //set the text to the button of whatever it's the value of the arrayLisst
            if (!results[j].toString().equals("0")) {
                NewJFrame.allbuttons.get(i).setText(results[j].toString());
            } else {
                NewJFrame.allbuttons.get(i).setText("");
            }

            if (NewJFrame.allbuttons.get(i).getText().equals("2")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.YELLOW);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("4")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.ORANGE);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("8")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.red);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("16")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.BLUE);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("32")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.GREEN);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("64")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.GRAY);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("128")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.CYAN);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("265")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.DARK_GRAY);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("512")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.LIGHT_GRAY);
            } else if (NewJFrame.allbuttons.get(i).getText().equals("1024")) {
                NewJFrame.allbuttons.get(i).setBackground(Color.BLACK);
                NewJFrame.allbuttons.get(i).setForeground(Color.white);
            } else {
                NewJFrame.allbuttons.get(i).setBackground(Color.GRAY);
            }
            i++;
            if(i%16==0){
                i=0;
        }
        Thread.sleep(10);

    }

}

@Override
        public void run(){
        try {
            paintscreen();
        

} catch (InterruptedException ex) {
            Logger.getLogger(GameMethods.class
.getName()).log(Level.SEVERE, null, ex);
        }
        
    }
         
}
