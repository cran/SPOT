package spotGui;

import java.awt.event.KeyEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.Event;
import java.awt.BorderLayout;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.KeyStroke;
import java.awt.Point;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JMenuItem;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JFrame;
import javax.swing.JDialog;
import javax.swing.JTextField;
import java.awt.Rectangle;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import java.awt.Dimension;
import java.io.File;
import java.awt.Toolkit;

public class tarFuncGenerator {

	private JFrame jFrame = null;  //  @jve:decl-index=0:visual-constraint="10,10"
	private JPanel jContentPane = null;
	private JMenuBar jJMenuBar = null;
	private JMenu fileMenu = null;
	private JMenu helpMenu = null;
	private JMenuItem exitMenuItem = null;
	private JMenuItem aboutMenuItem = null;
	private JDialog aboutDialog = null;  //  @jve:decl-index=0:visual-constraint="11,196"
	private JPanel aboutContentPane = null;
	private JLabel aboutVersionLabel = null;
	private JTextField functionFormulaTextField = null;
	private JLabel functionLabel = null;
	private JCheckBox noiseCheckBox = null;
	private JTextField noiseTextField = null;
	private JLabel noiseLabel = null;
	private JFileChooser functionFileChooser = null;  //  @jve:decl-index=0:visual-constraint="527,86"
	private File pathDefaultFile = new File("");  //  @jve:decl-index=0:
	private FileFilter fileFilterR = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			return f.getName().toLowerCase().endsWith(".r");
		}
		public String getDescription () { return "R Files(*.r)"; }  
	};
	
	private FileFilter fileFilterAll = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			return !f.getName().isEmpty();
		}
		public String getDescription () { return "All files (*.*)"; }
	};
	private JMenuItem saveMenuItem = null;
	/**
	 * This method initializes jFrame
	 * 
	 * @return javax.swing.JFrame
	 */
	private JFrame getJFrame() {
		if (jFrame == null) {
			jFrame = new JFrame();
			jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
			jFrame.setResizable(false);
			jFrame.setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("/spotGui/icon_tab_algorithm.png")));
			jFrame.setJMenuBar(getJJMenuBar());
			jFrame.setSize(480, 173);
			jFrame.setContentPane(getJContentPane());
			jFrame.setTitle("SPOT GUI Function Generator");			
			jFrame.addWindowListener(new java.awt.event.WindowAdapter() {
				public void windowClosing(java.awt.event.WindowEvent e) {
					switch(e.getID()) {
					case WindowEvent.WINDOW_CLOSING:
						int answer = JOptionPane.showConfirmDialog(jFrame,"Unsaved changes will be lost. Are you sure?","Exit", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
						if (answer == JOptionPane.YES_OPTION)
							jFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
						else  
							jFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE); //Only close if YES is pressed.							
						break;
					default:
						break;
					}
				}
			});
		}
		return jFrame;
	}

	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			noiseLabel = new JLabel();
			noiseLabel.setBounds(283, 98, 150, 22);
			noiseLabel.setEnabled(false);
			noiseLabel.setFont(new java.awt.Font("Dialog",1,12));
			noiseLabel.setText("default noise amplitude");
			functionLabel = new JLabel();
			functionLabel.setBounds(6, 8, 460, 50);
			functionLabel.setFont(new java.awt.Font("Dialog",1,12));
			functionLabel.setText("<html>Enter formula of the target function like this:<br>(x2 - 5.1/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10<br> </html>");
			jContentPane = new JPanel();
			jContentPane.setLayout(null);
			jContentPane.add(getFunctionFormulaTextField(), null);
			jContentPane.add(functionLabel, null);
			jContentPane.add(getNoiseCheckBox(), null);
			jContentPane.add(getNoiseTextField(), null);
			jContentPane.add(noiseLabel, null);
		}
		return jContentPane;
	}

	/**
	 * This method initializes jJMenuBar	
	 * 	
	 * @return javax.swing.JMenuBar	
	 */
	private JMenuBar getJJMenuBar() {
		if (jJMenuBar == null) {
			jJMenuBar = new JMenuBar();
			jJMenuBar.add(getFileMenu());
			jJMenuBar.add(getHelpMenu());
		}
		return jJMenuBar;
	}

	/**
	 * This method initializes jMenu	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getFileMenu() {
		if (fileMenu == null) {
			fileMenu = new JMenu();
			fileMenu.setText("File");
			fileMenu.add(getSaveMenuItem());
			fileMenu.add(getExitMenuItem());
		}
		return fileMenu;
	}

	/**
	 * This method initializes jMenu	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getHelpMenu() {
		if (helpMenu == null) {
			helpMenu = new JMenu();
			helpMenu.setText("Help");
			helpMenu.add(getAboutMenuItem());
		}
		return helpMenu;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getExitMenuItem() {
		if (exitMenuItem == null) {
			exitMenuItem = new JMenuItem();
			exitMenuItem.setText("Exit");
			exitMenuItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
						int answer = JOptionPane.showConfirmDialog(jFrame,"Unsaved changes will be lost. Are you sure?","Exit", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
						if (answer == JOptionPane.YES_OPTION)
							jFrame.dispose();
						else  
							jFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE); //Only close if YES is pressed.							
				}
			});
		}
		return exitMenuItem;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getAboutMenuItem() {
		if (aboutMenuItem == null) {
			aboutMenuItem = new JMenuItem();
			aboutMenuItem.setText("About");
			aboutMenuItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					JDialog aboutDialog = getAboutDialog();
					aboutDialog.pack();
					Point loc = getJFrame().getLocation();
					loc.translate(20, 20);
					aboutDialog.setLocation(loc);
					aboutDialog.setVisible(true);
				}
			});
		}
		return aboutMenuItem;
	}

	/**
	 * This method initializes aboutDialog	
	 * 	
	 * @return javax.swing.JDialog
	 */
	private JDialog getAboutDialog() {
		if (aboutDialog == null) {
			aboutDialog = new JDialog(getJFrame(), true);
			aboutDialog.setTitle("About");
			aboutDialog.setSize(new Dimension(295, 171));
			aboutDialog.setResizable(false);
			aboutDialog.setContentPane(getAboutContentPane());
		}
		return aboutDialog;
	}

	/**
	 * This method initializes aboutContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getAboutContentPane() {
		if (aboutContentPane == null) {
			aboutContentPane = new JPanel();
			aboutContentPane.setLayout(new BorderLayout());
			aboutContentPane.add(getAboutVersionLabel(), BorderLayout.CENTER);
		}
		return aboutContentPane;
	}

	/**
	 * This method initializes aboutVersionLabel	
	 * 	
	 * @return javax.swing.JLabel	
	 */
	private JLabel getAboutVersionLabel() {
		if (aboutVersionLabel == null) {
			aboutVersionLabel = new JLabel();
			aboutVersionLabel.setText("<html><center>This is a function generator for the SPOT GUI. <br> The parameters of the generated functions <br> can be optimized by SPOT, both by using the <br> GUI or with the command line SPOT call.<br><br>The main purpose is to demonstrate how to <br> build a target function for SPOT, and to easily <br> generate simple test functions.</center></html>");
			aboutVersionLabel.setHorizontalAlignment(SwingConstants.CENTER);
		}
		return aboutVersionLabel;
	}

	/**
	 * This method initializes functionFormulaTextField	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getFunctionFormulaTextField() {
		if (functionFormulaTextField == null) {
			functionFormulaTextField = new JTextField();
			functionFormulaTextField.setText("x1*x1+x2*x2+x3*x3");
			functionFormulaTextField.setBounds(new Rectangle(6, 65, 460, 20));
		}
		return functionFormulaTextField;
	}
	/**
	 * This method initializes noiseCheckBox	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getNoiseCheckBox() {
		if (noiseCheckBox == null) {
			noiseCheckBox = new JCheckBox();
			noiseCheckBox.setFont(new java.awt.Font("Dialog",1,12));
			noiseCheckBox.setText("add noise to the Formula ");
			noiseCheckBox.setBounds(new Rectangle(9, 94, 172, 31));
			/*noiseCheckBox.addItemListener(new java.awt.event.ItemListener() {
				public void itemStateChanged(java.awt.event.ItemEvent e) {
					if(!noiseTextField.isEnabled()){
						noiseTextField.setEnabled(true);
						noiseLabel.setEnabled(true);
					}else{
						noiseTextField.setEnabled(false);
						noiseLabel.setEnabled(false);
					}
				}
			});*/
			noiseCheckBox.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(!noiseTextField.isEnabled()){
						noiseTextField.setEnabled(true);
						noiseLabel.setEnabled(true);
					}else{
						noiseTextField.setEnabled(false);
						noiseLabel.setEnabled(false);
					}
				}
			});
		}
		return noiseCheckBox;
	}

	/**
	 * This method initializes noiseTextField	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getNoiseTextField() {
		if (noiseTextField == null) {
			noiseTextField = new JTextField();
			noiseTextField.setEnabled(false);
			noiseTextField.setBounds(190, 100, 87, 20);
			noiseTextField.setText("0");
		}
		return noiseTextField;
	}

	/**
	 * This method initializes functionFileChooser	
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getFunctionFileChooser() {
		if (functionFileChooser == null) {
			functionFileChooser = new JFileChooser();
			//functionFileChooser.setSize(new Dimension(329, 169));
			//	jFileChooser_reportSelect = new JFileChooser();
			functionFileChooser.removeChoosableFileFilter(functionFileChooser.getChoosableFileFilters()[0]);
			functionFileChooser.addChoosableFileFilter(fileFilterAll);
			functionFileChooser.addChoosableFileFilter(fileFilterR);
		}
		functionFileChooser.setCurrentDirectory(pathDefaultFile); // ***TOD0*** : Analyzepath
		return functionFileChooser;
	}

	/**
	 * This method initializes saveMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getSaveMenuItem() {
		if (saveMenuItem == null) {
			saveMenuItem = new JMenuItem();
			saveMenuItem.setText("Save As .R File");
			saveMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
					Event.CTRL_MASK, true));
			saveMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					JFileChooser functionFileFC = getFunctionFileChooser();
					int ret = functionFileFC.showDialog(saveMenuItem.getParent(), "Save");
					if (ret == JFileChooser.APPROVE_OPTION){
						pathDefaultFile = functionFileFC.getSelectedFile();
						if(!pathDefaultFile.toString().toLowerCase().endsWith(".r")){
							pathDefaultFile = new File(pathDefaultFile.toString()+".r");
						}
						//Calculate the dimension of formula
						int dim=0;
						String formula=functionFormulaTextField.getText().toString();
						int tempDim=0;
						int tempI;
						for(int i=0;i<formula.length();i++){
							if(formula.substring(i).contains("x")){
								tempI=formula.substring(i).indexOf("x");
								for(int ii=i+tempI+1;ii<formula.length();ii++){
									if(ii==formula.length()-1){
										tempDim=Integer.valueOf(formula.substring(i+tempI+1,ii+1)).intValue();
										break;	
									}
									else if(!Character.isDigit(formula.charAt(ii+1))){
										tempDim=Integer.valueOf(formula.substring(i+tempI+1,ii+1)).intValue();
										break;
									}
								}							
								if(tempDim>dim)
									dim=tempDim;
								i=i+1;
							}
						}
						fileIO.writeTarFunc(pathDefaultFile, //File to be written
								formula, //Function formula
								dim, //Dimension of formula
								noiseCheckBox.isSelected(), //noise false/true
								noiseTextField.getText().toString()); //noise amplitude
					}	
				}
			});
		}
		return saveMenuItem;
	}

	/**
	 * Launches this application
	 */
	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				tarFuncGenerator application = new tarFuncGenerator();
				application.getJFrame().setVisible(true);
			}
		});
	}

}
