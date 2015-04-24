package spotGui;

import java.awt.event.KeyEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import java.awt.Point;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JFrame;
import javax.swing.JDialog;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Properties;
import javax.swing.BorderFactory;
import javax.swing.ComboBoxModel;
import javax.swing.DefaultCellEditor;
import javax.swing.DefaultComboBoxModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TableModelEvent;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.FontUIResource;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import org.apache.commons.lang.SystemUtils; // included in extern jar (referenced library)

public class spotGuiMain {

	// --------------SPOT GUI VARS--------------
	String SPOTGUIVERSION = "1.3";  //  @jve:decl-index=0:
	Properties setupProperties = new Properties(); // path setup will be saved in this var  //  @jve:decl-index=0:
	// --------------PATH VARS--------------
	private boolean setupPropertieExist = false;
	private File pathFile_SetupProperties = new File(""); // SETUP FILE (like %path_SPOT%/gui/setup.cfg)  //  @jve:decl-index=0:
	private File path_JAR = new File("");
	private File pathFile_HelpPdf = new File("");
	private File pathFile_R = new File("");	// filepath to r.exe (win) or r (others)  //  @jve:decl-index=0:
	private File pathFile_ImportSettigns = new File("");	// filepath to import .settings file  //  @jve:decl-index=0:
	private File pathFile_ExportSettigns = new File("");	// filepath to export .settings file
	private File path_Project = new File("");
	private File pathFile_ProjectConf = new File("");
	private File pathFile_ProjectRoi = new File("");
	private File pathFile_AlgorithmScript = new File("");
	private File pathFile_APD = new File("");
	private File pathFile_LoadROI = new File("");  //  @jve:decl-index=0:
	//private File pathFile_LoadCONF = new File("");
	private File pathFile_ReportScript = new File("");	
	// --------------FC DEFAULT PATH VARS--------------
	private File pathFCDefaultRPATH = new File("");  //  @jve:decl-index=0:
	private File pathFCDefaultImportSettigns = new File("");
	private File pathFCDefaultExportSettigns = new File("");
	private File pathFCDefaultCONFproject = new File("");
	private File pathFCDefaultCONFalgorithmScript = new File("");
	private File pathFCDefaultReportScript = new File("");
	private File pathFCDefaultAPD = new File("");	
	// --------------ROI VARS--------------
	private int roiTableMinDim = 1;
	private int roiTableMaxDim = 100;
	private int roiTableDimInit = 5;
	private int roiTableDim = roiTableDimInit;
	private String[] roiTableColumnNames = {"Parametername", "Lower bound", "Upper bound", "Datatype"};
	private String[] roiTableTypes = {"INT","FLOAT","FACTOR"};
	private String[][] roiTableData = new String[roiTableMaxDim][roiTableColumnNames.length];
	private String roiTableDefaultMinValue = "0";  //  @jve:decl-index=0:
	private String roiTableDefaultMaxValue = "10";
	private String roiTableDefaultTypeValue = roiTableTypes[1];  //  @jve:decl-index=0:
	// --------------CONF VARS--------------
	private String conf_algPath = "";
	private String conf_algName = "";
	private String conf_algPdFile = "";		
	private String conf_model = "";
	private String conf_design_type = "";
	private String conf_init_design_size = "";
	private String conf_seq_design_size = "";
	private String conf_auto_loop_steps = "";  //  @jve:decl-index=0:
	private String conf_auto_loop_nevals = "";
	private String relativePathFileString_algPath = "";
	private String relativePathFileString_algPd = "";
	private String sepLineSettings = "[---SETTINGSFILESEPARATOR---]";
	private String sepRoiFile = " ";
	//private String relativePathFileString_reportPath = "";		
	private String reportFunction = "";
	//private String reportPath = "";  //  @jve:decl-index=0:
	// --------------Global Checks--------------
	private boolean confAPDSelectedCheck = false;
	private boolean confAPDExistCheck = false;
	// --------------SYSTEM CHECK VARS--------------
	boolean IS_OS_WINDOWS = false;
	boolean IS_OS_LINUX = false;
	boolean IS_OS_MAC = false;
	String FILE_SEPERATOR = "";
	String LINE_SEPERATOR = "";  //  @jve:decl-index=0:
	// --------------IMAGEICONS--------------	
	//ImageIcon iconTab1 = new ImageIcon(this.getClass().getResource("icon_tab_welcome.gif"));  //  @jve:decl-index=0:
	//ImageIcon iconTab2 = new ImageIcon(this.getClass().getResource("icon_tab_algorithm.png"));  //  @jve:decl-index=0:
	//ImageIcon iconTab3 = new ImageIcon(this.getClass().getResource("icon_tab_roi.gif"));  //  @jve:decl-index=0:
	//ImageIcon iconTab4 = new ImageIcon(this.getClass().getResource("icon_tab_conf.gif"));  //  @jve:decl-index=0:
	//ImageIcon iconTab5 = new ImageIcon(this.getClass().getResource("icon_tab_run.gif"));  //  @jve:decl-index=0:
	//ImageIcon iconTab6 = new ImageIcon(this.getClass().getResource("icon_tab_analyse.gif"));  //  @jve:decl-index=0:
	//ImageIcon(getClass().getResource("/spotGui/icon_check_invalid.gif"
	ImageIcon iconCheckValid = new ImageIcon(getClass().getResource("icon_check_valid.gif"));  //  @jve:decl-index=0:
	ImageIcon iconCheckInvalid = new ImageIcon(getClass().getResource("icon_check_invalid.gif"));  //  @jve:decl-index=0:
	ImageIcon iconCheckUnknown = new ImageIcon(getClass().getResource("icon_check_unknown.gif"));  //  @jve:decl-index=0:
	ImageIcon iconLogo = new ImageIcon(getClass().getResource("spotOfficial.jpg"));  //  @jve:decl-index=0:
	//ImageIcon iconLogo = new ImageIcon(this.getClass().getResource("icon_logo.png"));  //  @jve:decl-index=0:
	//ImageIcon iconLogo2 = new ImageIcon(this.getClass().getResource("spotOfficial.jpg"));  //  @jve:decl-index=0:
		//ImageIcon iconApplication =new ImageIcon(this.getClass().getResource("temp.gif"));  //  @jve:decl-index=0:
	// --------------Combo Box String Lists--------------
	String[] designTypeStrings = new String[] { "spotCreateDesignBasicDoe","spotCreateDesignDoeR3","spotCreateDesignLhd","spotCreateDesignLhs","spotCreateDesignFrF2"};		
	String[] modelStrings = new String[] {"spotPredictTree","spotPredictRandomForest","spotPredictRandomForestMlegp","spotPredictLm","spotPredictTgp","spotPredictMlegp","spotPredictForrester","spotPredictGausspr","spotPredictKsvm","spotPredictPsvm","spotPredictQrnn","spotPredictEarth"};
	String[] algnameStrings = new String[] { "spotFuncStartBranin","spotFuncStartSixHump","spotFuncStartSphere","spotAlgStartSann","spotAlgStartEs"};
	String[] analyzeFunctionStrings =	new String[] { "spotReportDefault", "spotReportSens" , "spotReport3d" , "spotReportContour"};
	// --------------NONVISIBLE GUI ELEMENTS (FCs)--------------
	private JFileChooser jFileChooser_importSettings;
	private JFileChooser jFileChooser_import2Settings;
	private JFileChooser jFileChooser_import3Settings;
	private JFileChooser jFileChooser_exportSettings;
	private JFileChooser jFileChooser_rPath;
	private JFileChooser jFileChooser_algorithmSelect;
	//private JFileChooser jFileChooser_reportSelect;
	private JFileChooser jFileChooser_CONFprojectPath;
	private JFileChooser jFileChooser_APD;
	// --------------FILEFILTERS--------------
/*	private FileFilter fileFilterSetting = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			return f.getName().toLowerCase().endsWith(".setting");
		}
		public String getDescription () { return "Setting files (*.setting)"; }  
	};*/
	private FileFilter fileFilterExe = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;
			return f.getName().toLowerCase().endsWith(".exe"); //|| f.getName().toLowerCase().endsWith(".app") || f.getName().toLowerCase().endsWith(".sh");
		}
		public String getDescription () { return "Executable files (*.exe)"; }  
	};
	private FileFilter fileFilterAlgo = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			String tmp = f.getName().toLowerCase();
			return tmp.endsWith(".r") || tmp.endsWith(".bat") || tmp.endsWith(".sh");
		}
		public String getDescription () { return "Algorithm script files (*.r, *.bat, *.sh)"; }  
	};
/*	private FileFilter fileFilterImport = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			String tmp = f.getName().toLowerCase();
			return tmp.endsWith(".setting") || tmp.endsWith(".roi") || tmp.endsWith(".conf");
		}
		public String getDescription () { return "Importable files (*.setting, *.roi, *.conf)"; }  
	};*/
	private FileFilter fileFilterAll = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			return !f.getName().isEmpty();
		}
		public String getDescription () { return "All files (*.*)"; }
	};
	private FileFilter fileFilterR = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			return f.getName().toLowerCase().endsWith(".r");
		}
		public String getDescription () { return "R script files (*.r)"; }
	};
	private FileFilter fileFilterROI = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			return f.getName().toLowerCase().endsWith(".roi");
		}
		public String getDescription () { return "ROI files (*.roi)"; }
	};
	private FileFilter fileFilterCONF = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;  //  @jve:decl-index=0:
			return f.getName().toLowerCase().endsWith(".conf");
		}
		public String getDescription () { return "CONF files (*.conf)"; }
	};
	private FileFilter fileFilterAPD = new FileFilter() {
		public boolean accept(File f) {
			if (f.isDirectory()) return true;
			return f.getName().toLowerCase().endsWith(".apd");
		}
		public String getDescription () { return "Algorithm problem design files (*.apd)"; }
	};

	//--------------SET LOOK AND FEEL--------------
	{
		try {
			javax.swing.UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
			// overwrite fonts from LAF (to get nearly same text-look (and width!) on win/linux)
			Font defaultFont= new Font( "Dialog", Font.PLAIN, 11 );
			javax.swing.UIManager.put( "TitledBorder.font", new FontUIResource ( new Font( "Dialog", Font.BOLD, 11 ) ) );
			javax.swing.UIManager.put( "Table.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "Panel.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "Label.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "TextPane.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "ScrollPane.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "OptionPane.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "EditorPane.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "Tree.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "TextField.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "RadioButton.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "MenuItem.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "ComboBox.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "ToolTip.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "TextArea.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "ProgressBar.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "MenuBar.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "ColorChooser.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "ToolBar.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "Text.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "PopupMenu.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "Menu.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "Checkbox.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "ToggleButton.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "TableHeader.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "PasswordField.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "List.font", new FontUIResource ( defaultFont ) );
			javax.swing.UIManager.put( "Button.font", new FontUIResource ( defaultFont ) );
		} catch(Exception e) {
			e.printStackTrace();
		}
	}	
	//##################################################################################################
	boolean globalDEBUG = true;
	//##################################################################################################
	private JFrame jFrame = null;  //  @jve:decl-index=0:visual-constraint="10,-8"
	private JPanel jContentPane = null;
	private JTabbedPane jTabbedPaneMain = null;
	private JPanel jPanel_welcome = null;
	private JLabel jLabel_logo = null;
	private JPanel jPanel_algorithm = null;
	//private JTextField jTextField_selectedReportScript = null;
	//private JButton jButton_reportPathBrowse = null;
	private JLabel jLabel_reportFunction = null;
	private JComboBox jComboBox_reportFunction = null;
	private JComboBox jComboBox_CONFalgname = null;
	private JLabel jLabel_CONFalgname = null;
	private JTextField jTextField_CONFalgPdFilePath = null;
	private JButton jButton_CONFalgPdFilePathBrowse = null;
	private JTextField jTextField_selectedAlgorithmScript = null;
	private JPanel jPanel4 = null;
	private JButton jButton_algorithmPathBrowse = null;
	private JPanel jPanel_roi = null;
	private JScrollPane jScrollPane1 = null;
	private JTable jTable_ROI = null;
	private JPanel jPanel11 = null;
	private JTextField jTextField_ROIdim = null;
	private JSlider jSlider_ROIdim = null;
	private JPanel jPanel2 = null;
	private JLabel jLabel_ROIdefaultType = null;
	private JLabel jLabel_ROIdefaultLow = null;
	private JLabel jLabel_ROIdefaultHigh = null;
	private JComboBox jComboBox_ROIdefaultType = null;
	private JTextField jTextField_ROIdefaultLow = null;
	private JTextField jTextField_ROIdefaultHigh = null;
	private JPanel jPanel_spoconf = null;
	private JScrollPane jScrollPane2 = null;
	private JTextArea jTextArea_CONFadditional = null;
	private JPanel jPanel_CONFstep = null;
	private JLabel jLabel_CONFinitDesignPoints = null;
	private JTextField jTextField_CONFinitLhdPoints = null;
	private JComboBox jComboBox_CONFdesignType = null;
	private JLabel jLabel_CONFdesignType = null;
	private JPanel jPanel3 = null;
	private JTextField jTextField_CONFautoLoopSteps = null;
	private JTextField jTextField_CONFautoLoopNEvals = null;
	private JLabel jLabel_CONFautoLoopSteps = null;
	private JLabel jLabel_CONFautoLoopNEvals = null;
	private JTextField jTextField_CONFseqDesignSize = null;
	private JLabel jLabel_CONFseqDesignSize = null;
	private JLabel jLabel_CONFmodel = null;
	private JComboBox jComboBox_CONFmodel = null;
	private JCheckBox jCheckBox_CONFadvanced = null;
	private JPanel jPanel_CONFproject = null;
	private JTextField jTextField_CONFprojectPath = null;
	private JButton jButton_CONFprojectPathBrowse = null;
	private JLabel jLabel_CONFprojectPath = null;
	private JTextField jTextField_CONFprojectPrefix = null;
	private JLabel jLabel_CONFprojectPrefix = null;
	private JCheckBox jCheckBox_CONFprojectPathSubfolder = null;
	private JPanel jPanel_run = null;
	private JPanel jPanel6 = null;
	private JLabel jLabel1 = null;
	private JLabel jLabel2 = null;
	private JLabel jLabel3 = null;
	private JPanel jPanel_checkSpotRun = null;
	private JLabel jLabel_checkSpotRun_algorithm = null;
	private JLabel jLabel_checkSpotRun_roiConfig = null;
	private JLabel jLabel_checkSpotRun_spoConfig = null;
	private JLabel jLabel_checkSpotRun_rPath = null;
	private JLabel jLabel_checkSpotRun_spoConfigFile = null;
	private JLabel jLabel_checkSpotRun_roiConfigFile = null;
	private JButton jButton_checkSpotRun = null;
	private JPanel jPanel1_SpotRun = null;
	private JLabel jLabel_taskSelect = null;
	private JComboBox jComboBox_taskSelect = null;
	private JButton jButton_spotRun = null;
	private JScrollPane jScrollPane3 = null;
	private ColorPane jColorPane_spotRun = null;
	private ColorPane jColorPane_analyzeRun = null;
	private JPanel jPanel_analyze = null;
	private JScrollPane jScrollPane4 = null;
	private JPanel jPanel_viewFiles = null;
	private JComboBox jComboBox_ViewRawFile = null;
	private JComboBox jComboBox_ViewPdfFile = null;
	private JButton jButton_viewRawFile = null;
	private JButton jButton_viewPdfFile = null;
	private JScrollPane jScrollPane5 = null;
	private JEditorPane jJEditorPane_Intro = null;
	private JDialog jDialog_pathSetup = null;  //  @jve:decl-index=0:visual-constraint="552,52"
	private JPanel jContentPane1 = null;
	private JTextField jTextField_rPath = null;
	private JButton jButton_rPathBrowse = null;
	private JLabel jLabel_rPath = null;
	private JButton jButton_pathSetupCancel = null;
	private JButton jButton_savePathSetup = null;
	private JMenuBar jJMenuBar = null;
	private JMenu fileMenu = null;
	private JMenu helpMenu = null;
	private JMenuItem exitMenuItem = null;
	private JMenuItem aboutMenuItem = null;
	private JDialog aboutDialog = null;
	private JPanel aboutContentPane = null;
	private JLabel aboutVersionLabel = null;
	private JMenuItem helpPdfMenuItem = null;
	private JMenuItem importMenuItem = null;
	private JMenuItem exportMenuItem = null;
	private JMenu toolsMenu = null;
	private JMenuItem funcGeneratorMenuItem = null;
	private JMenuItem setRPathMenuItem = null;
	private JPanel selectAlgorithmPanel = null;
	private JPanel selectApdPanel = null;
	private JPanel selectReportPanel = null;
	private JPanel autoConfigPanel = null;
	private JLabel tarDescriptionLabel = null;
	private JLabel repDescriptionLabel = null;
	private JLabel apdDescriptionLabel = null;
	private JMenuItem import2MenuItem = null;
	private JMenuItem import3MenuItem = null;
	/**
	 * This method initializes jFrame
	 * 
	 * @return javax.swing.JFrame
	 */
	private JFrame getJFrame() {
		if (jFrame == null) {
			jFrame = new JFrame();
			jFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE); //TODO: Oder EXIT_ON_CLOSE?
			jFrame.setIconImage(Toolkit.getDefaultToolkit().getImage(getClass().getResource("/spotGui/temp.gif")));
			jFrame.setJMenuBar(getJJMenuBar());
			jFrame.setContentPane(getJContentPane());
			jFrame.setTitle("Application");
			systemCheck();
			try {
				path_JAR = new File(URLDecoder.decode(new File(spotGuiMain.class.getProtectionDomain().getCodeSource().getLocation().getPath()).getParent(),"ISO-8859-15"));
			} catch (IOException e) {
				e.printStackTrace();
			}
			pathFile_HelpPdf = new File(path_JAR.toString()+FILE_SEPERATOR+"help.pdf");
			pathFile_SetupProperties = new File(path_JAR.toString()+ FILE_SEPERATOR + "setup.cfg");
			if(globalDEBUG) System.out.println("path_JAR: " + path_JAR.toString());
			if(globalDEBUG) System.out.println("pathFile_SetupProperties: " + pathFile_SetupProperties.toString());
			if(globalDEBUG) System.out.println("pathFileHelpPdf: " + pathFile_HelpPdf.toString());			
			if(!pathFile_HelpPdf.exists()){
				System.err.println("Help PDF file ("+pathFile_HelpPdf.toString()+") not found");
				helpPdfMenuItem.setEnabled(false);
			}
			loadPathSetup(pathFile_SetupProperties);			
			Locale.setDefault(Locale.ENGLISH);
			JComponent.setDefaultLocale(Locale.ENGLISH);		
			jFrame.setPreferredSize(new java.awt.Dimension(530, 530));
			jFrame.setSize(530, 513);
			jFrame.setResizable(false);			
			jFrame.addWindowListener(new java.awt.event.WindowAdapter() {
				public void windowClosing(java.awt.event.WindowEvent e) {
					switch(e.getID()) {
					case WindowEvent.WINDOW_CLOSING:
						int answer = JOptionPane.showConfirmDialog(jFrame,"All current settings will be lost. Are you sure?","Exit", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
						if (answer == JOptionPane.YES_OPTION)
							System.exit(0);
						else  
							jFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE); //Only close if YES is pressed.							
						break;
					default:
						break;
					}
				}
			});
			jFrame.setTitle("SPOT GUI "+SPOTGUIVERSION);	
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
			jContentPane = new JPanel();
			jContentPane.setLayout(null);
			jContentPane.add(getJTabbedPaneMain());
		}
		return jContentPane;
	}

	/**
	 * This method initializes jTabbedPaneMain	
	 * 	
	 * @return javax.swing.JTabbedPane	
	 */
	private JTabbedPane getJTabbedPaneMain() {
		if (jTabbedPaneMain == null) {
			jTabbedPaneMain = new JTabbedPane();
			jTabbedPaneMain.setFont(new java.awt.Font("Dialog",0,12));
			jTabbedPaneMain.setSize(504, 451);
			jTabbedPaneMain.setLocation(new Point(8, 2));
			jTabbedPaneMain.addTab("Intro", new ImageIcon(getClass().getResource("/spotGui/icon_tab_welcome.gif")), getJPanel_welcome(), "Welcome to SPOT GUI");
			jTabbedPaneMain.addTab("Function select", new ImageIcon(getClass().getResource("/spotGui/icon_tab_algorithm.png")), getJPanel_algorithm(), "First step: Select your files");
			jTabbedPaneMain.addTab("ROI", new ImageIcon(getClass().getResource("/spotGui/icon_tab_roi.gif")), getJPanel_roi(), "Second step: Define your region of interest");
			jTabbedPaneMain.addTab("Config", new ImageIcon(getClass().getResource("/spotGui/icon_tab_conf.gif")), getJPanel_spoconf(), "Configuration: Define SPOT parameters");
			jTabbedPaneMain.addTab("Run", new ImageIcon(getClass().getResource("/spotGui/icon_tab_run.gif")), getJPanel_run(), "Get Results of your SPOT Run");
			jTabbedPaneMain.addTab("Results", new ImageIcon(getClass().getResource("/spotGui/icon_tab_analyse.gif")), getJPanel_analyze(), "Get Results of your SPOT Run");
			
			//jTabbedPaneMain.addTab("Intro", iconTab1, getJPanel_welcome(), "Welcome to SPOT GUI");			
			//jTabbedPaneMain.addTab("File select", iconTab2, getJPanel_algorithm(), "First step: Select your files");
			//jTabbedPaneMain.addTab("ROI", iconTab3, getJPanel_roi(), "Second step: Define your region of interest");
			//jTabbedPaneMain.addTab("Config",iconTab4, getJPanel_spoconf(), "Configuration: Define SPOT parameters");
			//jTabbedPaneMain.addTab("Run", iconTab5, getJPanel_run(), "Get Results of your SPOT Run");
			//jTabbedPaneMain.addTab("Results", iconTab6, getJPanel_analyze(), "Get Results of your SPOT Run");
			
			jTabbedPaneMain.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);	
			/*
			jTabbedPaneMain.addTab("Intro",null, getJPanel_welcome(), "Welcome to SPOT GUI");			
			jTabbedPaneMain.addTab("File select", null, getJPanel_algorithm(), "First step: Select your files");
			jTabbedPaneMain.addTab("ROI", null, getJPanel_roi(), "Second step: Define your region of interest");
			jTabbedPaneMain.addTab("Config",null, getJPanel_spoconf(), "Configuration: Define SPOT parameters");
			jTabbedPaneMain.addTab("Run", null, getJPanel_run(), "Get Results of your SPOT Run");
			jTabbedPaneMain.addTab("Results", null, getJPanel_analyze(), "Get Results of your SPOT Run");
*/
		}
		return jTabbedPaneMain;
	}

	/**
	 * This method initializes jPanel_welcome	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_welcome() {
		if (jPanel_welcome == null) {
			jPanel_welcome = new JPanel();
			jPanel_welcome.setLayout(null);
			jPanel_welcome.add(getJPanel4(), null);
		}
		return jPanel_welcome;
	}

	/**
	 * This method initializes jPanel_algorithm	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_algorithm() {
		if (jPanel_algorithm == null) {
			jPanel_algorithm = new JPanel();
			jPanel_algorithm.setLayout(null);
			jLabel_CONFalgname = new JLabel();
			jLabel_CONFalgname.setText("R Function which starts the algorithm:");
			jLabel_CONFalgname.setBounds(11, 152, 182, 14);
			jLabel_CONFalgname.setFont(new java.awt.Font("Dialog",0,10)); //TODO: Andere brauchen den selben font nicht
			jLabel_reportFunction = new JLabel();
			jLabel_reportFunction.setText("R-Function which starts the report:");
			jLabel_reportFunction.setLayout(null);
			jLabel_reportFunction.setFont(new java.awt.Font("Dialog",0,10));
			jLabel_reportFunction.setBounds(11, 81, 182, 14);
			jPanel_algorithm.add(getSelectAlgorithmPanel(), null);
			jPanel_algorithm.add(getSelectApdPanel(), null);
			jPanel_algorithm.add(getSelectReportPanel(), null);
		}
		return jPanel_algorithm;
	}

	/**
	 * This method initializes jTextField_selectedReportScript	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	/*
	private JTextField getJTextField_selectedReportScript() {
		if (jTextField_selectedReportScript == null) {
			jTextField_selectedReportScript = new JTextField();
			jTextField_selectedReportScript.setEditable(false);
			jTextField_selectedReportScript.setBounds(new Rectangle(11, 59, 360, 20));
		}
		return jTextField_selectedReportScript;
	}*/

	/**
	 * This method initializes jButton_reportPathBrowse	
	 * 	
	 * @return javax.swing.JButton	
	 */
	/*
	private JButton getJButton_reportPathBrowse() {
		if (jButton_reportPathBrowse == null) {
			jButton_reportPathBrowse = new JButton();
			jButton_reportPathBrowse.setText("Browse");
			jButton_reportPathBrowse.setBounds(new Rectangle(381, 57, 100, 22));
			jButton_reportPathBrowse.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					JFileChooser analyzePathFC = getJFileChooser_analyzeSelect();
					int ret = analyzePathFC.showDialog(jButton_reportPathBrowse.getParent(), "Set script path");
					if (ret == JFileChooser.APPROVE_OPTION){
						pathFile_ReportScript = analyzePathFC.getSelectedFile();
						pathFCDefaultReportScript = pathFile_ReportScript;
						jTextField_selectedReportScript.setText(pathFile_ReportScript.toString());
						String[] tmp = fileIO.readFuncFromRscript(pathFile_ReportScript);
						if(tmp!=null){
							String[] tmp1= new String[tmp.length+2]; // Add standard entries
							for(int i=0;i<tmp.length;i++){
								tmp1[i]=tmp[i];
							}
							tmp1[tmp1.length-2]="spotReportDefault";
							tmp1[tmp1.length-1]="spotReportSens";
							ComboBoxModel jComboBox_analyzeFunctionModel = new DefaultComboBoxModel(tmp1);
							jComboBox_reportFunction.setModel(jComboBox_analyzeFunctionModel);
							jComboBox_reportFunction.setSelectedItem(null);
						}					
					}
				}
			});
		}
		return jButton_reportPathBrowse;
	}*/

	/**
	 * This method initializes jComboBox_reportFunction	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getJComboBox_reportFunction() {
		if (jComboBox_reportFunction == null) {
			jComboBox_reportFunction = new JComboBox();
			jComboBox_reportFunction.setBounds(201, 77, 280, 21);
			ComboBoxModel jComboBox_analyzeFunctionModel = 
				new DefaultComboBoxModel(analyzeFunctionStrings);							
			jComboBox_reportFunction.setModel(jComboBox_analyzeFunctionModel);	
		}
		return jComboBox_reportFunction;
	}

	/**
	 * This method initializes jComboBox_CONFalgname	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getJComboBox_CONFalgname() {
		if (jComboBox_CONFalgname == null) {
			jComboBox_CONFalgname = new JComboBox();
			jComboBox_CONFalgname.setEnabled(true);
			jComboBox_CONFalgname.setBounds(201, 148, 280, 21);
			ComboBoxModel jComboBox_CONFalgnameModel = 
				new DefaultComboBoxModel(algnameStrings);							
			jComboBox_CONFalgname.setModel(jComboBox_CONFalgnameModel);	
		}
		return jComboBox_CONFalgname;
	}

	/**
	 * This method initializes jTextField_CONFalgPdFilePath	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_CONFalgPdFilePath() {
		if (jTextField_CONFalgPdFilePath == null) {
			jTextField_CONFalgPdFilePath = new JTextField();
			jTextField_CONFalgPdFilePath.setEditable(false);
			jTextField_CONFalgPdFilePath.setBounds(new Rectangle(11, 64, 360, 20));
		}
		return jTextField_CONFalgPdFilePath;
	}

	/**
	 * This method initializes jButton_CONFalgPdFilePathBrowse	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_CONFalgPdFilePathBrowse() {
		if (jButton_CONFalgPdFilePathBrowse == null) {
			jButton_CONFalgPdFilePathBrowse = new JButton();
			jButton_CONFalgPdFilePathBrowse.setText("Browse");
			jButton_CONFalgPdFilePathBrowse.setBounds(new Rectangle(381, 62, 100, 22));
			jButton_CONFalgPdFilePathBrowse
			.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					JFileChooser apdFilePathFC = getJFileChooser_APD();
					int ret = apdFilePathFC.showDialog(jButton_CONFalgPdFilePathBrowse.getParent(), "Select APD");
					if (ret == JFileChooser.APPROVE_OPTION){
						pathFile_APD = apdFilePathFC.getSelectedFile();
						pathFCDefaultAPD = pathFile_APD;
						jTextField_CONFalgPdFilePath.setText(pathFile_APD.toString());
					}
				}
			});
		}
		return jButton_CONFalgPdFilePathBrowse;
	}

	/**
	 * This method initializes jTextField_selectedAlgorithmScript	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_selectedAlgorithmScript() {
		if (jTextField_selectedAlgorithmScript == null) {
			jTextField_selectedAlgorithmScript = new JTextField();
			jTextField_selectedAlgorithmScript.setEditable(false);
			jTextField_selectedAlgorithmScript.setBounds(new Rectangle(11, 120, 360, 20));
		}
		return jTextField_selectedAlgorithmScript;
	}

	/**
	 * This method initializes jPanel4	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel4() {
		if (jPanel4 == null) {
			jPanel4 = new JPanel();
			jPanel4.setBorder(BorderFactory.createTitledBorder("Quick introduction"));
			jPanel4.setLayout(null);
			jPanel4.setBounds(5, 5, 490, 410);
			jPanel4.add(getJScrollPane5(), null);
		}
		return jPanel4;
	}

	/**
	 * This method initializes jButton_algorithmPathBrowse	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_algorithmPathBrowse() {
		if (jButton_algorithmPathBrowse == null) {
			jButton_algorithmPathBrowse = new JButton();
			jButton_algorithmPathBrowse.setText("Browse");
			jButton_algorithmPathBrowse.setBounds(new Rectangle(381, 118, 100, 22));
			jButton_algorithmPathBrowse.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					JFileChooser algoPathFC = getJFileChooser_algorithmSelect();
					int ret = algoPathFC.showDialog(jButton_algorithmPathBrowse.getParent(), "Select script");
					if (ret == JFileChooser.APPROVE_OPTION){
						pathFCDefaultCONFalgorithmScript = algoPathFC.getSelectedFile();
						pathFile_AlgorithmScript = pathFCDefaultCONFalgorithmScript;
						jTextField_selectedAlgorithmScript.setText(pathFile_AlgorithmScript.toString());									
						String[] tmp = fileIO.readFuncFromRscript(algoPathFC.getSelectedFile());
						if (tmp!=null){										
							String[] tmp1= new String[tmp.length+5]; // Add standard entries
							for(int i=0;i<tmp.length;i++){
								tmp1[i]=tmp[i];
							}
							tmp1[tmp1.length-5]="spotFuncStartBranin";
							tmp1[tmp1.length-4]="spotFuncStartSixHump";
							tmp1[tmp1.length-3]="spotFuncStartSphere";
							tmp1[tmp1.length-2]="spotAlgStartSann";
							tmp1[tmp1.length-1]="spotAlgStartEs";
							ComboBoxModel jComboBox_CONFalgnameModel = new DefaultComboBoxModel(tmp1);
							jComboBox_CONFalgname.setModel(jComboBox_CONFalgnameModel);
							jComboBox_CONFalgname.setSelectedItem(null);
						}
						//jTextField_CONFalgorithmScriptFile.setText(pathFile_AlgorithmScript.getName());		
					}
				}
			});
		}
		return jButton_algorithmPathBrowse;
	}

	/**
	 * This method initializes jPanel_roi	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_roi() {
		if (jPanel_roi == null) {
			jPanel_roi = new JPanel();
			jPanel_roi.setLayout(null);
			jPanel_roi.add(getJScrollPane1(), null);
			jPanel_roi.add(getJPanel11(), null);
			jPanel_roi.add(getJPanel2(), null);
		}
		return jPanel_roi;
	}

	/**
	 * This method initializes jScrollPane1	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane1() {
		if (jScrollPane1 == null) {
			jScrollPane1 = new JScrollPane();
			jScrollPane1.setBounds(9, 152, 486, 263);
			jScrollPane1.setViewportView(getJTable_ROI());
			jScrollPane1.getHorizontalScrollBar().addAdjustmentListener(new AdjustmentListener(){
				public void adjustmentValueChanged(AdjustmentEvent e) {
					jTable_ROI.repaint();
				}
			});
			jScrollPane1.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
				public void adjustmentValueChanged(AdjustmentEvent e) {
					jTable_ROI.repaint();
				}
			});
		}
		return jScrollPane1;
	}

	/**
	 * This method initializes jTable_ROI	
	 * 	
	 * @return javax.swing.JTable	
	 */
	private JTable getJTable_ROI() {
		if (jTable_ROI == null) {	
			jTable_ROI = new JTable();
			initTableData(roiTableDefaultMinValue, roiTableDefaultMaxValue, roiTableDefaultTypeValue);
			myTableModel myModel = new myTableModel();
			jTable_ROI = new JTable(myModel);//$hide$			
			JComboBox comboBox = new JComboBox();
			comboBox.setModel(new DefaultComboBoxModel(roiTableTypes));
			jTable_ROI.getColumnModel().getColumn(3).setCellEditor(new DefaultCellEditor(comboBox));
			jTable_ROI.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			jTable_ROI.getTableHeader().setReorderingAllowed(false);
			Dimension d = jTable_ROI.getIntercellSpacing();
			jTable_ROI.setIntercellSpacing(new Dimension(d.width+1, d.height+1));
			jTable_ROI.setRowHeight(jTable_ROI.getRowHeight()+1);
			// COLUMN ALLIGNMENT:
			DefaultTableCellRenderer renT = new DefaultTableCellRenderer();
			renT.setHorizontalAlignment(JLabel.RIGHT);
			jTable_ROI.getColumn(roiTableColumnNames[1]).setCellRenderer(renT);//$hide$
			jTable_ROI.getColumn(roiTableColumnNames[2]).setCellRenderer(renT);//$hide$ 
		}
		return jTable_ROI;
	}
	/**
	 * This method initializes jPanel11	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel11() {
		if (jPanel11 == null) {
			jPanel11 = new JPanel();
			jPanel11.add(getJTextField_ROIdim(), null);
			jPanel11.add(getJSlider_ROIdim(), null);
			jPanel11.setBounds(7, 5, 487, 60);
			jPanel11.setLayout(null);
			jPanel11.setBorder(BorderFactory.createTitledBorder("Count of parameters"));
		}
		return jPanel11;
	}

	/**
	 * This method initializes jTextField_ROIdim	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_ROIdim() {
		if (jTextField_ROIdim == null) {
			jTextField_ROIdim = new JTextField();
			jTextField_ROIdim.setBounds(18, 22, 50, 25);
			jTextField_ROIdim.setText(Integer.toString(roiTableDimInit));
			jTextField_ROIdim.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					int val=0;
					if(isRegExInt(jTextField_ROIdim.getText())!=null){
						val = Integer.valueOf(jTextField_ROIdim.getText()).intValue();
						if(val>=roiTableMinDim && val<=roiTableMaxDim){
							updateTableDim(val);
						}else{
							JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "The value is out of range. The valid value range is defined from " + Integer.toString(roiTableMinDim) + " to " + Integer.toString(roiTableMaxDim) + "   (Invalid value was: "+val+")", "Value error", JOptionPane.ERROR_MESSAGE);
							jTextField_ROIdim.setText(Integer.toString(jSlider_ROIdim.getValue()));
						}
					}else{
						JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "Type of value have to be INT." + "   (Invalid value was: "+jTextField_ROIdim.getText()+")", "Value type error", JOptionPane.ERROR_MESSAGE);
						jTextField_ROIdim.setText(Integer.toString(jSlider_ROIdim.getValue()));
					}
				}
			});
		}
		return jTextField_ROIdim;
	}

	/**
	 * This method initializes jSlider_ROIdim	
	 * 	
	 * @return javax.swing.JSlider	
	 */
	private JSlider getJSlider_ROIdim() {
		if (jSlider_ROIdim == null) {
			jSlider_ROIdim = new JSlider();
			jSlider_ROIdim.setBounds(78, 22, 403, 25);
			jSlider_ROIdim.setMinimum(roiTableMinDim);
			jSlider_ROIdim.setMaximum(roiTableMaxDim);
			jSlider_ROIdim.setValue(roiTableDimInit);
			jSlider_ROIdim.setSnapToTicks(true);
			jSlider_ROIdim.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					JSlider source = (JSlider)e.getSource();
					int sliderValue = source.getValue();
					jTextField_ROIdim.setText(String.valueOf(sliderValue));
					if(!source.getValueIsAdjusting()){ // don't update values while slider is in adjusting action
						updateTableDim(sliderValue);
					}
				}
			});
		}
		return jSlider_ROIdim;
	}

	/**
	 * This method initializes jPanel2	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel2() {
		if (jPanel2 == null) {
			jLabel_ROIdefaultHigh = new JLabel();
			jLabel_ROIdefaultHigh.setText("default upper bound:");
			jLabel_ROIdefaultHigh.setBounds(5, 51, 111, 14);
			jLabel_ROIdefaultHigh.setFont(new java.awt.Font("Dialog",0,10));
			jLabel_ROIdefaultHigh.setHorizontalAlignment(SwingConstants.RIGHT);
			jLabel_ROIdefaultLow = new JLabel();
			jLabel_ROIdefaultLow.setText("default lower bound:");
			jLabel_ROIdefaultLow.setBounds(5, 25, 111, 14);
			jLabel_ROIdefaultLow.setFont(new java.awt.Font("Dialog",0,10));
			jLabel_ROIdefaultLow.setHorizontalAlignment(SwingConstants.RIGHT);
			jLabel_ROIdefaultType = new JLabel();
			jLabel_ROIdefaultType.setText("default type:");
			jLabel_ROIdefaultType.setBounds(203, 25, 79, 14);
			jLabel_ROIdefaultType.setFont(new java.awt.Font("Dialog",0,10));
			jLabel_ROIdefaultType.setHorizontalAlignment(SwingConstants.RIGHT);
			jPanel2 = new JPanel();
			jPanel2.setBounds(7, 65, 487, 81);
			jPanel2.setLayout(null);
			jPanel2.setBorder(BorderFactory.createTitledBorder("Default values"));
			jPanel2.add(jLabel_ROIdefaultType, null);
			jPanel2.add(jLabel_ROIdefaultLow, null);
			jPanel2.add(jLabel_ROIdefaultHigh, null);
			jPanel2.add(getJComboBox_ROIdefaultType(), null);
			jPanel2.add(getJTextField_ROIdefaultLow(), null);
			jPanel2.add(getJTextField_ROIdefaultHigh(), null);
		}
		return jPanel2;
	}

	/**
	 * This method initializes jComboBox_ROIdefaultType	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getJComboBox_ROIdefaultType() {
		if (jComboBox_ROIdefaultType == null) {
			jComboBox_ROIdefaultType = new JComboBox();
			jComboBox_ROIdefaultType.setModel(new DefaultComboBoxModel(roiTableTypes));
			jComboBox_ROIdefaultType.setBounds(286, 22, 83, 20);
			jComboBox_ROIdefaultType.setSelectedIndex(1);
			jComboBox_ROIdefaultType.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					int answer = JOptionPane.showConfirmDialog(jComboBox_ROIdefaultType.getParent(), "All current values in the datatype column will be overwritten. Are you sure you want to go on?","Overwrite tablecolumn with defined default value", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
					if (answer == JOptionPane.YES_OPTION){
						String val = jComboBox_ROIdefaultType.getSelectedItem().toString();
						for(int i=0;i<roiTableMaxDim;i++)
							jTable_ROI.setValueAt(val, i, 3);
						updateTableDim(roiTableDim);
					}
				}
			});
		}
		return jComboBox_ROIdefaultType;
	}

	/**
	 * This method initializes jTextField_ROIdefaultLow	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_ROIdefaultLow() {
		if (jTextField_ROIdefaultLow == null) {
			jTextField_ROIdefaultLow = new JTextField();
			jTextField_ROIdefaultLow.setText("0");
			jTextField_ROIdefaultLow.setBounds(120, 22, 83, 20);
			jTextField_ROIdefaultLow.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String val = jTextField_ROIdefaultLow.getText();
					if (isRegExFloat(val)!=null || isRegExInt(val)!=null || hlp.isRegExPosInf(val) || hlp.isRegExNegInf(val)){
						int answer = JOptionPane.showConfirmDialog(jTextField_ROIdefaultLow.getParent(), "All current values in the lower bound column will be overwritten. Are you sure you want to go on?","Overwrite tablecolumn with defined default value", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
						if (answer == JOptionPane.YES_OPTION){
							for(int i=0;i<roiTableMaxDim;i++)
								jTable_ROI.setValueAt(val, i, 1);
							updateTableDim(roiTableDim);
							jTextField_ROIdefaultLow.setText((String)jTable_ROI.getValueAt(0, 1));
						}
					}else{
						JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "Type of value have to be INT or FLOAT. Other allowed values are: Inf and -Inf." + "   (Invalid value was: "+val.toString()+")", "Value type error", JOptionPane.ERROR_MESSAGE);
						jTextField_ROIdefaultLow.setText("");
					}
				}
			});
		}
		return jTextField_ROIdefaultLow;
	}

	/**
	 * This method initializes jTextField_ROIdefaultHigh	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_ROIdefaultHigh() {
		if (jTextField_ROIdefaultHigh == null) {
			jTextField_ROIdefaultHigh = new JTextField();
			jTextField_ROIdefaultHigh.setText("10");
			jTextField_ROIdefaultHigh.setBounds(120, 48, 83, 20);
			jTextField_ROIdefaultHigh.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String val = jTextField_ROIdefaultHigh.getText();
					if (isRegExFloat(val)!=null || isRegExInt(val)!=null || hlp.isRegExPosInf(val) || hlp.isRegExNegInf(val)){
						int answer = JOptionPane.showConfirmDialog(jTextField_ROIdefaultHigh.getParent(), "All current values in the upper bound column will be overwritten. Are you sure you want to go on?","Overwrite tablecolumn with defined default value", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
						if (answer == JOptionPane.YES_OPTION){
							for(int i=0;i<roiTableMaxDim;i++)
								jTable_ROI.setValueAt(val, i, 2);
							updateTableDim(roiTableDim);
							jTextField_ROIdefaultHigh.setText((String)jTable_ROI.getValueAt(0, 2));
						}
					}else{
						JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "Type of value have to be INT or FLOAT. Other allowed values are: Inf and -Inf." + "   (Invalid value was: "+val.toString()+")", "Value type error", JOptionPane.ERROR_MESSAGE);
						jTextField_ROIdefaultHigh.setText("");
					}
				}
			});
		}
		return jTextField_ROIdefaultHigh;
	}

	/**
	 * This method initializes jPanel_spoconf	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_spoconf() {
		if (jPanel_spoconf == null) {
			jPanel_spoconf = new JPanel();
			jPanel_spoconf.setLayout(null);
			jPanel_spoconf.add(getJScrollPane2(), null);
			jPanel_spoconf.add(getJPanel_CONFstep(), null);
			jPanel_spoconf.add(getJPanel3(), null);
			jPanel_spoconf.add(getJCheckBox_CONFadvanced(), null);
			jPanel_spoconf.add(getJPanel_CONFproject(), null);
			jPanel_spoconf.add(getAutoConfigPanel(), null);
		}
		return jPanel_spoconf;
	}

	/**
	 * This method initializes jScrollPane2	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane2() {
		if (jScrollPane2 == null) {
			jScrollPane2 = new JScrollPane();
			jScrollPane2.setBounds(110, 234, 381, 182);
			jScrollPane2.setViewportView(getJTextArea_CONFadditional());
			jScrollPane2.getHorizontalScrollBar().addAdjustmentListener(new AdjustmentListener(){
				public void adjustmentValueChanged(AdjustmentEvent e) {
					jTextArea_CONFadditional.repaint();
				}
			});
			jScrollPane2.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
				public void adjustmentValueChanged(AdjustmentEvent e) {
					jTextArea_CONFadditional.repaint();
				}
			});
		}
		return jScrollPane2;
	}

	/**
	 * This method initializes jTextArea_CONFadditional	
	 * 	
	 * @return javax.swing.JTextArea	
	 */
	private JTextArea getJTextArea_CONFadditional() {
		if (jTextArea_CONFadditional == null) {
			jTextArea_CONFadditional = new JTextArea();
			jTextArea_CONFadditional.setText("## initial\ninit.design.retries = 100;\n## sequential\nseq.design.func = \"spotCreateDesignLhs\";\nseq.design.oldBest.size = 0;\nseq.design.new.size = 2;\nseq.design.retries = 10;\nseq.transformation.func = I;\nseq.merge.func = mean;\n## overall\nspot.seed = 1000;\nspot.ocba  = F;\nio.verbosity = 3");
			jTextArea_CONFadditional.setCaretPosition(0);
			jTextArea_CONFadditional.setEnabled(false);
			jTextArea_CONFadditional.setFont(new java.awt.Font("Dialog",0,13));
		}
		return jTextArea_CONFadditional;
	}

	/**
	 * This method initializes jPanel_CONFstep	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_CONFstep() {
		if (jPanel_CONFstep == null) {
			jLabel_CONFdesignType = new JLabel();
			jLabel_CONFdesignType.setText("Design Function:");
			jLabel_CONFdesignType.setBounds(175, 20, 112, 16);
			jLabel_CONFdesignType.setHorizontalAlignment(SwingConstants.RIGHT);
			jLabel_CONFinitDesignPoints = new JLabel();
			jLabel_CONFinitDesignPoints.setText("Design Size:");
			jLabel_CONFinitDesignPoints.setBounds(3, 20, 105, 16);
			jLabel_CONFinitDesignPoints.setHorizontalAlignment(SwingConstants.RIGHT);
			jPanel_CONFstep = new JPanel();
			jPanel_CONFstep.setBounds(7, 81, 485, 45);
			jPanel_CONFstep.setBorder(BorderFactory.createTitledBorder("Initial config"));
			jPanel_CONFstep.setLayout(null);
			jPanel_CONFstep.add(jLabel_CONFinitDesignPoints, null);
			jPanel_CONFstep.add(getJTextField_CONFinitLhdPoints(), null);
			jPanel_CONFstep.add(getJComboBox_CONFdesignType(), null);
			jPanel_CONFstep.add(jLabel_CONFdesignType, null);
		}
		return jPanel_CONFstep;
	}

	/**
	 * This method initializes jTextField_CONFinitLhdPoints	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_CONFinitLhdPoints() {
		if (jTextField_CONFinitLhdPoints == null) {
			jTextField_CONFinitLhdPoints = new JTextField();
			jTextField_CONFinitLhdPoints.setText("NA");
			jTextField_CONFinitLhdPoints.setBounds(110, 16, 60, 23);
		}
		return jTextField_CONFinitLhdPoints;
	}

	/**
	 * This method initializes jComboBox_CONFdesignType	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getJComboBox_CONFdesignType() {
		if (jComboBox_CONFdesignType == null) {
			jComboBox_CONFdesignType = new JComboBox();
			ComboBoxModel jComboBox_CONFdesignTypeModel = 
				new DefaultComboBoxModel(designTypeStrings);
			jComboBox_CONFdesignType.setModel(jComboBox_CONFdesignTypeModel);
			jComboBox_CONFdesignType.setSelectedIndex(2); //LHD design as default
			jComboBox_CONFdesignType.setBounds(292, 16, 183, 23);
		}
		return jComboBox_CONFdesignType;
	}

	/**
	 * This method initializes jPanel3	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel3() {
		if (jPanel3 == null) {
			jLabel_CONFmodel = new JLabel();
			jLabel_CONFmodel.setText("Prediction Model:");
			jLabel_CONFmodel.setBounds(175, 22, 112, 16);
			jLabel_CONFmodel.setHorizontalAlignment(SwingConstants.RIGHT);
			jLabel_CONFseqDesignSize = new JLabel();
			jLabel_CONFseqDesignSize.setText("Predicted Points:");
			jLabel_CONFseqDesignSize.setBounds(3, 22, 105, 16);
			jLabel_CONFseqDesignSize.setHorizontalAlignment(SwingConstants.RIGHT);
			jLabel_CONFautoLoopNEvals = new JLabel();
			jLabel_CONFautoLoopNEvals.setText("Loop Evaluations:");
			jLabel_CONFautoLoopNEvals.setBounds(new Rectangle(204, 26, 113, 14));
			jLabel_CONFautoLoopNEvals.setHorizontalAlignment(SwingConstants.RIGHT);
			jLabel_CONFautoLoopSteps = new JLabel();
			jLabel_CONFautoLoopSteps.setText("Loop Steps:");
			jLabel_CONFautoLoopSteps.setBounds(new Rectangle(9, 26, 87, 14));
			jLabel_CONFautoLoopSteps.setHorizontalAlignment(SwingConstants.RIGHT);
			jPanel3 = new JPanel();
			jPanel3.setBounds(7, 126, 486, 49);
			jPanel3.setBorder(BorderFactory.createTitledBorder("Sequential config"));
			jPanel3.setLayout(null);
			jPanel3.add(getJTextField_CONFseqDesignSize(), null);
			jPanel3.add(jLabel_CONFseqDesignSize, null);
			jPanel3.add(jLabel_CONFmodel, null);
			jPanel3.add(getJComboBox_CONFmodel(), null);
		}
		return jPanel3;
	}

	/**
	 * This method initializes jTextField_CONFautoLoopSteps	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_CONFautoLoopSteps() {
		if (jTextField_CONFautoLoopSteps == null) {
			jTextField_CONFautoLoopSteps = new JTextField();
			jTextField_CONFautoLoopSteps.setText("Inf");
			jTextField_CONFautoLoopSteps.setBounds(new Rectangle(100, 21, 87, 23));
		}
		return jTextField_CONFautoLoopSteps;
	}

	/**
	 * This method initializes jTextField_CONFautoLoopNEvals	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_CONFautoLoopNEvals() {
		if (jTextField_CONFautoLoopNEvals == null) {
			jTextField_CONFautoLoopNEvals = new JTextField();
			jTextField_CONFautoLoopNEvals.setText("100");
			jTextField_CONFautoLoopNEvals.setBounds(new Rectangle(321, 21, 87, 23));
		}
		return jTextField_CONFautoLoopNEvals;
	}

	/**
	 * This method initializes jTextField_CONFseqDesignSize	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_CONFseqDesignSize() {
		if (jTextField_CONFseqDesignSize == null) {
			jTextField_CONFseqDesignSize = new JTextField();
			jTextField_CONFseqDesignSize.setText("200");
			jTextField_CONFseqDesignSize.setBounds(110, 19, 60, 23);
		}
		return jTextField_CONFseqDesignSize;
	}

	/**
	 * This method initializes jComboBox_CONFmodel	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getJComboBox_CONFmodel() {
		if (jComboBox_CONFmodel == null) {
			jComboBox_CONFmodel = new JComboBox();
			ComboBoxModel jComboBox_CONFmodelModel = 
				new DefaultComboBoxModel(modelStrings);							
			jComboBox_CONFmodel.setModel(jComboBox_CONFmodelModel);
			jComboBox_CONFmodel.setSelectedIndex(0); //Tree Model as default
			jComboBox_CONFmodel.setBounds(292, 17, 183, 23);
		}
		return jComboBox_CONFmodel;
	}

	/**
	 * This method initializes jCheckBox_CONFadvanced	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getJCheckBox_CONFadvanced() {
		if (jCheckBox_CONFadvanced == null) {
			jCheckBox_CONFadvanced = new JCheckBox();
			jCheckBox_CONFadvanced.setText("<html>Enable<br>advanced<br>config<br>setup</html>");
			jCheckBox_CONFadvanced.setBounds(19, 289, 74, 103);
			jCheckBox_CONFadvanced.setHorizontalAlignment(SwingConstants.CENTER);
			jCheckBox_CONFadvanced.setFont(new java.awt.Font("Dialog",0,11));
			jCheckBox_CONFadvanced.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if(jCheckBox_CONFadvanced.isSelected()){
						jTextArea_CONFadditional.setEnabled(true);
					}else{
						jTextArea_CONFadditional.setEnabled(false);
					}
				}
			});
		}
		return jCheckBox_CONFadvanced;
	}

	/**
	 * This method initializes jPanel_CONFproject	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_CONFproject() {
		if (jPanel_CONFproject == null) {
			jLabel_CONFprojectPrefix = new JLabel();
			jLabel_CONFprojectPrefix.setText("Projectname:");
			jLabel_CONFprojectPrefix.setHorizontalAlignment(SwingConstants.RIGHT);
			jLabel_CONFprojectPrefix.setBounds(265, 24, 78, 14);
			jLabel_CONFprojectPath = new JLabel();
			jLabel_CONFprojectPath.setText("Projectpath:");
			jLabel_CONFprojectPath.setHorizontalAlignment(SwingConstants.RIGHT);
			jLabel_CONFprojectPath.setFont(new java.awt.Font("Dialog",0,10));
			jLabel_CONFprojectPath.setBounds(17, 49, 62, 14);
			jPanel_CONFproject = new JPanel();
			jPanel_CONFproject.setBounds(7, 5, 485, 76);
			jPanel_CONFproject.setBorder(BorderFactory.createTitledBorder("Project config"));
			jPanel_CONFproject.setLayout(null);
			jPanel_CONFproject.add(getJTextField_CONFprojectPath(), null);
			jPanel_CONFproject.add(getJButton_CONFprojectPathBrowse(), null);
			jPanel_CONFproject.add(jLabel_CONFprojectPath, null);
			jPanel_CONFproject.add(getJTextField_CONFprojectPrefix(), null);
			jPanel_CONFproject.add(jLabel_CONFprojectPrefix, null);
			jPanel_CONFproject.add(getJCheckBox_CONFprojectPathSubfolder(), null);
		}
		return jPanel_CONFproject;
	}

	/**
	 * This method initializes jTextField_CONFprojectPath	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_CONFprojectPath() {
		if (jTextField_CONFprojectPath == null) {
			jTextField_CONFprojectPath = new JTextField();
			jTextField_CONFprojectPath.setEditable(false);
			jTextField_CONFprojectPath.setBounds(79, 46, 292, 20);
		}
		return jTextField_CONFprojectPath;
	}

	/**
	 * This method initializes jButton_CONFprojectPathBrowse	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_CONFprojectPathBrowse() {
		if (jButton_CONFprojectPathBrowse == null) {
			jButton_CONFprojectPathBrowse = new JButton();
			jButton_CONFprojectPathBrowse.setText("Browse");
			jButton_CONFprojectPathBrowse.setBounds(377, 44, 91, 22);
			jButton_CONFprojectPathBrowse.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					JFileChooser projectPathFC = getJFileChooser_CONFprojectPath();
					int ret = projectPathFC.showDialog(jButton_CONFprojectPathBrowse.getParent(), "Set project path");
					if (ret == JFileChooser.APPROVE_OPTION){
						pathFCDefaultCONFproject = projectPathFC.getSelectedFile();
						if(jCheckBox_CONFprojectPathSubfolder.isSelected() && !jTextField_CONFprojectPrefix.getText().equals("") && hlp.validPref(jTextField_CONFprojectPrefix.getText())){
							jTextField_CONFprojectPath.setText(projectPathFC.getSelectedFile().toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText());
						}else{
							jTextField_CONFprojectPath.setText(pathFCDefaultCONFproject.toString());
						}
						path_Project = new File(jTextField_CONFprojectPath.getText());
					}
				}
			});
		}
		return jButton_CONFprojectPathBrowse;
	}

	/**
	 * This method initializes jTextField_CONFprojectPrefix	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_CONFprojectPrefix() {
		if (jTextField_CONFprojectPrefix == null) {
			jTextField_CONFprojectPrefix = new JTextField();
			jTextField_CONFprojectPrefix.setFont(new java.awt.Font("Dialog",1,11));
			jTextField_CONFprojectPrefix.setText("demo0001");
			jTextField_CONFprojectPrefix.setBounds(349, 22, 119, 20);
			jTextField_CONFprojectPrefix.addKeyListener(new KeyAdapter() {
				public void keyTyped(KeyEvent e) {}
				public void keyReleased(KeyEvent e) {
					if(hlp.validPref(jTextField_CONFprojectPrefix.getText())){
						jTextField_CONFprojectPrefix.setBackground(Color.white);
						if(jCheckBox_CONFprojectPathSubfolder.isSelected() && !jTextField_CONFprojectPath.getText().equals("")){
							jTextField_CONFprojectPath.setText(pathFCDefaultCONFproject.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText());
						}
					}else{
						jTextField_CONFprojectPrefix.setBackground(Color.red);
					}
				}
				public void keyPressed(KeyEvent e) {}
			});
			jTextField_CONFprojectPrefix.addFocusListener(new FocusAdapter() {
				public void focusLost(FocusEvent e) {
					if(hlp.validPref(jTextField_CONFprojectPrefix.getText())){
						jTextField_CONFprojectPrefix.setBackground(Color.white);
						path_Project = new File(jTextField_CONFprojectPath.getText());
					}else
						JOptionPane.showMessageDialog(jTextField_CONFprojectPrefix.getParent(), "Wrong name format. Allowed characters are: A-Z, a-z, _ and 0-9. Note, that the name have to start with a letter. Please select a proper name." + "   (Invalid name is: \""+jTextField_CONFprojectPrefix.getText()+"\")", "Value type error", JOptionPane.ERROR_MESSAGE);
				}
				public void focusGained(FocusEvent e) {}
			});
		}
		return jTextField_CONFprojectPrefix;
	}

	/**
	 * This method initializes jCheckBox_CONFprojectPathSubfolder	
	 * 	
	 * @return javax.swing.JCheckBox	
	 */
	private JCheckBox getJCheckBox_CONFprojectPathSubfolder() {
		if (jCheckBox_CONFprojectPathSubfolder == null) {
			jCheckBox_CONFprojectPathSubfolder = new JCheckBox();
			jCheckBox_CONFprojectPathSubfolder.setText("<html>Create subfolder for project</html>");
			jCheckBox_CONFprojectPathSubfolder.setFont(new java.awt.Font("Dialog",0,10));
			jCheckBox_CONFprojectPathSubfolder.setBounds(13, 24, 171, 17);
			jCheckBox_CONFprojectPathSubfolder.setHorizontalAlignment(SwingConstants.LEFT);
			jCheckBox_CONFprojectPathSubfolder.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if(!jTextField_CONFprojectPath.getText().equals("")){
						if(jCheckBox_CONFprojectPathSubfolder.isSelected() && !path_Project.toString().equals("")){
							if(path_Project.toString().endsWith(FILE_SEPERATOR)){//case if e.g. path="C:\" (without this IF it would be ==> "C:\\demo0001" and not "C:\demo0001")												
								jTextField_CONFprojectPath.setText(path_Project.toString()+jTextField_CONFprojectPrefix.getText());
								path_Project = new File(jTextField_CONFprojectPath.getText());
							}else{
								jTextField_CONFprojectPath.setText(path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText());
								path_Project = new File(jTextField_CONFprojectPath.getText());
							}
						}else{
							jTextField_CONFprojectPath.setText(pathFCDefaultCONFproject.toString());
							path_Project = pathFCDefaultCONFproject;
						}
					}
				}
			});
		}
		return jCheckBox_CONFprojectPathSubfolder;
	}

	/**
	 * This method initializes jPanel_run	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_run() {
		if (jPanel_run == null) {
			jPanel_run = new JPanel();
			jPanel_run.setLayout(null);
			jPanel_run.add(getJPanel6(), null);
			jPanel_run.add(getJPanel_checkSpotRun(), null);
			jPanel_run.add(getJPanel1_SpotRun(), null);
			jPanel_run.add(getJScrollPane3(), null);
		}
		return jPanel_run;
	}

	/**
	 * This method initializes jPanel6	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel6() {
		if (jPanel6 == null) {
			jLabel3 = new JLabel();
			jLabel3.setText("= invalid/error");
			jLabel3.setIcon(iconCheckInvalid);
			jLabel3.setBounds(266, 21, 118, 17);
			jLabel2 = new JLabel();
			jLabel2.setText("= unchecked");
			jLabel2.setIcon(iconCheckUnknown);
			jLabel2.setBounds(141, 21, 119, 17);
			jLabel1 = new JLabel();
			jLabel1.setText("= valid");
			jLabel1.setIcon(iconCheckValid);
			jLabel1.setBounds(17, 21, 118, 17);
			jPanel6 = new JPanel();
			jPanel6.setBorder(BorderFactory.createTitledBorder("Information"));
			jPanel6.setBounds(7, 232, 489, 51);
			jPanel6.setLayout(null);
			jPanel6.add(jLabel1, null);
			jPanel6.add(jLabel2, null);
			jPanel6.add(jLabel3, null);
		}
		return jPanel6;
	}

	/**
	 * This method initializes jPanel_checkSpotRun	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_checkSpotRun() {
		if (jPanel_checkSpotRun == null) {
			jLabel_checkSpotRun_roiConfigFile = new JLabel();
			jLabel_checkSpotRun_roiConfigFile.setText("Write ROI config file");
			jLabel_checkSpotRun_roiConfigFile.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_roiConfigFile.setBounds(156, 21, 131, 15);
			jLabel_checkSpotRun_spoConfigFile = new JLabel();
			jLabel_checkSpotRun_spoConfigFile.setText("Write SPO config file");
			jLabel_checkSpotRun_spoConfigFile.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_spoConfigFile.setBounds(156, 42, 131, 15);
			jLabel_checkSpotRun_rPath = new JLabel();
			jLabel_checkSpotRun_rPath.setText("R Path");
			jLabel_checkSpotRun_rPath.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_rPath.setBounds(13, 84, 131, 15);
			jLabel_checkSpotRun_spoConfig = new JLabel();
			jLabel_checkSpotRun_spoConfig.setText("SPO config");
			jLabel_checkSpotRun_spoConfig.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_spoConfig.setBounds(13, 63, 131, 15);
			jLabel_checkSpotRun_roiConfig = new JLabel();
			jLabel_checkSpotRun_roiConfig.setText("ROI config");
			jLabel_checkSpotRun_roiConfig.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_roiConfig.setBounds(13, 42, 131, 15);
			jLabel_checkSpotRun_algorithm = new JLabel();
			jLabel_checkSpotRun_algorithm.setText("Algorithm script");
			jLabel_checkSpotRun_algorithm.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_algorithm.setBounds(13, 21, 131, 15);
			jPanel_checkSpotRun = new JPanel();
			jPanel_checkSpotRun.setBounds(7, 283, 329, 132);
			jPanel_checkSpotRun.setBorder(BorderFactory.createTitledBorder("Check list"));
			jPanel_checkSpotRun.setLayout(null);
			jPanel_checkSpotRun.add(jLabel_checkSpotRun_algorithm, null);
			jPanel_checkSpotRun.add(jLabel_checkSpotRun_roiConfig, null);
			jPanel_checkSpotRun.add(jLabel_checkSpotRun_spoConfig, null);
			jPanel_checkSpotRun.add(jLabel_checkSpotRun_rPath, null);
			jPanel_checkSpotRun.add(jLabel_checkSpotRun_spoConfigFile, null);
			jPanel_checkSpotRun.add(jLabel_checkSpotRun_roiConfigFile, null);
			jPanel_checkSpotRun.add(getJButton_checkSpotRun(), null);
		}
		return jPanel_checkSpotRun;
	}

	/**
	 * This method initializes jButton_checkSpotRun	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_checkSpotRun() {
		if (jButton_checkSpotRun == null) {
			jButton_checkSpotRun = new JButton();
			jButton_checkSpotRun.setText("<html><center>Check & write config files</center></html>");
			jButton_checkSpotRun.setBounds(185, 73, 117, 42);
			jButton_checkSpotRun.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if(checkSPO()!=0){			// ERROR: ...										
						jLabel_taskSelect.setEnabled(false);
						jComboBox_taskSelect.setEnabled(false);
						jButton_spotRun.setEnabled(false);
					}else{						// ALL CHECKED AND OK										
						jLabel_taskSelect.setEnabled(true);
						jComboBox_taskSelect.setEnabled(true);
						jButton_spotRun.setEnabled(true);
					}
				}
			});
		}
		return jButton_checkSpotRun;
	}

	/**
	 * This method initializes jPanel1_SpotRun	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel1_SpotRun() {
		if (jPanel1_SpotRun == null) {
			jLabel_taskSelect = new JLabel();
			jLabel_taskSelect.setText("Select Task:");
			jLabel_taskSelect.setBounds(17, 22, 94, 14);			
			jLabel_taskSelect.setEnabled(false);
			jPanel1_SpotRun = new JPanel();
			jPanel1_SpotRun.setBounds(336, 283, 160, 132);
			jPanel1_SpotRun.setBorder(BorderFactory.createTitledBorder("Start SPOT"));
			jPanel1_SpotRun.setLayout(null);
			jPanel1_SpotRun.add(jLabel_taskSelect, null);
			jPanel1_SpotRun.add(getJComboBox_taskSelect(), null);
			jPanel1_SpotRun.add(getJButton_spotRun(), null);
		}
		return jPanel1_SpotRun;
	}

	/**
	 * This method initializes jComboBox_taskSelect	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getJComboBox_taskSelect() {
		if (jComboBox_taskSelect == null) {
			jComboBox_taskSelect = new JComboBox();
			ComboBoxModel jComboBox_taskSelectModel = 
				new DefaultComboBoxModel(
						new String[] { "init", "run", "seq", "auto", "rep"});				
			jComboBox_taskSelect.setModel(jComboBox_taskSelectModel);
			jComboBox_taskSelect.setBounds(17, 36, 117, 20);
			jComboBox_taskSelect.setEnabled(false);
			jComboBox_taskSelect.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					if(!jButton_spotRun.isEnabled())
						jButton_spotRun.setEnabled(true);
				}
			});
		}
		return jComboBox_taskSelect;
	}

	/**
	 * This method initializes jButton_spotRun	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_spotRun() {
		if (jButton_spotRun == null) {
			jButton_spotRun = new JButton();
			jButton_spotRun.setText("<html><center>Start SPOT</center></html>");
			jButton_spotRun.setBounds(17, 73, 117, 42);
			jButton_spotRun.setEnabled(false);
			jButton_spotRun.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String RscriptExe = hlp.strQuote(pathFile_R.getParent()+FILE_SEPERATOR+"Rscript.exe"); 
					String confString = pathFile_ProjectConf.toString();//???
					String taskString = jComboBox_taskSelect.getSelectedItem().toString();
					String tempString1 = path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+".R";
					String tempString2 = path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+".Rout";
					String[] cmdWINDOWS = {
							"cmd", "/C","start", "\"\"","/wait",
							RscriptExe,
							hlp.strQuote(tempString1)//, //File that is called with Rcmd.exe or Rscript.exe
					};						
					String cmdLINUX[] ={
							"xterm",
							"-exec",
							"R --slave --args "+" <"+ hlp.strQuote(tempString1)
					};
					String cmdCallString[]; //String that's actually used to call SPOT
					fileIO.writeRSpot(confString.replace('\\','/'),taskString,tempString1,tempString2.replace('\\','/'),pathFile_AlgorithmScript.toString().replace('\\','/')); //Writes a script file with the assigned spot tastk and config to be started by R
					if (IS_OS_WINDOWS)	//Check Operating System and use cmd String accordingly
						cmdCallString=cmdWINDOWS;
					else if(IS_OS_LINUX) 
						cmdCallString=cmdLINUX;
					else if(IS_OS_MAC) 
						cmdCallString=cmdLINUX;
					else
						cmdCallString=cmdLINUX;									
					jColorPane_spotRun.setText("");
					jColorPane_spotRun.append(Color.blue, "[*** STARTING SPOT ***]");
					jColorPane_spotRun.append(Color.blue, "\n\nExecuting command String[]:\n");
					for(int j=0;j<cmdCallString.length;j++){
						jColorPane_spotRun.append(Color.blue, "...cmd["+j+"] = ");
						jColorPane_spotRun.append(Color.black, cmdCallString[j]+"\n");
					}
					jColorPane_spotRun.append(Color.blue, "\nSPOT task is starting now, please stand by and wait for SPOT to finish");
					jButton_spotRun.setEnabled(false);							
					try {	//Now start SPOT/R										
						Process p = Runtime.getRuntime().exec(cmdCallString);
						p.waitFor();//After this the logfile in tempString2 will be read, and posted into the colorpane
						//TODO: Error Abfrage wenn kein File geschrieben wurde, oder leer ist mit entsprechender Roter Fehlermeldung
						StringBuffer contents = fileIO.getTxt(tempString2); //Read content of logfile (spotOut.txt)
						jColorPane_spotRun.append(Color.blue, "\n");
						jColorPane_spotRun.append(Color.black, contents.toString());
						jColorPane_spotRun.append(Color.blue, "\nThis SPOT task is finished");
						jButton_spotRun.setEnabled(true);										
					}catch (Exception err){
						err.printStackTrace();
					}
				}
			});
		}
		return jButton_spotRun;
	}
	/**
	 * This method initializes jScrollPane3	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane3() {
		if (jScrollPane3 == null) {
			jScrollPane3 = new JScrollPane();
			jScrollPane3.setBounds(9, 11, 486, 220);
			jScrollPane3.setEnabled(false);
			jScrollPane3.setAutoscrolls(true);
			{	jColorPane_spotRun = new ColorPane();
				jScrollPane3.setViewportView(jColorPane_spotRun);
				jColorPane_spotRun.setBounds(12, 12, 480, 220);
				jColorPane_spotRun.setEditable(false);
				jScrollPane3.getHorizontalScrollBar().addAdjustmentListener(new AdjustmentListener(){
					public void adjustmentValueChanged(AdjustmentEvent e) {
						jColorPane_spotRun.repaint();
					}
				});
				jScrollPane3.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
					public void adjustmentValueChanged(AdjustmentEvent e) {
						jColorPane_spotRun.repaint();
					}
				});
			}
		}
		return jScrollPane3;
	}

	/**
	 * This method initializes jPanel_analyze	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_analyze() {
		if (jPanel_analyze == null) {
			jPanel_analyze = new JPanel();
			jPanel_analyze.setLayout(null);
			jPanel_analyze.add(getJScrollPane4(), null);
			jPanel_analyze.add(getJPanel_viewFiles(), null);			
		}
		return jPanel_analyze;
	}

	/**
	 * This method initializes jScrollPane4	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane4() {
		if (jScrollPane4 == null) {
			jScrollPane4 = new JScrollPane();
			jScrollPane4.setBounds(9, 11, 486, 315);
			jScrollPane4.setEnabled(false);
			jScrollPane4.setAutoscrolls(true);
			{
				jColorPane_analyzeRun = new ColorPane();
				jScrollPane4.setViewportView(jColorPane_analyzeRun);
				jColorPane_analyzeRun.setBounds(12, 12, 480, 220);
				jColorPane_analyzeRun.setEditable(false);
				jScrollPane4.getHorizontalScrollBar().addAdjustmentListener(new AdjustmentListener(){
					public void adjustmentValueChanged(AdjustmentEvent e) {
						jColorPane_analyzeRun.repaint();
					}
				});
				jScrollPane4.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener(){
					public void adjustmentValueChanged(AdjustmentEvent e) {
						jColorPane_analyzeRun.repaint();
					}
				});
			}
		}
		return jScrollPane4;
	}

	/**
	 * This method initializes jPanel_viewFiles	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel_viewFiles() {
		if (jPanel_viewFiles == null) {
			jPanel_viewFiles = new JPanel();
			jPanel_viewFiles.setBounds(7, 332, 485, 83);
			jPanel_viewFiles.setBorder(BorderFactory.createTitledBorder("View Project And Result Files"));
			jPanel_viewFiles.setLayout(null);
			jPanel_viewFiles.add(getJComboBox_ViewRawFile(), null);
			jPanel_viewFiles.add(getJComboBox_ViewPdfFile(), null);
			jPanel_viewFiles.add(getJButton_viewRawFile(), null);
			jPanel_viewFiles.add(getJButton_viewPdfFile(), null);
		}
		return jPanel_viewFiles;
	}

	/**
	 * This method initializes jComboBox_ViewRawFile	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getJComboBox_ViewRawFile() {
		if (jComboBox_ViewRawFile == null) {
			jComboBox_ViewRawFile = new JComboBox();
			ComboBoxModel Model_RawFiles = 
				new DefaultComboBoxModel(
						new String[] { ".conf SPOT configuration",".apd algorithm problem definition",".roi region of interest",".des experiment design",".res list of results",".bst list of best results",".R script called by R to call SPOT",".Rout logfile of the SPOT run, see report output here"});
			jComboBox_ViewRawFile.setModel(Model_RawFiles);
			jComboBox_ViewRawFile.setSelectedIndex(0);
			jComboBox_ViewRawFile.setBounds(159, 20, 309, 23);
		}
		return jComboBox_ViewRawFile;
	}

	/**
	 * This method initializes jComboBox_ViewPdfFile	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getJComboBox_ViewPdfFile() {
		if (jComboBox_ViewPdfFile == null) {
			jComboBox_ViewPdfFile = new JComboBox();
			ComboBoxModel Model_PdfFiles = 
				new DefaultComboBoxModel(
						new String[] {"Final Report","Ongoing Report"});
			jComboBox_ViewPdfFile.setModel(Model_PdfFiles);
			jComboBox_ViewPdfFile.setSelectedIndex(0);
			jComboBox_ViewPdfFile.setBounds(159, 50, 309, 23);
		}
		return jComboBox_ViewPdfFile;
	}

	/**
	 * This method initializes jButton_viewRawFile	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_viewRawFile() {
		if (jButton_viewRawFile == null) {
			jButton_viewRawFile = new JButton();
			jButton_viewRawFile.setText("<html><center>View Raw File</center></html>");
			jButton_viewRawFile.setBounds(10, 20, 138, 23);
			jButton_viewRawFile.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					//If this button is pressed, the gui will present the content of the chosen project file in the colorpane of this tab
					String tarSuffix=".conf";
					switch(jComboBox_ViewRawFile.getSelectedIndex())
					{
					case 0:tarSuffix=".conf";break;										
					case 1:tarSuffix=".apd";break;							
					case 2:tarSuffix=".roi";break;
					case 3:tarSuffix=".des";break;
					case 4:tarSuffix=".res";break;
					case 5:tarSuffix=".bst";break;
					case 6:tarSuffix=".R";break;
					case 7:tarSuffix=".Rout";break;
					}
					String tarFile="";
					if(tarSuffix.equals(".apd"))
						tarFile = jTextField_CONFalgPdFilePath.getText();//path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+tarSuffix;
					else
						tarFile = path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+tarSuffix;
					if(new File(tarFile).exists())									{
						StringBuffer contents = fileIO.getTxt(tarFile); //Read content of logfile (spotOut.txt)
						jColorPane_analyzeRun.setText("");
						jColorPane_analyzeRun.append(Color.blue, "\nContent of file:"+tarFile);
						jColorPane_analyzeRun.append(Color.blue, "\n\n");
						jColorPane_analyzeRun.append(Color.black, contents.toString());
						jColorPane_analyzeRun.append(Color.blue, "\nEnd of file: "+tarFile);
					}
					else{
						jColorPane_analyzeRun.setText("");
						jColorPane_analyzeRun.append(Color.red, "\nFile does not exist: "+tarFile);
					}
				}
			});
		}
		return jButton_viewRawFile;
	}

	/**
	 * This method initializes jButton_viewPdfFile	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_viewPdfFile() {
		if (jButton_viewPdfFile == null) {
			jButton_viewPdfFile = new JButton();
			jButton_viewPdfFile.setText("<html><center>View PDF File</center></html>");
			jButton_viewPdfFile.setBounds(10, 50, 138, 23);
			jButton_viewPdfFile.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String pdfFileString="";
					if(jComboBox_ViewPdfFile.getSelectedIndex()==1)
						pdfFileString=path_Project.toString()+FILE_SEPERATOR+"Rplots.pdf";//Ongoing report
					else
						pdfFileString=path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+".pdf";//Final report
					File pdfFile = new File(pdfFileString);
					if(pdfFile.exists())
					{
						try {//JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "PDF file will be opened in your default viewer.", "starting viewer", JOptionPane.INFORMATION_MESSAGE);
							Desktop.getDesktop().browse(pdfFile.toURI());
						}catch (IOException e1){e1.printStackTrace();}
					}
					else
					{
						jColorPane_analyzeRun.setText("");
						jColorPane_analyzeRun.append(Color.red, "\nFile does not exist: "+pdfFileString);
					}
				}
			});
		}
		return jButton_viewPdfFile;
	}
	/**
	 * This method initializes jScrollPane5	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane5() {
		if (jScrollPane5 == null) {
			jScrollPane5 = new JScrollPane();
			jScrollPane5.setBounds(5, 20, 478, 380);
			jScrollPane5.setViewportView(getJJEditorPane_Intro());
		}
		return jScrollPane5;
	}

	/**
	 * This method initializes jJEditorPane_Intro	
	 * 	
	 * @return javax.swing.JEditorPane	
	 */
	private JEditorPane getJJEditorPane_Intro() {
		if (jJEditorPane_Intro == null) {
			jJEditorPane_Intro = new JEditorPane();
			jJEditorPane_Intro.setFont(new java.awt.Font("Dialog",0,11));
			jJEditorPane_Intro.setContentType("text/html");
			jJEditorPane_Intro.setText("<html>"+
					"<h4>Welcome to SPOT GUI</h4>" +
					"If this is your first time of use, you should read this carefully, to get an "+
					"overview. With this graphical interface you can easily configure and start "+
					"optimization runs via the software <i>SPOT (sequential parameter optimization "+
					"toolbox)</i>. This GUI does not contain any optimization features itself, but "+
					"it will help you to set up and archive own optimization projects for <i>SPOT</i> "+
					"without creating the required configuration files manually. The tabs of this "+
					"software represent the required steps for setting up and running a project: "+
					"<br><br><b>1. <u>File select</u>:</b> In this tab you have to select files or functions for your "+
					"optimization problem. Three files can, but do not have to be chosen. First you can choose a file "+
					"that contains a call to your target Algorithm in R. "+
					"Note that the algorithm script does not have to be the algorithm, "+
					"which should be optimized, but can rather be an interface which will invoke the algorithm. "+
					"Additionally you can choose an .apd File, which defines parameters of your algorithm, "+
					"that will not be changed by <i>SPOT</i>, i.e. the problem definition."+
					"Finally you can choose one of the default report functions for the evaluation of your <i>SPOT</i> run. "+
					"If you do not choose a file for your algorithm function, you can "+
					"alternatively choose functions that are allready implemented in <i>SPOT</i>. "+
					"This can be done with the drop down menues, regardless of the files chosen. "+
					"<br><b>2. <u>ROI</u>:</b> Specify the name and range of the exogenous parameters which should be fit. "+
					"<br><b>3. <u>Config</u>:</b> Set <i>SPOT</i> related options for the optimization run and project settings. "+
					"<br><b>4. <u>Run</u>:</b> Check the whole configuration of your current project and finally start <i>SPOT</i> with one "+
					"of the supplied tasks. You can choose a full automatic task as well, which will include all other tasks sequentially. "+
					"<br><b>5. <u>Results</u>:</b>"+
					"Here you can view the different results produced by <i>SPOT</i>, including textfiles and pdf figures. "+
					"<br><br>When you first use <i>SPOT</i> you should load existing ROI and CONF files to get used to valid setups. "+
					"To get further information for single configuration options, or a more detailed how-to manual, "+
					"you can use the \"Help\" button."+
			"</html>");
			jJEditorPane_Intro.setCaretPosition(0);
			jJEditorPane_Intro.setPreferredSize(new java.awt.Dimension(458, 392));
			jJEditorPane_Intro.setEditable(false);
		}
		return jJEditorPane_Intro;
	}

	/**
	 * This method initializes jDialog	
	 * 	
	 * @return javax.swing.JDialog	
	 */
	private JDialog getJDialog1() {
		if (jDialog_pathSetup == null) {
			jDialog_pathSetup = new JDialog(getJFrame());
			jDialog_pathSetup.setSize(new java.awt.Dimension(480, 140));
			jDialog_pathSetup.setContentPane(getJContentPane1());
			jDialog_pathSetup.setTitle("Setup: Path to R.exe");
		}
		if(!pathFile_SetupProperties.exists()){
			jButton_savePathSetup.setEnabled(true);
		}
		return jDialog_pathSetup;
	}

	/**
	 * This method initializes jContentPane1	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJContentPane1() {
		if (jContentPane1 == null) {
			jLabel_rPath = new JLabel();
			jLabel_rPath.setText("Filepath to R.exe (windows systems only):");
			jLabel_rPath.setBounds(8, 22, 359, 14);					
			jContentPane1 = new JPanel();
			jContentPane1.setLayout(null);
			jContentPane1.add(getJTextField_rPath(), null);
			jContentPane1.add(getJButton_rPathBrowse(), null);
			jContentPane1.add(jLabel_rPath, null);
			jContentPane1.add(getJButton_pathSetupCancel(), null);
			jContentPane1.add(getJButton_savePathSetup(), null);
			if(!IS_OS_WINDOWS){
				jTextField_rPath.setEnabled(false);
				jButton_rPathBrowse.setEnabled(false);
				jLabel_rPath.setEnabled(false);		
				jButton_savePathSetup.setEnabled(false);
			}
		}
		return jContentPane1;
	}

	/**
	 * This method initializes jTextField_rPath	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField_rPath() {
		if (jTextField_rPath == null) {
			jTextField_rPath = new JTextField();
			jTextField_rPath.setEditable(false);
			jTextField_rPath.setText(pathFile_R.toString());
			jTextField_rPath.setBounds(8, 36, 357, 20);
		}
		return jTextField_rPath;
	}

	/**
	 * This method initializes jButton_rPathBrowse	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_rPathBrowse() {
		if (jButton_rPathBrowse == null) {
			jButton_rPathBrowse = new JButton();
			jButton_rPathBrowse.setBounds(371, 35, 91, 22);
			jButton_rPathBrowse.setText("Browse");
			jButton_rPathBrowse.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					JFileChooser rPathFC = getJFileChooser_rPath();
					int ret = rPathFC.showDialog(jButton_rPathBrowse.getParent(), "Set R filepath");
					if (ret == JFileChooser.APPROVE_OPTION){
						if(rPathFC.getSelectedFile().getName().equalsIgnoreCase("R.exe")){
							pathFile_R = rPathFC.getSelectedFile();
							pathFCDefaultRPATH = pathFile_R;
							jTextField_rPath.setText(pathFile_R.toString());
							jButton_savePathSetup.setEnabled(true);
						}else{
							JOptionPane.showMessageDialog(jButton_savePathSetup.getParent(), "R filepath is invalid. Please use the browse button to select valid path for R (R.exe)" , "Path setup", JOptionPane.ERROR_MESSAGE);
							jButton_savePathSetup.setEnabled(false);
						}
					}
				}
			});
		}
		return jButton_rPathBrowse;
	}

	/**
	 * This method initializes jButton_pathSetupCancel	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_pathSetupCancel() {
		if (jButton_pathSetupCancel == null) {
			jButton_pathSetupCancel = new JButton();
			jButton_pathSetupCancel.setText("Cancel");
			jButton_pathSetupCancel.setBounds(350, 82, 112, 23);
			jButton_pathSetupCancel.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {							
					if (setupPropertieExist){ // THIS CASE => SKIP ChANGES
						String old_pathR = setupProperties.getProperty("pathFile_R");
						if(!old_pathR.equals(pathFile_R.toString())){
							pathFile_R = new File(old_pathR);
							pathFCDefaultRPATH = pathFile_R;
							jTextField_rPath.setText(pathFile_R.toString());
						}
						loadPathSetup(pathFile_SetupProperties);
						jTextField_rPath.setText(pathFile_R.toString());
					}else{
						jTextField_rPath.setText("");
					}
					jButton_savePathSetup.setEnabled(false);
					jDialog_pathSetup.dispose();
				}
			});
		}
		return jButton_pathSetupCancel;
	}

	/**
	 * This method initializes jButton_savePathSetup	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getJButton_savePathSetup() {
		if (jButton_savePathSetup == null) {
			jButton_savePathSetup = new JButton();
			jButton_savePathSetup.setText("Save path setup");
			jButton_savePathSetup.setBounds(185, 82, 155, 23);
			jButton_savePathSetup.setEnabled(false);
			jButton_savePathSetup.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					setupProperties.setProperty("pathFile_R", pathFile_R.toString());
					try {
						setupProperties.storeToXML(new FileOutputStream(pathFile_SetupProperties), null);
					} catch (FileNotFoundException exc) {
						exc.printStackTrace();
					} catch (IOException exc) {
						exc.printStackTrace();
					}
					loadPathSetup(pathFile_SetupProperties); // updating default paths
					jButton_savePathSetup.setEnabled(false);
					jDialog_pathSetup.dispose();
				}
			});
		}
		return jButton_savePathSetup;
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
			jJMenuBar.add(getToolsMenu());
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
			fileMenu.add(getImportMenuItem());
			fileMenu.add(getImport2MenuItem());
			fileMenu.add(getImport3MenuItem());
			fileMenu.add(getExportMenuItem());
			fileMenu.add(getSetRPathMenuItem());
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
			helpMenu.add(getHelpPdfMenuItem());
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
					int answer = JOptionPane.showConfirmDialog(exitMenuItem.getParent(), "All current settings will be lost. Are you sure?","Exit", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
					if (answer == JOptionPane.YES_OPTION)
						System.exit(0);
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
			jLabel_logo = new JLabel();
			jLabel_logo.setBounds(17, 139, 455, 254);
			jLabel_logo.setIcon(iconLogo);
			jLabel_logo.setHorizontalAlignment(SwingConstants.CENTER);
			aboutContentPane.add(jLabel_logo, BorderLayout.NORTH);
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
			aboutVersionLabel.setText("<html><center><h3>SPOT GUI ("+SPOTGUIVERSION+")</h3>A graphical user interface for the sequential parameter optimization toolbox <i>SPOT</i><br>This software has been developed in the context of a diploma thesis.<br><br><i>Authors: Tobias Zimmer, Martin Zaefferer</i></center></html>");
			aboutVersionLabel.setHorizontalAlignment(SwingConstants.CENTER);
		}
		return aboutVersionLabel;
	}

	/**
	 * This method initializes helpPdfMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getHelpPdfMenuItem() {
		if (helpPdfMenuItem == null) {
			helpPdfMenuItem = new JMenuItem();
			helpPdfMenuItem.setText("PDF Manual");
			helpPdfMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					try {//JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "Help PDF file will be opened in your default viewer.", "starting help", JOptionPane.INFORMATION_MESSAGE);
						Desktop.getDesktop().browse(pathFile_HelpPdf.toURI());
					} catch (IOException e1) {e1.printStackTrace();}	
				}
			});
		}
		return helpPdfMenuItem;
	}

	/**
	 * This method initializes importMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getImportMenuItem() {
		if (importMenuItem == null) {
			importMenuItem = new JMenuItem();
			importMenuItem.setText("Import Configuration (.conf)");
			importMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {					
					int answer = JOptionPane.showConfirmDialog(importMenuItem.getParent(), "Your current .conf Settings will be overwritten. Are you sure you want to go on?","Import settings from file", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
					if (answer == JOptionPane.YES_OPTION){
						JFileChooser importSettingsFC = getJFileChooser_importSettings();
						int ret = importSettingsFC.showDialog(importMenuItem.getParent(), "Import");
						if (ret == JFileChooser.APPROVE_OPTION){
							File chosenFile = new File("");
							chosenFile= importSettingsFC.getSelectedFile();
							pathFCDefaultImportSettigns= chosenFile.getParentFile();;
							if(chosenFile.toString().toLowerCase().endsWith(".conf")){
								pathFile_ProjectConf = chosenFile;
								if (makeStringListToConfVal(fileIO.readToStrList(pathFile_ProjectConf))!=0){
									JOptionPane.showMessageDialog(importMenuItem.getParent(), "Error: CONF load failed.", "Load failed", JOptionPane.ERROR_MESSAGE);
								}else{
									loadConfValues(pathFile_ProjectConf);
									jCheckBox_CONFprojectPathSubfolder.setSelected(false);
								}
							}	
						}
					}
				}
			});
		}
		return importMenuItem;
	}
	
	
	public boolean accept(File f) {
		if (f.isDirectory()) return true;
		String tmp = f.getName().toLowerCase();
		return tmp.endsWith(".setting") || tmp.endsWith(".roi") || tmp.endsWith(".conf");
	}
	public String getDescription () { return "Importable files (*.setting, *.roi, *.conf)"; } 

	/**
	 * This method initializes exportMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getExportMenuItem() {
		if (exportMenuItem == null) {
			exportMenuItem = new JMenuItem();
			exportMenuItem.setText("Export (.setting)");
			exportMenuItem.setEnabled(false);
			exportMenuItem.setVisible(false);
			exportMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
						JFileChooser exportSettingsFC = getJFileChooser_exportSettings();
						int ret = exportSettingsFC.showDialog(exportMenuItem.getParent(), "Export");
						if (ret == JFileChooser.APPROVE_OPTION){
							pathFCDefaultExportSettigns = exportSettingsFC.getSelectedFile();
							if(!pathFCDefaultExportSettigns.toString().toLowerCase().endsWith(".setting")){
								pathFCDefaultExportSettigns = new File(pathFCDefaultExportSettigns.toString()+".setting");
							}
							if(exportSetting(pathFCDefaultExportSettigns)!=0){
								JOptionPane.showMessageDialog(exportMenuItem.getParent(), "Error: Export failed.", "Export failed", JOptionPane.ERROR_MESSAGE);
								pathFCDefaultExportSettigns = pathFile_ExportSettigns;
							}else{
								pathFile_ExportSettigns = pathFCDefaultExportSettigns;
							}
						}
					}
			});
		}
		return exportMenuItem;
	}

	/**
	 * This method initializes toolsMenu	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getToolsMenu() {
		if (toolsMenu == null) {
			toolsMenu = new JMenu();
			toolsMenu.setText("Tools");
			toolsMenu.add(getFuncGeneratorMenuItem());
		}
		return toolsMenu;
	}

	/**
	 * This method initializes funcGeneratorMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getFuncGeneratorMenuItem() {
		if (funcGeneratorMenuItem == null) {
			funcGeneratorMenuItem = new JMenuItem();
			funcGeneratorMenuItem.setText("Target Function Generator");
			funcGeneratorMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					//DUMMY CODE FOR FUTURE DEVELOPMENT
					String[] callArgs = {
							"cmd", "/C","start", "\"\"","/wait"//File that is called with Rcmd.exe or Rscript.exe
					};	
					tarFuncGenerator.main(callArgs);
				}
			});
		}
		return funcGeneratorMenuItem;
	}

	/**
	 * This method initializes setRPathMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getSetRPathMenuItem() {
		if (setRPathMenuItem == null) {
			setRPathMenuItem = new JMenuItem();
			setRPathMenuItem.setText("Path Setup");
			setRPathMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					JDialog pathSetupJD = getJDialog1();
					pathSetupJD.setLocationRelativeTo(setRPathMenuItem.getParent());
					pathSetupJD.setVisible(true);				
				}
			});
		}
		return setRPathMenuItem;
	}

	/**
	 * This method initializes selectAlgorithmPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getSelectAlgorithmPanel() {
		if (selectAlgorithmPanel == null) {
			tarDescriptionLabel = new JLabel();
			tarDescriptionLabel.setBounds(new Rectangle(13, 17, 462, 92));
			tarDescriptionLabel.setText("<html>Please select a target function. You can either choose an internal function from the dropdown list, or first select a file where your custom function is stored. This has to be an .R script. Then select the name of the function from the dropdown list. However it has to be prepared for use with SPOT. To create your own functions easily use the function generator in the \"Tools\" Menu.</html>");
			selectAlgorithmPanel = new JPanel();
			selectAlgorithmPanel.setBorder(BorderFactory.createTitledBorder("Target Function"));
			selectAlgorithmPanel.setLayout(null);
			selectAlgorithmPanel.setBounds(new Rectangle(5, 5, 490, 175));
			selectAlgorithmPanel.add(getJComboBox_CONFalgname(), null);
			selectAlgorithmPanel.add(jLabel_CONFalgname, null);
			selectAlgorithmPanel.add(getJTextField_selectedAlgorithmScript(), null);
			selectAlgorithmPanel.add(getJButton_algorithmPathBrowse(), null);
			selectAlgorithmPanel.add(tarDescriptionLabel, null);
		}
		return selectAlgorithmPanel;
	}

	/**
	 * This method initializes selectApdPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getSelectApdPanel() {
		if (selectApdPanel == null) {
			apdDescriptionLabel = new JLabel();
			apdDescriptionLabel.setBounds(new Rectangle(11, 13, 471, 59));
			apdDescriptionLabel.setText("<html>Choose an APD File here. Not mandatory, unless your custom target function requests it.</html>");
			selectApdPanel = new JPanel();
			selectApdPanel.setBorder(BorderFactory.createTitledBorder("Algorithm Problem Definition"));
			selectApdPanel.setLayout(null);
			selectApdPanel.setBounds(new Rectangle(5, 180, 490, 110));
			selectApdPanel.add(getJTextField_CONFalgPdFilePath(), null);
			selectApdPanel.add(getJButton_CONFalgPdFilePathBrowse(), null);
			selectApdPanel.add(apdDescriptionLabel, null);
		}
		return selectApdPanel;
	}

	/**
	 * This method initializes selectReportPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getSelectReportPanel() {
		if (selectReportPanel == null) {
			repDescriptionLabel = new JLabel();
			repDescriptionLabel.setBounds(new Rectangle(15, 22, 465, 28));
			repDescriptionLabel.setText("<html>You can choose one of SPOTs default report functions here, simply use the dropdown menu. Note that the 3d Report will not work properly from within the spotGUI.</html>");
			selectReportPanel = new JPanel();
			selectReportPanel.setBorder(BorderFactory.createTitledBorder("Report Function"));
			selectReportPanel.setLayout(null);
			selectReportPanel.setBounds(new Rectangle(5, 295, 490, 120));
			//selectReportPanel.add(getJTextField_selectedReportScript(), null);
			selectReportPanel.add(jLabel_reportFunction, null);
			selectReportPanel.add(getJComboBox_reportFunction(), null);
			//selectReportPanel.add(getJButton_reportPathBrowse(), null);
			selectReportPanel.add(repDescriptionLabel, null);
		}
		return selectReportPanel;
	}

	/**
	 * This method initializes autoConfigPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getAutoConfigPanel() {
		if (autoConfigPanel == null) {
			autoConfigPanel = new JPanel();
			autoConfigPanel.setLayout(null);
			autoConfigPanel.setBounds(7, 175, 486, 51);
			autoConfigPanel.setBorder(BorderFactory.createTitledBorder("Auto config"));
			autoConfigPanel.add(jLabel_CONFautoLoopSteps, null);
			autoConfigPanel.add(getJTextField_CONFautoLoopSteps(), null);
			autoConfigPanel.add(getJTextField_CONFautoLoopNEvals(), null);
			autoConfigPanel.add(jLabel_CONFautoLoopNEvals, null);
		}
		return autoConfigPanel;
	}

	/**
	 * This method initializes import2MenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getImport2MenuItem() {
		if (import2MenuItem == null) {
			import2MenuItem = new JMenuItem();
			import2MenuItem.setText("Import Region of Interest (.roi)");
			import2MenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {					
					int answer = JOptionPane.showConfirmDialog(import2MenuItem.getParent(), "Your current Region Of Interest will be overwritten. Are you sure you want to go on?","Import settings from file", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
					if (answer == JOptionPane.YES_OPTION){
						JFileChooser import2SettingsFC = getJFileChooser_import2Settings();
						int ret = import2SettingsFC.showDialog(import2MenuItem.getParent(), "Import");
						if (ret == JFileChooser.APPROVE_OPTION){
							File chosenFile = new File("");
							chosenFile= import2SettingsFC.getSelectedFile();
							pathFCDefaultImportSettigns= chosenFile.getParentFile();;
							if(chosenFile.toString().toLowerCase().endsWith(".roi"))							{								
								pathFile_LoadROI = chosenFile;
								if (makeStringListToRoiVal(fileIO.readToStrList(pathFile_LoadROI))!=0){
									JOptionPane.showMessageDialog(import2MenuItem.getParent(), "Error: ROI load failed.", "Load failed", JOptionPane.ERROR_MESSAGE);
								}
							}

						}
					}
				}
			});
		}
		return import2MenuItem;
	}

	/**
	 * This method initializes import3MenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getImport3MenuItem() {
		if (import3MenuItem == null) {
			import3MenuItem = new JMenuItem();
			import3MenuItem.setText("Import Algorithm Problem Design (.apd)");
			import3MenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {					
					//int answer = JOptionPane.showConfirmDialog(import3MenuItem.getParent(), "All current settings will be lost. Are you sure you want to go on?","Import settings from file", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
					//if (answer == JOptionPane.YES_OPTION){
						JFileChooser import3SettingsFC = getJFileChooser_import3Settings();
						int ret = import3SettingsFC.showDialog(import3MenuItem.getParent(), "Import");
						if (ret == JFileChooser.APPROVE_OPTION){
							File chosenFile = new File("");
							chosenFile= import3SettingsFC.getSelectedFile();
							pathFCDefaultImportSettigns= chosenFile.getParentFile();;
							pathFile_APD = import3SettingsFC.getSelectedFile();
							pathFCDefaultAPD = pathFile_APD;
							jTextField_CONFalgPdFilePath.setText(pathFile_APD.toString());	
						}
					//}
				}
			});
		}
		return import3MenuItem;
	}
	/**
	 * Launches this application
	 */
	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				spotGuiMain application = new spotGuiMain();
				application.getJFrame().setVisible(true);
			}
		});
	}

	// --------------------FILECHOOSER--------------------
	/**
	 * This method initializes jFileChooser_importSettings
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getJFileChooser_importSettings() {
		if(jFileChooser_importSettings == null) {
			jFileChooser_importSettings = new JFileChooser();
			jFileChooser_importSettings.removeChoosableFileFilter(jFileChooser_importSettings.getChoosableFileFilters()[0]);
			jFileChooser_importSettings.addChoosableFileFilter(fileFilterAll);
			jFileChooser_importSettings.addChoosableFileFilter(fileFilterCONF);
		}
		jFileChooser_importSettings.setCurrentDirectory(pathFCDefaultImportSettigns);
		return jFileChooser_importSettings;
	}
	/**
	 * This method initializes jFileChooser_import2Settings
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getJFileChooser_import2Settings() {
		if(jFileChooser_import2Settings == null) {
			jFileChooser_import2Settings = new JFileChooser();
			jFileChooser_import2Settings.removeChoosableFileFilter(jFileChooser_import2Settings.getChoosableFileFilters()[0]);
			jFileChooser_import2Settings.addChoosableFileFilter(fileFilterAll);
			jFileChooser_import2Settings.addChoosableFileFilter(fileFilterROI);
		}
		jFileChooser_import2Settings.setCurrentDirectory(pathFCDefaultImportSettigns);
		return jFileChooser_import2Settings;
	}
	/**
	 * This method initializes jFileChooser_import3Settings
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getJFileChooser_import3Settings() {
		if(jFileChooser_import3Settings == null) {
			jFileChooser_import3Settings = new JFileChooser();
			jFileChooser_import3Settings.removeChoosableFileFilter(jFileChooser_import3Settings.getChoosableFileFilters()[0]);
			jFileChooser_import3Settings.addChoosableFileFilter(fileFilterAll);
			jFileChooser_import3Settings.addChoosableFileFilter(fileFilterAPD);
		}
		jFileChooser_import3Settings.setCurrentDirectory(pathFCDefaultImportSettigns);
		return jFileChooser_import3Settings;
	}
	/**
	 * This method initializes jFileChooser_exportSettings
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getJFileChooser_exportSettings() {
		if(jFileChooser_exportSettings == null) {
			jFileChooser_exportSettings = new JFileChooser();
			jFileChooser_exportSettings.removeChoosableFileFilter(jFileChooser_exportSettings.getChoosableFileFilters()[0]);
			jFileChooser_exportSettings.addChoosableFileFilter(fileFilterAll);
			//jFileChooser_exportSettings.addChoosableFileFilter(fileFilterSetting);
		}
		jFileChooser_exportSettings.setCurrentDirectory(pathFCDefaultExportSettigns);
		return jFileChooser_exportSettings;
	}
	/**
	 * This method initializes jFileChooser_CONFprojectPath
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getJFileChooser_CONFprojectPath() {
		if(jFileChooser_CONFprojectPath == null) {
			jFileChooser_CONFprojectPath = new JFileChooser();
			jFileChooser_CONFprojectPath.removeChoosableFileFilter(jFileChooser_CONFprojectPath.getChoosableFileFilters()[0]);
			jFileChooser_CONFprojectPath.addChoosableFileFilter(fileFilterAll);
			jFileChooser_CONFprojectPath.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		}
		jFileChooser_CONFprojectPath.setCurrentDirectory(pathFCDefaultCONFproject); // ***TOD0*** : ProjectPath aktualisieren
		return jFileChooser_CONFprojectPath;
	}
	/**
	 * This method initializes jFileChooser_algorithmSelect
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getJFileChooser_algorithmSelect() {
		if(jFileChooser_algorithmSelect == null) {
			jFileChooser_algorithmSelect = new JFileChooser();
			jFileChooser_algorithmSelect.removeChoosableFileFilter(jFileChooser_algorithmSelect.getChoosableFileFilters()[0]);
			jFileChooser_algorithmSelect.addChoosableFileFilter(fileFilterAll);
			jFileChooser_algorithmSelect.addChoosableFileFilter(fileFilterAlgo);
		}
		jFileChooser_algorithmSelect.setCurrentDirectory(pathFCDefaultCONFalgorithmScript); // ***TOD0*** : Algorithmpath aktualisieren
		return jFileChooser_algorithmSelect;
	}
	/**
	 * This method initializes jFileChooser_reportSelec
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	/*
	private JFileChooser getJFileChooser_analyzeSelect() {
		if(jFileChooser_reportSelect == null) {
			jFileChooser_reportSelect = new JFileChooser();
			jFileChooser_reportSelect.removeChoosableFileFilter(jFileChooser_reportSelect.getChoosableFileFilters()[0]);
			jFileChooser_reportSelect.addChoosableFileFilter(fileFilterAll);
			jFileChooser_reportSelect.addChoosableFileFilter(fileFilterR);
		}
		jFileChooser_reportSelect.setCurrentDirectory(pathFCDefaultReportScript); // ***TOD0*** : Analyzepath
		return jFileChooser_reportSelect;
	}	*/
	/**
	 * This method initializes jFileChooser_rPath
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getJFileChooser_rPath() {
		if(jFileChooser_rPath == null) {
			jFileChooser_rPath = new JFileChooser();
			jFileChooser_rPath.removeChoosableFileFilter(jFileChooser_rPath.getChoosableFileFilters()[0]);
			jFileChooser_rPath.addChoosableFileFilter(fileFilterAll);
			jFileChooser_rPath.addChoosableFileFilter(fileFilterExe);
		}
		jFileChooser_rPath.setCurrentDirectory(pathFCDefaultRPATH);
		return jFileChooser_rPath;
	}
	/**
	 * This method initializes jFileChooser_APD
	 * 	
	 * @return javax.swing.JFileChooser	
	 */
	private JFileChooser getJFileChooser_APD() {
		if(jFileChooser_APD == null) {
			jFileChooser_APD = new JFileChooser();
			jFileChooser_APD.removeChoosableFileFilter(jFileChooser_APD.getChoosableFileFilters()[0]);
			jFileChooser_APD.addChoosableFileFilter(fileFilterAll);
			jFileChooser_APD.addChoosableFileFilter(fileFilterAPD);
		}
		jFileChooser_APD.setCurrentDirectory(pathFCDefaultAPD);
		return jFileChooser_APD;
	}
	//##################################################################################################
	// TABLE MODEL + ROI bezogene Funktionen 
	/** 
	 * TABLE MODEL CLASS
	 */
	class myTableModel extends AbstractTableModel{
		private static final long serialVersionUID = 1L;

		public int getColumnCount(){
			return roiTableColumnNames.length;
		}
		public int getRowCount(){
			return roiTableDim;
		}
		public Object getValueAt(int r, int c){
			return roiTableData[r][c];
		}
		public String getColumnName(int i){
			return(roiTableColumnNames[i]);
		}
		@SuppressWarnings("unchecked")
		public Class getColumnClass(int i){
			return getValueAt(0, i).getClass();
		}
		public boolean isCellEditable(int r, int c){
			return true;
		}
		public void setValueAt(Object val, int r, int c) {
			boolean isValidType = false;// val isValidType if: String is equals to one of the valid Types (which are defined in ==> String[] roiTableTypes)
			if (val.toString().equalsIgnoreCase(roiTableTypes[0]) || val.toString().equalsIgnoreCase(roiTableTypes[1]) || val.toString().equalsIgnoreCase(roiTableTypes[2]))
				isValidType = true;				
			if(c==0){
				if (isRegExString(val.toString()))
					roiTableData[r][c] = new String(val.toString());
				else
					JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "Wrong name format. Allowed characters are: A-Z, a-z, _ and 0-9. Note, that the name have to start with a letter." + "   (Invalid value was: \""+val.toString()+"\" in column "+roiTableColumnNames[c]+" @ parameter "+jTable_ROI.getValueAt(r, 0)+")", "Value type error", JOptionPane.ERROR_MESSAGE);
			}else if(c==1 || c==2){
				if (isRegExInt(val.toString())!=null){
					roiTableData[r][c] = new String(isRegExInt(val.toString()));
				}else if(isRegExFloat(val.toString())!=null){
					String temp = isRegExFloat(val.toString());
					if(temp.endsWith(".0"))
						temp = temp.replace(".0", "");
					roiTableData[r][c] = new String(temp);
				}else if (hlp.isRegExPosInf(val.toString())){
					roiTableData[r][c] = new String("Inf");
				}else if (hlp.isRegExNegInf(val.toString())){
					roiTableData[r][c] = new String("-Inf");
				}else 
					JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "Type of value have to be INT or FLOAT. Other allowed values are: Inf and -Inf." + "   (Invalid value was: \""+val.toString()+"\" in column "+roiTableColumnNames[c]+" @ parameter "+jTable_ROI.getValueAt(r, 0)+")", "Value type error", JOptionPane.ERROR_MESSAGE);
			}else if(c==3){
				if (isValidType){
					roiTableData[r][c] = new String(val.toString());
				}else
					JOptionPane.showMessageDialog(jTextField_ROIdim.getParent(), "Type have to be INT, FLOAT or FACTOR" + "   (Invalid value was: \""+val.toString()+"\" in column "+roiTableColumnNames[c]+" @ parameter "+jTable_ROI.getValueAt(r, 0)+")", "Value type error", JOptionPane.ERROR_MESSAGE);
			}
			fireTableCellUpdated(r, c);
		}
	}
	/**
	 * INIT TABLE DATA
	 */
	public void initTableData(String def_min, String def_max, String def_type){
		for (int i=0; i<roiTableMaxDim; i++){
			roiTableData[i][0] = new String("VARX"+Integer.toString(i+1));
			roiTableData[i][1] = new String(def_min);
			roiTableData[i][2] = new String(def_max);
			roiTableData[i][3] = new String(def_type);
		}
	}
	/**
	 * returns true if: String is not empty, starting with letter, including no extravagant Symbols ("-" and "_" is VALID)	
	 */
	private boolean isRegExString(String val){// val isValidString if: String is not empty, starting with letter, including no extravagant Symbols ("-" and "_" is VALID)	
		String regEx = "[a-zA-Z][\\w]*"; // [\\w]=[a-zA-Z_0-9]
		return val.toString().matches(regEx);
	}
	/**
	 * returns fixed int value String 
	 */
	public String isRegExInt(String val){// val isInt if: valid parse on datatype INT is possible
		// return is the fixed int value String
		try {
			return Integer.toString(Integer.valueOf(val.toString()).intValue());
		} catch(NumberFormatException nfe) {
			return null;
		}
	}
	/**
	 * returns fixed float value String 
	 */
	public String isRegExFloat(String val){	// val isFloat if: valid parse on datatype FLOAT is possible
		// return is the fixed float value String
		try {
			return Float.toString(Float.valueOf(val.toString().replace(",", ".")).floatValue());
		} catch(NumberFormatException nfe) {
			return null;
		}
	}

	//##################################################################################################
	// CHANGES/REFRESH/UPDATE bezogene Funktionen
	/*
	 * Clear all CONF vars
	 */
	public void clearConfVars(){
		conf_algPath = "";
		conf_algName = "";
		conf_algPdFile = "";
		conf_model = "";
		conf_design_type = "";
		conf_init_design_size = "";
		conf_seq_design_size = "";
		conf_auto_loop_steps = "";
	}			
	/**
	 * CHANGES OCCURED (spot configuration)
	 */
	public void refreshConf(){
		boolean DEBUG = true; // DEBUG
		boolean DEBUGadvanced = true;
		boolean changesOccured = false;
		String check = "";
		//***BASE CONF***
		//conf_algPath
		//conf_algName		
		//if(conf_algLanguage.equals("sourceR")){
			// ALGPATH RELATIVE FILEPATH GENERIEREN
			// NOTE: at this point path_Project has not been checked yet ... this will be done in checkSPO later
			if(!jTextField_selectedAlgorithmScript.equals("")){
				try {
					relativePathFileString_algPath = hlp.getRelPath(pathFile_AlgorithmScript,path_Project);
				} catch (IOException e) {
					System.err.println("IOException @ relativePathFileString_algPath=hlp.getRelPath(pathFile_AlgorithmScript,path_Project);");
				}
				File relativeAlgPath = new File(relativePathFileString_algPath);
				if(relativePathFileString_algPath.contains(FILE_SEPERATOR)) //Check is needed in case algorithm is in project directory, which leads to an error if getting the "parent" directory -> is empty
					check = relativeAlgPath.getParent().toString();
				else
					check = "";
				if(!conf_algPath.equals(check)){
					if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf_algPath:  old=" + hlp.strQuote(conf_algPath) + "  new=" + hlp.strQuote(check));
					conf_algPath = check;
					changesOccured = true;
				}
			}
			else{
				check="";
				if(!conf_algPath.equals(check)){
					if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: alg.path:  old=" + hlp.strQuote(conf_algPath) + "  new=" + hlp.strQuote(check));
					conf_algPath = check;
					changesOccured = true;
				}
			}		
			if (jComboBox_CONFalgname.getSelectedIndex()>=0){				
				check = jComboBox_CONFalgname.getSelectedItem().toString().replace(" = ", " <- ").split(" <- ")[0];
				if(!conf_algName.equals(check)){
					if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: alg.func:  old=" + hlp.strQuote(conf_algName) + "  new=" + hlp.strQuote(check));
					conf_algName = check;
					changesOccured = true;
				}
			}
		//}else{gibts nichtmehr...}			
		// conf_algPdFile RELATIVE FILEPATH GENERIEREN
		// NOTE: at this point conf_algPdFile has not been checked yet ... this will be done in checkSPO later
		try {
			relativePathFileString_algPd = hlp.getRelPath(pathFile_APD,path_Project);
		} catch (IOException e) {
			System.err.println("IOException @ relativePathFileString_algPd=hlp.getRelPath(pathFile_APD,path_Project);");
		}
		check = relativePathFileString_algPd;
		if(!conf_algPdFile.equals(check)){
			if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: io.apdFileName:  old=" + hlp.strQuote(conf_algPdFile) + "  new=" + hlp.strQuote(check));
			conf_algPdFile = check;
			changesOccured = true;
		}
		//***OPTIONAL CONF***
		//conf_model
		try{
			check = jComboBox_CONFmodel.getSelectedItem().toString();
			if(!conf_model.equals(check)){
				if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: seq.predictionModel.func:  old=" + hlp.strQuote(conf_model) + "  new=" + hlp.strQuote(check));
				conf_model = check;
				changesOccured = true;
			}
		}catch(Exception e){
			System.err.println("catched exception: jComboBox_CONFmodel.getSelectedItem()");
		}
		//conf_design_func
		try{
			check = jComboBox_CONFdesignType.getSelectedItem().toString();
			if(!conf_design_type.equals(check)){
				if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: init.design.func:  old=" + hlp.strQuote(conf_design_type) + "  new=" + hlp.strQuote(check));
				conf_design_type = check;
				changesOccured = true;
			}
		}catch(Exception e){
			System.err.println("catched exception: jComboBox_CONFmodel.getSelectedItem()");
		}
		//conf_init_design_size
		check = jTextField_CONFinitLhdPoints.getText();
		if(!conf_init_design_size.equals(check)){
			if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: init.design.size:  old=" + hlp.strQuote(conf_init_design_size) + "  new=" + hlp.strQuote(check));
			conf_init_design_size = check;
			changesOccured = true;
		}
		//conf_seq_design_size
		check = jTextField_CONFseqDesignSize.getText();
		if(!conf_seq_design_size.equals(check)){
			if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: seq.design.size:  old=" + hlp.strQuote(conf_seq_design_size) + "  new=" + hlp.strQuote(check));
			conf_seq_design_size = check;
			changesOccured = true;
		}
		//conf_auto_loop_steps
		check = jTextField_CONFautoLoopSteps.getText();
		if(!conf_auto_loop_steps.equals(check)){
			if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: auto.loop.steps:  old=" + hlp.strQuote(conf_auto_loop_steps) + "  new=" + hlp.strQuote(check));
			conf_auto_loop_steps = check;
			changesOccured = true;
		}
		//conf_auto_loop_nevals
		check = jTextField_CONFautoLoopNEvals.getText();
		if(!conf_auto_loop_nevals.equals(check)){
			if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: auto.loop.nevals:  old=" + hlp.strQuote(conf_auto_loop_nevals) + "  new=" + hlp.strQuote(check));
			conf_auto_loop_nevals = check;
			changesOccured = true;
		}		
		if (DEBUG && DEBUGadvanced && jCheckBox_CONFadvanced.isSelected()) {
			jColorPane_spotRun.append(Color.gray, "\nconf_advanced:");
			String[] advanced = jTextArea_CONFadditional.getText().split("\n");
			for(int i=0;i<advanced.length; i++){
				jColorPane_spotRun.append(Color.gray, "\n...");
				jColorPane_spotRun.append(Color.gray, advanced[i]);
			}
		}				
		/*if(!pathFile_ReportScript.toString().equals("") && !path_Project.toString().equals("")){
			try {
				relativePathFileString_reportPath = hlp.getRelPath(pathFile_ReportScript,path_Project);
			} catch (IOException e) {
				System.err.println("IOException @ relativePathFileString_reportPath=hlp.getRelPath(pathFile_ReportScript,path_Project);");
			}
		}*/
		//File relativeRepPath = new File(relativePathFileString_reportPath);
		//if(relativePathFileString_reportPath.contains(FILE_SEPERATOR))
		//	check = relativeRepPath.getParent().toString();
		//else 
		//	check = "";
		//if(!reportPath.equals(check)){
		//	if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: report.path:  old=" + hlp.strQuote(reportPath) + "  new=" + hlp.strQuote(check));
		//	reportPath = check;
		//	changesOccured = true;
		//}		
		if(jComboBox_reportFunction.getSelectedIndex()>=0){
			check = jComboBox_reportFunction.getSelectedItem().toString().replace(" = ", " <- ").split(" <- ")[0];
			if(!reportFunction.equals(check)){
				if(DEBUG) jColorPane_spotRun.append(Color.gray, "\nconf: report.func:  old=" + hlp.strQuote(reportFunction) + "  new=" + hlp.strQuote(check));
				reportFunction = check;
				changesOccured = true;
			}
		}	
		if(changesOccured){
			jLabel_checkSpotRun_algorithm.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_roiConfig.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_spoConfig.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_rPath.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_roiConfigFile.setIcon(iconCheckUnknown);
			jLabel_checkSpotRun_spoConfigFile.setIcon(iconCheckUnknown);
			jLabel_taskSelect.setEnabled(false);
			jComboBox_taskSelect.setEnabled(false);
			jButton_spotRun.setEnabled(false);
			jButton_checkSpotRun.setEnabled(true);
		}
	}
	/**
	 * Values of ROI Tab will be refreshed/overwritten and new dimension of table could be set.
	 */
	private int updateTableDim(int dim){
		if(dim>=roiTableMinDim && dim<=roiTableMaxDim){
			roiTableDim = dim;
			jSlider_ROIdim.setValue(dim);
			jTextField_ROIdim.setText(Integer.toString(dim));
			jTable_ROI.tableChanged(new TableModelEvent(jTable_ROI.getModel()));
			return 0;
		}else{
			jTable_ROI.tableChanged(new TableModelEvent(jTable_ROI.getModel()));
			return -1;
		}
	}
	/**
	 * Load spezial Conf Values after Loading existing CONF File
	 */
	private void loadConfValues(File confFile){
		// Sonderbehandlung der MUST-SET parameter
		// ***1/6*** report
		if(reportFunction.equals("spotReportDefault")||reportFunction.equals("spotReportSens")||reportFunction.equals("spotReportContour")||reportFunction.equals("spotReport3d"))
		{
			//jTextField_selectedReportScript.setText("");
			pathFile_ReportScript = new File("");
			ComboBoxModel temp1Model = 
				new DefaultComboBoxModel(
						new String[] { "spotReportDefault", "spotReportSens","spotReport3d", "spotReportContour"});					
			jComboBox_reportFunction.setModel(temp1Model);
			jComboBox_reportFunction.setSelectedItem(reportFunction);
		}
		//else if(!reportPath.equals("NA") && !reportPath.equals("")){
		//	try {jTextField_selectedReportScript.setText(new File(confFile.getParent()+FILE_SEPERATOR+reportPath+FILE_SEPERATOR+reportFunction+".R").getCanonicalPath());
		//	} catch (IOException e1) {e1.printStackTrace();}
		//	pathFile_ReportScript = new File(jTextField_selectedReportScript.getText());
		//	if(pathFile_AlgorithmScript.toString().contains(FILE_SEPERATOR)) //prevent "empty" parent folder creating an error
		//		pathFCDefaultReportScript = pathFile_ReportScript.getParentFile();
		//}
		// ***2/6*** algorithm Path
		if(conf_algName.equals("spotFuncStartBranin")||conf_algName.equals("spotFuncStartSixHump")||conf_algName.equals("spotFuncStartSphere")||conf_algName.equals("spotAlgStartSann")||conf_algName.equals("spotAlgStartEs"))
		{
			jTextField_selectedAlgorithmScript.setText("");
			pathFile_AlgorithmScript = new File("");
			ComboBoxModel temp2Model = 
				new DefaultComboBoxModel(
						new String[] { "spotFuncStartBranin","spotFuncStartSixHump","spotFuncStartSphere","spotAlgStartSann","spotAlgStartEs"});							
			jComboBox_CONFalgname.setModel(temp2Model);	
			jComboBox_CONFalgname.setSelectedItem(conf_algName);
		}
		else if(!conf_algPath.equals("NA") && !conf_algPath.equals("")){
			try {jTextField_selectedAlgorithmScript.setText(new File(confFile.getParent()+FILE_SEPERATOR+conf_algPath+FILE_SEPERATOR+conf_algName+".R").getCanonicalPath()); 
			} catch (IOException e1) {e1.printStackTrace();}
			pathFile_AlgorithmScript = new File(jTextField_selectedAlgorithmScript.getText());
			if(pathFCDefaultReportScript.equals("")&&pathFile_AlgorithmScript.toString().contains(FILE_SEPERATOR))
				pathFCDefaultReportScript = pathFile_AlgorithmScript.getParentFile();
			//jTextField_CONFalgorithmScriptFile.setText(new File(jTextField_selectedAlgorithmScript.getText()).getName());
		}	
		// ***3/6***
		// algorithm name
		String[] tmp = null;
		if(pathFile_AlgorithmScript.exists() && pathFile_AlgorithmScript.isFile()){
			tmp = fileIO.readFuncFromRscript(pathFile_AlgorithmScript);
		}
		if (tmp!=null){			
			String[] tmp1= new String[tmp.length+5]; // Add standard entries
			for(int i=0;i<tmp.length;i++){tmp1[i]=tmp[i];}
			tmp1[tmp1.length-5]="spotFuncStartBranin";
			tmp1[tmp1.length-4]="spotFuncStartSixHump";
			tmp1[tmp1.length-3]="spotFuncStartSphere";
			tmp1[tmp1.length-2]="spotAlgStartSann";
			tmp1[tmp1.length-1]="spotAlgStartEs";
			ComboBoxModel jComboBox_CONFalgnameModel = new DefaultComboBoxModel(tmp1);
			jComboBox_CONFalgname.setModel(jComboBox_CONFalgnameModel);
			jComboBox_CONFalgname.setSelectedItem(null);
			for(int i=0;i<jComboBox_CONFalgname.getItemCount();i++){
				if(jComboBox_CONFalgname.getItemAt(i).toString().split(" <- ")[0].equals(conf_algName)){
					jComboBox_CONFalgname.setSelectedIndex(i);
					break;
				}
			}
	
		}
		// ***4/6***
		// report function name
		tmp = null;
		if(pathFile_ReportScript.exists() && pathFile_ReportScript.isFile()) 
			tmp = fileIO.readFuncFromRscript(pathFile_ReportScript);		
		if (tmp!=null){			
			String[] tmp1= new String[tmp.length+2]; // Add standard entries
			for(int i=0;i<tmp.length;i++){tmp1[i]=tmp[i];}
			tmp1[tmp1.length-4]="spotReportDefault";
			tmp1[tmp1.length-3]="spotReportSens";
			tmp1[tmp1.length-2]="spotReport3d";
			tmp1[tmp1.length-1]="spotReportContour";
			ComboBoxModel jComboBox_CONFrepnameModel = new DefaultComboBoxModel(tmp1);
			jComboBox_reportFunction.setModel(jComboBox_CONFrepnameModel);
			jComboBox_reportFunction.setSelectedItem(null);
			for(int i=0;i<jComboBox_reportFunction.getItemCount();i++){
				if(jComboBox_reportFunction.getItemAt(i).toString().split(" <- ")[0].equals(reportFunction)){
					jComboBox_reportFunction.setSelectedIndex(i);
					break;
				}
			}
		}		
		// ***5/6***
		if(!conf_algPdFile.equals("") && !conf_algPdFile.equals("NA")){
			try {jTextField_CONFalgPdFilePath.setText(new File(confFile.getParent()+FILE_SEPERATOR+conf_algPdFile).getCanonicalPath());
			} catch (IOException e1) {e1.printStackTrace();}
			pathFile_APD = new File(jTextField_CONFalgPdFilePath.getText());
			pathFCDefaultAPD = pathFile_APD.getParentFile();
		}
		// Project path
		jTextField_CONFprojectPath.setText(confFile.getParent());
		path_Project = new File(jTextField_CONFprojectPath.getText());
		// Project prefix
		jTextField_CONFprojectPrefix.setText(confFile.getName().replaceFirst("\\..*", ""));

		// ***SHOULD PARAMETERS
		// conf_model *
		boolean modelIsKnown=false;
		if (!conf_model.equals("")){
			for(String i:modelStrings)
			{
				if(i.equals(conf_model))
				{
					jComboBox_CONFmodel.setSelectedItem(conf_model);
					modelIsKnown=true;
					break;
				}
			}
			if (!modelIsKnown){//Model is not known, and thus added to advanced setup
				jTextArea_CONFadditional.append("\nseq.predictionModel.func = "+hlp.strQuote(conf_model));
				if(!jCheckBox_CONFadvanced.isSelected()){
					jCheckBox_CONFadvanced.setSelected(true);
					jTextArea_CONFadditional.setEnabled(true);
				}		
			}
		}
		// conf_design_type *
		boolean designIsKnown=false;
		if (!conf_design_type.equals("")){	
			for(String i: designTypeStrings){
				if(i.equals(conf_design_type)){
					jComboBox_CONFdesignType.setSelectedItem(conf_design_type);
					designIsKnown=true;
					break;
				}
			}
			if (!designIsKnown){ //unkown initial design function is added to advanced setup
				jTextArea_CONFadditional.append("\ninit.design.func = "+conf_design_type);
				if(!jCheckBox_CONFadvanced.isSelected()){
					jCheckBox_CONFadvanced.setSelected(true);
					jTextArea_CONFadditional.setEnabled(true);
				}				
			}
		}
		// conf_init_design_size
		if(!conf_init_design_size.equals(""))
			jTextField_CONFinitLhdPoints.setText(conf_init_design_size);
		// conf_seq_design_size
		if(!conf_seq_design_size.equals(""))
			jTextField_CONFseqDesignSize.setText(conf_seq_design_size);
		// conf_auto_loop_steps
		if(!conf_auto_loop_steps.equals(""))
			jTextField_CONFautoLoopSteps.setText(conf_auto_loop_steps);
		if(!conf_auto_loop_nevals.equals(""))
			jTextField_CONFautoLoopNEvals.setText(conf_auto_loop_nevals);
		// ***ADDITIONAL PARAMETERS ==> handeled in makeStringListToConfVal
	}
	//##################################################################################################
	/**
	 * For multiple colored TextPane
	 */
	public class ColorPane extends JTextPane {
		private static final long serialVersionUID = 9915L;
		public void append(Color c, String s) { // better implementation--uses StyleContext
			boolean flag = false;
			if(!isEditable()){
				flag = true;
				setEditable(true);
			}
			StyleContext sc = StyleContext.getDefaultStyleContext();
			AttributeSet aset = sc.addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.Foreground, c);
			int len = getDocument().getLength(); // same value as getText().length();
			setCaretPosition(len);  // place caret at the end (with no selection)
			setCharacterAttributes(aset, false);
			replaceSelection(s); // there is no selection, so inserts at caret
			if(flag){
				setEditable(false);
			}
			this.repaint();
		}
	}	
	//##################################################################################################
	// CONF<-->STRINGLIST und ROI<-->STRINGLIST Funktionen

	/**
	 * Generates and returns a parseable StringList out of the ROI variables.
	 */
	public ArrayList<String> makeRoiValToStringList(){
		ArrayList<String> content = new ArrayList<String>();
		content.add("name low high type");
		for(int i=0;i<roiTableDim;i++)
			content.add(roiTableData[i][0].toString() +sepRoiFile+ roiTableData[i][1].toString() +sepRoiFile+ roiTableData[i][2].toString() +sepRoiFile+ roiTableData[i][3].toString());
		return content;
	}
	/**
	 * Parses the StringList for ROI variables and overwrite current values.
	 * Refresh will performed automatically at the end.
	 */
	public int makeStringListToRoiVal(ArrayList<String> myStrList){
		if(myStrList.size()-1>roiTableMaxDim){
			System.err.println("ERROR(makeStringListToRoiVal): ROI file has more than roiTableMaxDim ("+Integer.toString(roiTableMaxDim)+") parameterlines (count of lines was: "+Integer.toString(myStrList.size())+")");
			return -1;
		}
		String[] linesplit = new String[3];
		int i=1; // line 0 = Columnnames
		for (;i<myStrList.size();i++){
			linesplit = myStrList.get(i).split(sepRoiFile);
			jTable_ROI.setValueAt(linesplit[0], i-1, 0);
			jTable_ROI.setValueAt(linesplit[1], i-1, 1);
			jTable_ROI.setValueAt(linesplit[2], i-1, 2);
			jTable_ROI.setValueAt(linesplit[3], i-1, 3);
		}
		updateTableDim(i-1);
		return 0;
	}
	/**
	 * Parses the String str for CONF variables and overwrite current values.
	 * Refresh will performed automatically at the end.
	 */
	public int makeStringListToConfVal(ArrayList<String> myStrList){
		if(myStrList==null) return -1;
		boolean DEBUG = true;
		//boolean skip=false;;
		//ArrayList<String> must_set = new ArrayList<String>();
		//ArrayList<String> should_set = new ArrayList<String>();
		ArrayList<String> additional = new ArrayList<String>();
		if(DEBUG) System.out.println("\n\n\n**************************************");
		if(DEBUG) System.out.println("** ENTERING makeStringListToConfVal **");
		if(DEBUG) System.out.println("**************************************");
		String[] var = new String[2];
		String tmp = new String();
		if(DEBUG) System.out.println("linecount: "+myStrList.size());
		clearConfVars();
		for(int i=0;i<myStrList.size();i++){
			//skip = false;
			tmp = myStrList.get(i);

			tmp = tmp.replace(";","");				// ";" entfernen
			tmp = tmp.replace("<-","="); 			// "<-" durch "=" ersetzen
			tmp = tmp.replaceFirst(" *= *", "="); 	// spaces um "=" herum entfernen
			tmp = tmp.replaceFirst("\\s*#.*", "");	// angehÃ¤ngter Zeilenkommentar entfernen
			tmp = tmp.replaceAll("\"", "");			// quotes entfernen
			tmp = tmp.replace("\\\\","\\").replace("\\",FILE_SEPERATOR).replace("/",FILE_SEPERATOR);

			String checktmp = tmp.split("=")[0];
			// is MUST_SET configparameter ?
			// VERY UGLY kind of programming		
					if(checktmp.equals("alg.func")){
						var = tmp.split("=",2);
						conf_algName = var[1];
					}else{
						if(checktmp.equals("io.apdFileName")){
							var = tmp.split("=",2);
							/*relativePathFileString_algPd = var[1];*/
							conf_algPdFile = var[1];
						}else{
							// else is SHOULD_SET configparameter ?
							if(checktmp.equals("seq.predictionModel.func")){
								var = tmp.split("=",2);
								conf_model = var[1];
							}else{
								if(checktmp.equals("init.design.func")){
									var = tmp.split("=",2);
									conf_design_type = var[1];
								}else{
									if(checktmp.equals("init.design.size")){
										var = tmp.split("=",2);
										conf_init_design_size = var[1];
									}else{
										if(checktmp.equals("seq.design.size")){
											var = tmp.split("=",2);
											conf_seq_design_size = var[1];
										}else{
											if(checktmp.equals("auto.loop.steps")){
												var = tmp.split("=",2);
												conf_auto_loop_steps = var[1];
											}else{
												if(checktmp.equals("auto.loop.nevals")){
													var = tmp.split("=",2);
													conf_auto_loop_nevals = var[1];
												}else{													
													if(checktmp.equals("report.func")){
														var = tmp.split("=",2);
														reportFunction = var[1]; 
													}else{
															if(checktmp.equals("report.io.pdf")){
																//this is always set in the gui, so not store in variable
															}else{
																// ADDITIONAL configparameter !
																additional.add(myStrList.get(i));
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
		// ADDITIONAL configparameter !
		if(additional.size()>0) {
			jCheckBox_CONFadvanced.setSelected(true);
			jTextArea_CONFadditional.setEnabled(true);
			jTextArea_CONFadditional.setText("");
			jTextArea_CONFadditional.append(additional.get(0));
			for(int i=1;i<additional.size();i++){
				jTextArea_CONFadditional.append("\n"+additional.get(i));
			}
		}else{
			jCheckBox_CONFadvanced.setSelected(false);
			jTextArea_CONFadditional.setEnabled(false);
		}
		return 0;
	}
	/**
	 * Generates and returns a parseable String out of the CONF variables.
	 */
	public ArrayList<String> makeConfValToStringList(){
		ArrayList<String> confArrayList = new ArrayList<String>();
		confArrayList.add("##############################################################");
		confArrayList.add("### *** This CONF was generated by SPOT GUI (ver. "+SPOTGUIVERSION+") *** ###");
		confArrayList.add("##############################################################");
		confArrayList.add("");
		confArrayList.add("alg.func = "+hlp.strQuote(conf_algName));
		if(confAPDSelectedCheck && confAPDExistCheck)confArrayList.add("io.apdFileName = "+hlp.strQuote(conf_algPdFile.replace("\\\\","\\").replace("\\","\\\\")));
		confArrayList.add("io.verbosity=3");
		confArrayList.add("");
		confArrayList.add("init.design.func = "+hlp.strQuote(conf_design_type));
		confArrayList.add("init.design.size = "+conf_init_design_size);
		confArrayList.add("seq.predictionModel.func = "+hlp.strQuote(conf_model));
		confArrayList.add("auto.loop.nevals = "+conf_auto_loop_nevals);
		confArrayList.add("auto.loop.steps = "+conf_auto_loop_steps);
		confArrayList.add("seq.design.size = "+conf_seq_design_size); 
		confArrayList.add("");
		confArrayList.add("###########################");
		confArrayList.add("### ** REPORT CONFIG ** ###");
		confArrayList.add("###########################");
		confArrayList.add("");
		confArrayList.add("report.func = "+hlp.strQuote(reportFunction));
		confArrayList.add("report.io.pdf = TRUE");	// allways create PDF if possible
		confArrayList.add("report.interactive = FALSE");	// no interactive reports
		if(jCheckBox_CONFadvanced.isSelected()){
			String[] additional = jTextArea_CONFadditional.getText().split("\\n");
			if(additional.length>=1){
				confArrayList.add("");
				confArrayList.add("###############################");
				confArrayList.add("### ** ADDITIONAL CONFIG ** ###");
				confArrayList.add("###############################");
				confArrayList.add("");
			}
			for(int i=0;i<additional.length;i++){
				if(!additional[i].matches("\\s*#.*")) // ONLY NON COMMENT LINES
					confArrayList.add(additional[i]);
			}
		}
		return confArrayList;
	}	
	//##################################################################################################
	// CHECK Funktionen	
	/**
	 * CHECK EVERYTHING BEFORE SPOT OPT LUNCH
	 */
	public int checkSPO(){
		if(new File(path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+".conf").exists()){
			int answer = JOptionPane.showConfirmDialog(jFrame, "Project already exists. Do you want to overwrite ?","Overwrite existing project ?", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
			if (answer == JOptionPane.NO_OPTION)
				return -1;
		}
		jColorPane_spotRun.setText("");
		jColorPane_spotRun.append(Color.blue, "[*** UPDATE CONFIG PARAMETERS CHANGE ***]");
		refreshConf();
		jColorPane_spotRun.append(Color.black, "\n...");
		jColorPane_spotRun.append(Color.blue, "done");
		jColorPane_spotRun.append(Color.black, "...");
		jColorPane_spotRun.append(Color.blue, "\n\n[*** STARTING CHECK ***]");
		//Algorithm select
		boolean algoScriptSelectedCheck = false;
		boolean algoScriptExistCheck = false;
		//ROI
		boolean roiBoundvaluesCheck = true; // set true because of falsification
		boolean roiFactorCheck = true; // set true because of falsification
		boolean writeROI = false;
		//CONF
		boolean confRFunctionSelectedCheck = false;
		confAPDSelectedCheck = false;
		confAPDExistCheck = false;
		boolean confProjectPathSelectedCheck = false;
		boolean confProjectPrefixCheck = false;
		boolean confInitLhdPointsCheck = false;
		boolean confAutoLoopStepsCheck = false;
		boolean confAutoLoopNevalsCheck = false;
		boolean LimitCheck = false;
		boolean confIStepSampleSizeCheck = false;
		boolean confAdvancedConfCheck = false;
		boolean writeCONF = false;
		//File setup check
		boolean pathRCheck = false;
		//WRITE ROI+CONF 
		boolean fileROICheck = false;
		boolean fileCONFCheck = false;

		// ***CHECK ALGORITHM SCRIPT***
		// 		CHECK: AlgorithmScript selected ?
		// 		CHECK: AlgorithmScript exist ?
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: ALGORITHM SCRIPT - filepath selected......");
		if(!jTextField_selectedAlgorithmScript.getText().equals("")){
			algoScriptSelectedCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
			jColorPane_spotRun.append(Color.black, "\n...[CHECK]: ALGORITHM SCRIPT - file exists......");
			if(new File(jTextField_selectedAlgorithmScript.getText()).exists()){
				algoScriptExistCheck = true;
			}else jColorPane_spotRun.append(Color.red, "FAILED");
		}else{
			if(jComboBox_CONFalgname.getSelectedItem().toString().equals("spotFuncStartBranin")||jComboBox_CONFalgname.getSelectedItem().toString().equals("spotFuncStartSixHump")||jComboBox_CONFalgname.getSelectedItem().toString().equals("spotFuncStartSphere")||jComboBox_CONFalgname.getSelectedItem().toString().equals("spotAlgStartSann")||jComboBox_CONFalgname.getSelectedItem().toString().equals("spotAlgStartEs")){//If functions distributed with spot are selected instead of a file: check unnecessary
				jColorPane_spotRun.append(Color.blue, "SKIPPED, Algorithm is internal");
				algoScriptSelectedCheck = true;
				algoScriptExistCheck = true;
			}else jColorPane_spotRun.append(Color.red, "FAILED");			
		}		
		// ***CHECK ROI***
		// 		CHECK: ROI Table Lower Bounds <= Upper Bounds ?
		//					 		CHECK: ROI Table IF Type=FACTOR ==> Inf/-Inf ?
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: ROI - lower bound is less or equal upper bound......");
		for(int i=0;i<roiTableDim;i++){
			if(jTable_ROI.getValueAt(i,3).toString().equalsIgnoreCase("FACTOR") && ( jTable_ROI.getValueAt(i,1).toString().contains("Inf") || jTable_ROI.getValueAt(i,2).toString().contains("Inf") )){
				roiFactorCheck = false;
			}
			if(!(jTable_ROI.getValueAt(i, 1).toString().equals("Inf") || jTable_ROI.getValueAt(i, 1).toString().equals("-Inf") || jTable_ROI.getValueAt(i, 2).toString().equals("Inf") || jTable_ROI.getValueAt(i, 2).toString().equals("-Inf"))){
				if(Float.valueOf(jTable_ROI.getValueAt(i, 1).toString()) > Float.valueOf(jTable_ROI.getValueAt(i, 2).toString()))
					roiBoundvaluesCheck = false;				
			}else{ 
				if(jTable_ROI.getValueAt(i, 1).toString().equals("Inf") || jTable_ROI.getValueAt(i, 2).toString().equals("-Inf"))
					roiBoundvaluesCheck = false;				
			}
		}
		if(roiBoundvaluesCheck){
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");				
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: ROI - not used type factor on Inf/-Inf bounds......");
		if(roiFactorCheck){
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");		
		// ***CHECK PATH***
		// 		CHECK: pathFile_R exist ?
		if (IS_OS_WINDOWS){
			jColorPane_spotRun.append(Color.black, "\n...[CHECK]: PATH SETUP - valid filepath to R.exe selected......");
			if(pathFile_R.getName().equalsIgnoreCase("R.exe")){
				pathRCheck = true;
				jColorPane_spotRun.append(Color.green, "OK");
			}else jColorPane_spotRun.append(Color.red, "FAILED");			
		} else if(IS_OS_LINUX || IS_OS_MAC)
			pathRCheck = true;
		// 		CHECK: path_SPOT exist ?
		// ***CHECK CONF***
		// 		CHECK: RFunctionSelected (only if alg.language=sourceR)
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - algorithm R function selected......");
		if(jComboBox_CONFalgname.getSelectedIndex()>=0){
			confRFunctionSelectedCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");
		// 		CHECK: APDSelected
		// 		CHECK: APDExist
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - APD filepath selected......");
		if(!pathFile_APD.toString().equals("")){
			confAPDSelectedCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
			jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - APD file exists......");
			if(confAPDSelectedCheck && pathFile_APD.exists()){
				confAPDExistCheck = true;
				jColorPane_spotRun.append(Color.green, "OK");
			}else jColorPane_spotRun.append(Color.blue, "WARNING, not existing");			
		}else jColorPane_spotRun.append(Color.blue, "WARNING, none selected"); // apd file is not necessary, only a warning
		// 		CHECK: ProjectPathSelected
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - project path selected......");
		if(!path_Project.toString().equals("")){
			confProjectPathSelectedCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");		
		// 		CHECK: ProjectPrefix
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - project prefix name is valid......");
		if(hlp.validPref(jTextField_CONFprojectPrefix.getText())){
			confProjectPrefixCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");
		// 		CHECK: InitDesignSize
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - value of init.lhd.points is valid......");
		if(hlp.validIntNa(jTextField_CONFinitLhdPoints.getText())){
			confInitLhdPointsCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");		
		// 		CHECK: AutoLoopSteps
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - value of auto.loop.steps is valid......");
		if(hlp.validIntInf(jTextField_CONFautoLoopSteps.getText())){
			confAutoLoopStepsCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");		
		//		CHECK: AutoLoopNevals
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - value of auto.loop.nevals is valid......");
		if(hlp.validIntInf(jTextField_CONFautoLoopNEvals.getText())){
			confAutoLoopNevalsCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");
		// CHECK: Check if both limits (autoloopnevals/steps) are set to inf: no limit!
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - auto.loop.nevals and auto.loop.steps can not both be 'Inf'......");
		if(!(jTextField_CONFautoLoopNEvals.getText().equals("Inf")&&jTextField_CONFautoLoopSteps.getText().equals("Inf")))
		{
			LimitCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");		
		// 		CHECK: SeqDesignSize
		jColorPane_spotRun.append(Color.black, "\n...[CHECK]: CONF - value of seq.design.size is valid......");
		if(hlp.validIntNa(jTextField_CONFseqDesignSize.getText())){
			confIStepSampleSizeCheck = true;
			jColorPane_spotRun.append(Color.green, "OK");
		}else jColorPane_spotRun.append(Color.red, "FAILED");
		// 		CHECK: AdvancedConf
		jColorPane_spotRun.append(Color.black, "\n...note: additonal parameters are not checked, to assure full compatibility");
		//if(true){
			confAdvancedConfCheck = true;
			// TODO: advanced config is not checked right now
			//jColorPane_spotRun.append(Color.green, "OK");
		//}else{
			//jColorPane_spotRun.append(Color.red, "FAILED");
		//}

		// ***CHECK & SET IMAGES***
		// Algorithm script
		if(algoScriptSelectedCheck && algoScriptExistCheck){
			jLabel_checkSpotRun_algorithm.setIcon(iconCheckValid);
		}else{
			jLabel_checkSpotRun_algorithm.setIcon(iconCheckInvalid);
		}
		// PROJECT CONF FILEPATH GENERIEREN
		// PROJECT ROI FILEPATH GENERIEREN
		if(!path_Project.toString().equals("") && confProjectPrefixCheck){
			pathFile_ProjectConf = new File(path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+".conf");
			pathFile_ProjectRoi = new File(path_Project.toString()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+".roi");
		}
		// ROI
		if(roiBoundvaluesCheck && roiFactorCheck){
			jLabel_checkSpotRun_roiConfig.setIcon(iconCheckValid);
			if(confProjectPathSelectedCheck && confProjectPrefixCheck){
				writeROI = true; // ROI darf geschrieben werden
			}
		}
		else{
			jLabel_checkSpotRun_roiConfig.setIcon(iconCheckInvalid);
		}
		// CONF
		if(algoScriptSelectedCheck && algoScriptExistCheck && /*confLanguageSelectedCheck &&*/ confRFunctionSelectedCheck && confProjectPathSelectedCheck && confProjectPrefixCheck && confInitLhdPointsCheck && confAutoLoopStepsCheck && confAutoLoopNevalsCheck && LimitCheck && confIStepSampleSizeCheck && confAdvancedConfCheck){
			jLabel_checkSpotRun_spoConfig.setIcon(iconCheckValid);
			writeCONF = true; // CONF darf geschrieben werden
		}else{
			jLabel_checkSpotRun_spoConfig.setIcon(iconCheckInvalid);
		}
		// PATH (R)
		if(pathRCheck){
			jLabel_checkSpotRun_rPath.setIcon(iconCheckValid);
		}else{
			jLabel_checkSpotRun_rPath.setIcon(iconCheckInvalid);
		}
		// ---WRITE ROI FILE---
		if(writeROI){
			jColorPane_spotRun.append(Color.black, "\n...[CONFIG FILES]: ROI - generate and write file......");
			if(fileIO.writeStrListTo(makeRoiValToStringList(), pathFile_ProjectRoi)==0){
				fileROICheck = true;
				jColorPane_spotRun.append(Color.green, "OK");
			}else jColorPane_spotRun.append(Color.red, "FAILED");			
		}
		// ROI File
		if(!writeROI) jLabel_checkSpotRun_roiConfigFile.setIcon(iconCheckUnknown);
		else if(fileROICheck) jLabel_checkSpotRun_roiConfigFile.setIcon(iconCheckValid);
		else jLabel_checkSpotRun_roiConfigFile.setIcon(iconCheckInvalid);		
		// ---WRITE CONF FILE---
		if(writeCONF){
			jColorPane_spotRun.append(Color.black, "\n...[CONFIG FILES]: CONF - generate and write file......");
			if(fileIO.writeStrListTo(makeConfValToStringList(), pathFile_ProjectConf)==0){
				fileCONFCheck = true;
				jColorPane_spotRun.append(Color.green, "OK");
			}else jColorPane_spotRun.append(Color.red, "FAILED");			
		}
		// CONF File
		if(!writeCONF)jLabel_checkSpotRun_spoConfigFile.setIcon(iconCheckUnknown);
		else if(fileCONFCheck) jLabel_checkSpotRun_spoConfigFile.setIcon(iconCheckValid);
		else jLabel_checkSpotRun_spoConfigFile.setIcon(iconCheckInvalid);				
		// if ALL is ok (conf Files written & spot/R path valid), enable RUN Button
		if(fileROICheck && fileCONFCheck && pathRCheck){
			jColorPane_spotRun.append(Color.green, "\nALL CHECKS PASSED SUCCESSFULLY ... FEEL FREE TO START SPOT");
			return 0;	
		}else{
			jColorPane_spotRun.append(Color.red, "\nSOME CHECKS FAILED ... PLEASE RECONFIGURE FAILED PARTS");
			return -1;
		}
	}
	//##################################################################################################
	// IMPORT/EXPORT bezogene Funktionen
	/**
	 * Uses makeRoiToString() and makeConfToString() to generate a allinone String seperated by a special lineseparator.
	 * Finaly the allinone string will be written via writeStringToFile.
	 */
	private int exportSetting(File settingsFile){
		if(pathFCDefaultExportSettigns.isFile()){
			int answer = JOptionPane.showConfirmDialog(exportMenuItem.getParent(), "The choosen file ("+pathFCDefaultExportSettigns.getName()+") already exists. Do you want to overwrite? ","File exists", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
			if (answer == JOptionPane.NO_OPTION)
				return -1;
		}
		updateTableDim(roiTableDim);
		refreshConf();
		ArrayList<String> arrayListRoi = makeRoiValToStringList();
		ArrayList<String> arrayListConf = makeConfValToStringList();
		if(arrayListRoi==null){
			System.err.println("exportSetting returned -2");
			return -2;
		}
		if(arrayListConf==null){
			System.err.println("exportSetting returned -3");
			return -3;
		}
		ArrayList<String> arrayListSettings = new ArrayList<String>();
		for(int i=0;i<arrayListRoi.size();i++)
			arrayListSettings.add(arrayListRoi.get(i));
		arrayListSettings.add(sepLineSettings);
		for(int i=0;i<arrayListConf.size();i++)
			arrayListSettings.add(arrayListConf.get(i));
		arrayListSettings.add(jTextField_CONFprojectPath.getText()+FILE_SEPERATOR+jTextField_CONFprojectPrefix.getText()+".conf");
		int ret = fileIO.writeStrListTo(arrayListSettings, settingsFile);
		if (ret!=0){
			System.err.println("exportSetting returned -4");
			return -4;
		}
		return 0;
	}
	/**
	 * Reads String out of File via readFileToString and splitt into two substrings via the special lineseparator.
	 * Finaly the substrings will be converted to Values by makeStringToRoiVal and makeStringToConfVal.
	 * After all a refresh will be performed.
	 */
	private int importSetting(File settingsFile){
		if(!settingsFile.isFile()){
			System.err.println("importSetting returned -1");
			return -1;
		}
		ArrayList<String> arrayListSettings = fileIO.readToStrList(settingsFile);
		if (arrayListSettings==null){
			System.err.println("importSetting returned -2");
			return -2;
		}
		ArrayList<String> arrayListRoi = new ArrayList<String>();
		ArrayList<String> arrayListConf = new ArrayList<String>();
		int indexOfSeperator = 0;
		boolean validSeperator = false;
		for (int i=0;i<arrayListSettings.size();i++){
			if (arrayListSettings.get(i).equals(sepLineSettings)){
				indexOfSeperator = i;
				validSeperator = true;
				i = arrayListSettings.size(); // or just break
			}
		}
		if(!validSeperator){
			System.err.println("ERROR(importSetting): File does not contain the valid separator for settings files ("+sepLineSettings+")");
			return -3;
		}
		for(int i=0;i<indexOfSeperator;i++)
			arrayListRoi.add(arrayListSettings.get(i));
		for(int i=indexOfSeperator+1;i<arrayListSettings.size()-1;i++)
			arrayListConf.add(arrayListSettings.get(i));
		pathFile_ProjectConf = new File(arrayListSettings.get(arrayListSettings.size()-1));
		int retRoi = makeStringListToRoiVal(arrayListRoi);
		int retConf = makeStringListToConfVal(arrayListConf);
		if (retRoi!=0 || retConf!=0){
			if(retRoi!=0 && retConf!=0){
				System.err.println("importSetting returned -4");
				return -4;	// ROI+CONF invalid
			}
			if(retRoi!=0){
				System.err.println("importSetting returned -5");
				return -5; // ROI invalid
			}
			System.err.println("importSetting returned -6");
			return -6; // CONF invalid
		}
		loadConfValues(pathFile_ProjectConf);
		return 0;
	}	
	//##################################################################################################
	//AUTOSTART	
	/**
	 * Checks OS and JVM version
	 */
	public void systemCheck(){		
		boolean error = false;
		if(SystemUtils.FILE_SEPARATOR==null){
			System.err.println("Could not read SystemUtils.FILE_SEPARATOR");
			error = true;
		}
		if(SystemUtils.LINE_SEPARATOR==null){
			System.err.println("Could not read SystemUtils.LINE_SEPARATOR");
			error = true;
		}
		if((Object)SystemUtils.IS_OS_WINDOWS==null){
			System.err.println("Could not read SystemUtils.IS_OS_WINDOWS");
			error = true;
		}
		if((Object)SystemUtils.IS_OS_LINUX==null){
			System.err.println("Could not read SystemUtils.IS_OS_LINUX");
			error = true;
		}
		if((Object)SystemUtils.IS_OS_MAC==null){
			System.err.println("Could not read SystemUtils.IS_OS_MAC");
			error = true;
		}
		if(error) 
			return;
		//---------------
		IS_OS_WINDOWS = SystemUtils.IS_OS_WINDOWS;
		IS_OS_LINUX = SystemUtils.IS_OS_LINUX;
		IS_OS_MAC = SystemUtils.IS_OS_MAC;
		FILE_SEPERATOR = SystemUtils.FILE_SEPARATOR;
		LINE_SEPERATOR = SystemUtils.LINE_SEPARATOR;
		if(globalDEBUG) System.out.println("\n### SYSTEM CHECK START ###");
		if(globalDEBUG) System.out.println("Current JVM version: "+System.getProperty("java.version"));
		if(SystemUtils.IS_JAVA_1_6) {
			if(globalDEBUG) System.out.println("   ==> JVM-CHECK: valid");
		} else {
			System.err.println("   ==> JVM-CHECK Warning: At least Java 1.6 is required for this application.");
		}
		if(globalDEBUG) System.out.println("Current operating system: "+System.getProperty("os.name")+" (OS-Architecture: "+System.getProperty("os.arch")+", OS-Version: "+System.getProperty("os.version")+")" );
		if(IS_OS_WINDOWS || IS_OS_LINUX || IS_OS_MAC) {
			if(globalDEBUG) System.out.println("   ==> OS-CHECK: valid");
		} else {
			System.err.println("   ==> OS-CHECK Warning: This application was only tested under Windows (XP), Linux, MAC OS and may cause problems on your platform.");
		}
		if(globalDEBUG) System.out.println("IS OS WINDOWS: "+IS_OS_WINDOWS);
		if(globalDEBUG) System.out.println("IS OS LINUX: "+IS_OS_LINUX);
		if(globalDEBUG) System.out.println("IS OS MAC: "+IS_OS_MAC);
		if(globalDEBUG) System.out.println("OS FILE SEPERATOR: "+hlp.strQuote(FILE_SEPERATOR));
		if(globalDEBUG) System.out.println("### SYSTEM CHECK END ###\n");
		if(!IS_OS_WINDOWS){ // Path Setup ist unter Linux/Mac nichtmehr nötig, da das SPOT Verzeichnis nichtmehr gesucht werden muss
			setRPathMenuItem.setEnabled(false);
			setRPathMenuItem.setVisible(false);
		}
	}
	/**
	 * LOAD PATH SETUP
	 */
	private void loadPathSetup(File psFile){
		if(pathFile_SetupProperties.exists()){
			try {
				setupProperties.loadFromXML(new FileInputStream(psFile));
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}				
			if(setupProperties.containsKey("pathFile_R")){//Check if setup contains the r-path, and use it accordingly
				File tmp = new File(setupProperties.getProperty("pathFile_R"));
				if(IS_OS_WINDOWS){
					if(!tmp.toString().equals("") && tmp.exists()){
						pathFile_R = tmp;
						pathFCDefaultRPATH = pathFile_R;
						setupPropertieExist = true;
						if(globalDEBUG) System.out.println("re-loaded pathFile_R:\""+pathFile_R.toString()+"\"");
					}
				}else{}//maybe user want to set R path to use a userdefined version of R and not the default R version
			}			
			// Man könnte die unten stehenden default paths auch in den setupproperties speichern und dann hier wie den r path laden.
			if(setupPropertieExist){ //TODO Der hier zugeführte Default path muss eigtl. ein anderer sein, war vorher der spopathdefault, der wurde gelöscht... muss überdacht werden
				//pathFCDefaultImportSettigns = pathFCDefaultRPATH;	pathFCDefaultExportSettigns = pathFCDefaultRPATH;pathFCDefaultCONFproject = pathFCDefaultRPATH;pathFCDefaultCONFalgorithmScript = pathFCDefaultRPATH;pathFCDefaultLoadROI = pathFCDefaultRPATH;pathFCDefaultLoadCONF = pathFCDefaultRPATH;pathFCDefaultReportScript = pathFCDefaultRPATH;pathFCDefaultAPD = pathFCDefaultRPATH;
			}else if(!IS_OS_LINUX){
				JOptionPane.showMessageDialog(jFrame, "Your path setup file was removed, because it was invalid. Please do the \"Path setup\", to assure comfortable and proper usage.", "Information", JOptionPane.ERROR_MESSAGE);
				psFile.delete();
			}
		}else if(!IS_OS_LINUX){
			JOptionPane.showMessageDialog(jFrame, "If this is the first time the GUI is launched, please do the \"Path setup\" first of all, to assure comfortable and proper usage.", "Information", JOptionPane.INFORMATION_MESSAGE);
		}
	}
	//##################################################################################################
}
