package spotGui;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;

import org.apache.commons.lang.SystemUtils;

public class fileIO {
	/**
	 * Writes the StringList myStr to the File myFile.
	 * Folders will be created if nessecary.
	 * If File exists already, it will be overwriten.
	 */
	public static int writeStrListTo(ArrayList<String> myStrList, File myFile){
		File pathOnly = new File(myFile.getParent());
		if(myFile.isDirectory()){
			System.out.println("ERROR(writeStringListToFile): File was a Directory");
			return -1;
		}
		if(!pathOnly.exists()){
			pathOnly.mkdirs();
			System.out.println("ROI path created: \""+pathOnly.toString()+"\"");
		}
		
		try { 
			FileWriter fw = new FileWriter(myFile); 
			BufferedWriter bw = new BufferedWriter(fw); 
			bw.write(myStrList.get(0));
			for(int i=1;i<myStrList.size();i++)
				bw.write(SystemUtils.LINE_SEPARATOR+myStrList.get(i));
			bw.close();
			System.out.println("File written: \""+myFile.toString()+"\"");
			}
			catch (ArrayIndexOutOfBoundsException aioobe) { 
				System.err.println("ERROR(writeStringListToFile): ArrayIndexOutOfBoundsException"+SystemUtils.LINE_SEPARATOR+"==> "+aioobe);
				return -2;
			}
			catch (IOException ioe) {
				System.err.println("ERROR(writeStringListToFile): IOException"+SystemUtils.LINE_SEPARATOR+"==> "+ioe);
				return -3;
			} 
		return 0;
	}

	/**
	 * Reads the File myFile and returns the content as an arraylist (lines of file)
	 */
	public static ArrayList<String> readToStrList(File myFile){
		//String LINE_SEPERATOR = "";
		if(!myFile.exists()){
			System.err.println("ERROR(readFileToString): File does not exist");
			return null;
		}
		if(!myFile.canRead()){
			System.err.println("ERROR(readFileToString): Cannot read file");
			return null;
		}
		ArrayList<String> content = new ArrayList<String>();
		try { 
			FileReader fr = new FileReader(myFile); 
			BufferedReader br = new BufferedReader(fr); 
			String line = null;
			int i=0;
			for (;(line=br.readLine()) != null;i++)
				if(!line.matches("\\s*#.*") && !line.equals("")) //dont read comments or empty lines
					content.add(line);
			br.close();
		}
			catch (ArrayIndexOutOfBoundsException aioobe) { 
				System.err.println("ERROR(readFileToString): ArrayIndexOutOfBoundsException"+SystemUtils.LINE_SEPARATOR+"==> "+aioobe);
				return null;
		}
			catch (IOException ioe) {
				System.err.println("ERROR(readFileToString): IOException"+SystemUtils.LINE_SEPARATOR+"==> "+ioe);
				return null;
			} 
		return content;
	}
	/**
	 * Write a script file to call with rscript.exe, calling the spot package
	 */	
	public static void writeRSpot(String conf,String task, String tar, String log, String algpath)
	{ //conf string is file path to configuration file, task string is task of spot to be done ("auto","init", etc), tar is file name of script that will be called by rscript.exe
		try
		{			
			File file = new File(tar);
			PrintWriter out = new PrintWriter(new FileWriter(file));
			out.println("sink(\""+log+"\",split=T)");
			out.println("require(SPOT)"); //load spot package in R
			if(!algpath.equals(""))out.println("source(\""   + algpath + "\")"); //load custom function file
			out.println("mySpotConfig = spot(\""+conf+"\",\""+task+"\")"); //construct call to spot package with given task, and configuration file
			out.println("sink()");
			out.close();
		}
		catch( IOException e )
		{
			e.printStackTrace();
		}
		
	}
	/**
	 * Get Textfile as StringBuffer
	 * String tar is the path to the target file
	 */	
	public static StringBuffer getTxt(String tar) 
	{	
	File file = new File(tar); // new File from target string
	StringBuffer cont = new StringBuffer(); 
    BufferedReader reader = null;
    try{
        reader = new BufferedReader(new FileReader(file));
        String text = null;
        // repeat until all lines is read
        while ((text = reader.readLine()) != null){
            cont.append(text)
                .append(System.getProperty(
                    "line.separator"));
        }
    } catch (FileNotFoundException e0){
        e0.printStackTrace();
    }catch (IOException e1){
        e1.printStackTrace();
    } finally{
        try{
            if (reader != null)
                reader.close();
        } catch (IOException e2){
            e2.printStackTrace();
        }
    }	
    return cont;// return file contents here    
	}
	/**
	 * Read R script file and return all function lines as formatted String array
	 */
	public static String[] readFuncFromRscript(File myFile){
		if(!myFile.exists()) return null;
		ArrayList<String> arrayListFullFile = fileIO.readToStrList(myFile);
		ArrayList<String> arrayListFunctions = new ArrayList<String>();
		String tmpLine = "";
		for (int i=0;i<arrayListFullFile.size();i++){
			tmpLine = arrayListFullFile.get(i).split("#")[0].replace(" ", "").replace("\t","").replace("{", ""); //  this makes sure not to get some comment
			if(tmpLine.contains("=function")){
				tmpLine = tmpLine.replace("=function", "<-function");
			}
			if (tmpLine.contains("<-function")){ // this check make sure not to get a line within a variable wich name includes "function"
				arrayListFunctions.add(tmpLine.replace("<-", " <- "));
				//System.out.println("\""+tmpLine+"\"");
			}
		}
		return (String[])arrayListFunctions.toArray(new String[arrayListFunctions.size()]);
	}
	/**
	 * Write a target function created with tarFuncGenerator
	 */	
	public static void writeTarFunc(File myFile, String fFormula, int dim, boolean noise, String noiseAmp) 
	{ //myFile is name of file to be created;  fFormula is the Formula of the function, dim is the dimension of the formula (number of x variables, e.g. x1, x2, x3, x4 ---> dim=4), noise tells if noise should be added to formula, noiseAmp is the default amplitude of noise
		String fName="";//fName is the name of the function
		try
		{	
			if(myFile.getName().endsWith(".r"))
				fName=myFile.getName().replace(".r","");
			if(myFile.getName().endsWith(".R"))
				fName=myFile.getName().replace(".R","");
			PrintWriter out = new PrintWriter(new FileWriter(myFile));
			//out.println(fName+"  "+fFormula+"  "+dim+"  "+noise+"  "+noiseAmp); //TEST LINE			
			out.println(fName+" <- function(spotConfig){");			
			out.println("	f<-\""+fName+"\"");//funktionsname aus apd File
			out.println("	n<-"+dim);//Dimension
			if(noise)out.println("	noise<-NULL");
			out.println("	if (spotConfig$spot.fileMode){");
			out.println("		writeLines(paste(\"Loading design file data from::\",  spotConfig$io.desFileName), con=stderr());");
			out.println("		des <- read.table(spotConfig$io.desFileName, sep=\" \", header = TRUE);");			
			out.println("	}else{");			
			out.println("		des <- spotConfig$alg.currentDesign;");
			out.println("	}");
			if(noise)out.println("	noise="+noiseAmp+"  #Default noise value, will be overwritten if set in .apd");
			out.println("	if(file.exists(spotConfig$io.apdFileName)){source(spotConfig$io.apdFileName,local=TRUE)}");
			out.println("	config<-nrow(des);");
			out.println("	for (k in 1:config){");
			out.println("		for (i in 1:des$REPEATS[k]){");
			for(int i=1;i <= dim;i++){
				//out.println("			if (exists(\"X"+i+"\")){");
				out.println("			x"+i+" <- des$x"+i+"[k]");
				//out.println("			}");
			}
			//out.println("			conf <- k");
			//out.println("			if (exists(\"CONFIG\")){");
			out.println("			conf <- des$CONFIG[k]");
			//out.println("			}");
			//out.println("			if (exists(\"STEP\")){");
			out.println("			step <- des$STEP[k]");
			//out.println("			}");
			out.println("			seed <- des$SEED[k]+i-1");		
			//out.println("			set.seed(seed)"); //in internen funktionen nicht vorhanden.. notwendig?
			out.println("			print(c(\"Config:\",k ,\" Repeat:\",i))");
			//out.println("			y <- spotNoisySphereFunction(c(x1,x2,x3), noise)");
			out.println("			y <- "+fFormula+"+"+noiseAmp+"*rnorm(1)");
			out.println("			print(y)");
			out.println("			res <- NULL");
			out.println("			res <- list(Y=y,");
			for(int i=1;i <= dim;i++){
				//out.println("			if (exists(\"X"+i+"\")){");
				out.println("					x"+i+"=x"+i+",");
				//out.println("			}");
			}			
			//out.println("					VARX1=x1,");
			//out.println("					VARX2=x2,");
			//out.println("					VARX3=x3,");
			out.println("					Function=f,");					
			out.println("					DIM=n,");
			out.println("					STEP=step,");
			out.println("					SEED=seed,");
			out.println("					CONFIG=conf");
			out.println("			)");
			out.println("			res <-data.frame(res)");
			out.println("			if (spotConfig$spot.fileMode){");
			out.println("				colNames = TRUE");
			out.println("				if (file.exists(spotConfig$io.resFileName)){");
			out.println("					colNames = FALSE");
			out.println("				}");	
			out.println("				write.table(res");
			out.println("						, file = spotConfig$io.resFileName");
			out.println("						, row.names = FALSE");
			out.println("						, col.names = colNames");
			out.println("						, sep = \" \"");              
			out.println("						, append = !colNames");
			out.println("						, quote = FALSE");
			out.println("				);");
			out.println("			}");
			out.println("			spotConfig$alg.currentResult=rbind(spotConfig$alg.currentResult,res);");
			out.println("		}");
			out.println("	}");
			out.println("	return(spotConfig)");
			out.println("}");
			out.close();
		}
		catch( IOException e )
		{
			e.printStackTrace();
		}
		
	}
}
