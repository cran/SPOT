package spotGui;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

public class hlp { // Help Methods for the spotGui
	/**
	 * returns true if: equals "inf" || "+inf" || "+ inf"
	 */
	public static boolean isRegExPosInf(String val){// Regular Expression is Inf Positive if: equals "inf" || "+inf" || "+ inf"
		String regEx = "(?i)([+]?|([+][ ]))inf";
		return val.matches(regEx);
	}
		
	/**
	 * returns true if: equals "inf" || "-inf" || "- inf"
	 */
	public static boolean isRegExNegInf(String val){// Regular Expression val is Inf Negativ if: equals "inf" || "-inf" || "- inf"
		String regEx = "(?i)([-]+|([-][ ]))inf";
		return val.matches(regEx);
	}
	/**
	 * Checks the (Project name) Prefix (e.g. demo1000)
	 */
	public static boolean validPref(String str){
		String regEx = "[a-zA-Z][\\w]*"; // [\\w]=[a-zA-Z_0-9]
		return str.matches(regEx);
	}
	/**
	 * Checks if input string is valid (INT>0 or "NA")
	 */
	public static boolean validIntNa(String str){
		int val = 0;
		try{
			val = Integer.parseInt(str);
		}catch(Exception e){}
		return (str.equals("NA") || val>0);
	}	
	/**
	 * Checks if input string is valid (INT>0 or "Inf")
	 */
	public static boolean validIntInf(String str){
		int val = 0;
		try{
			val = Integer.parseInt(str);
		}catch(Exception e){}
		return (str.equals("Inf") || val>0);
	}	
	/**
	 * Unniversal solution for "get path 'XX' relative to path 'YY'" (resolves ".." as well)
	 */
	public static String getRelPath(File file, File relativeTo) throws IOException {
		/*
		 * windows seems in some cases not to stop getParent() at 'c:\', which I
		 * considered to be root. For that reason I had to tweak in the
		 * following to 'ugly' lines:
		 */
		file = new File(file + File.separator + "89243jmsjigs45u9w43545lkhj7").getParentFile();
		relativeTo = new File(relativeTo + File.separator + "984mvcxbsfgqoykj30487df556").getParentFile();

		File origFile = file;
		File origRelativeTo = relativeTo;
		ArrayList<File> filePathStack = new ArrayList<File>();
		ArrayList<File> relativeToPathStack = new ArrayList<File>();
		// build the path stack info to compare it afterwards
		file = file.getCanonicalFile();
		while (file != null) {
			filePathStack.add(0, file);
			file = file.getParentFile();
		}
		relativeTo = relativeTo.getCanonicalFile();
		while (relativeTo != null) {
			relativeToPathStack.add(0, relativeTo);
			relativeTo = relativeTo.getParentFile();
		}
		// compare as long it goes
		int count = 0;
		file = filePathStack.get(count);
		relativeTo = relativeToPathStack.get(count);
		while ((count < filePathStack.size() - 1) && (count < relativeToPathStack.size() - 1) && file.equals(relativeTo)) {
			count++;
			file = filePathStack.get(count);
			relativeTo = relativeToPathStack.get(count);
		}
		if (file.equals(relativeTo))
			count++;
		// up as far as necessary
		StringBuffer relString = new StringBuffer();
		for (int i = count; i < relativeToPathStack.size(); i++) {
			relString.append(".." + File.separator);
		}
		// now back down to the file
		for (int i = count; i < filePathStack.size() - 1; i++) {
			relString.append(filePathStack.get(i).getName() + File.separator);
		}
		relString.append(filePathStack.get(filePathStack.size() - 1).getName());
		// just to test
		File relFile = new File(origRelativeTo.getAbsolutePath() + File.separator + relString.toString());
		if (!relFile.getCanonicalFile().equals(origFile.getCanonicalFile())) {
			throw new IOException("Failed to find relative path.");
		}
		return relString.toString();
	}
	/**
	 * surrounds a string with quotes (e.g. for Path with spaces inside)
	 */
	public static String strQuote(String str){
		return "\""+str+"\"";
	}

}
