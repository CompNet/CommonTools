package fr.univavignon.tools.file;

/*
 * CommonTools
 * Copyright 2010-19 Vincent Labatut
 * 
 * This file is part of CommonTools.
 * 
 * CommonTools is free software: you can redistribute it and/or modify it under 
 * the terms of the GNU General Public License as published by the Free Software 
 * Foundation, either version 2 of the License, or (at your option) any later version.
 * 
 * CommonTools is distributed in the hope that it will be useful, but WITHOUT ANY 
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with CommonTools. If not, see <http://www.gnu.org/licenses/>.
 */

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Scanner;

/**
 * This class contains various methods
 * related to file management.
 *  
 * @version 2.1
 * @author Vincent Labatut
 */
public class FileTools
{	
	/////////////////////////////////////////////////////////////////
	// FILTERS			/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** Filter able to retain only directories */
	public final static FileFilter FILTER_DIRECTORY = new FileFilter()
	{	@Override
		public boolean accept(File file)
		{	boolean result = file.isDirectory();
			return result;
		}
	};
	
//	/** Filter focusing on raw.txt files */
//	public final static FilenameFilter FILTER_RAW_TEXT = createFilenameFilter(FileNames.FI_RAW_TEXT);
//	/** Filter able to retain only directories containing a raw.txt file */
//	public final static FileFilter FILTER_ARTICLES = new FileFilter()
//	{	@Override
//		public boolean accept(File file)
//		{	boolean result = false;
//			if(file.isDirectory())
//			{	String rf[] = file.list(FILTER_RAW_TEXT);
//				result = rf!=null && rf.length>0;
//			}
//			return result;
//		}
//	};
	
	/**
	 * Creates a filter able to retain only files
	 * with the same name as the specified parameter.
	 * 
	 * @param fileName
	 * 		Targeted filename.
	 * @return
	 * 		Filter dedicated to this name.
	 */
	public final static FilenameFilter createFilenameFilter(final String fileName)
	{	FilenameFilter result = new FilenameFilter()
		{	@Override
			public boolean accept(File folder, String name)
			{	boolean result = fileName.equals(name);
				return result;
			}
		};
		return result;
	}
	
	/**
	 * Creates a filter able to retain only files
	 * with the same name as the specified parameter.
	 * 
	 * @param extension
	 * 		Targeted extension.
	 * @return
	 * 		Filter dedicated to this name.
	 */
	public final static FilenameFilter createExtensionFilter(String extension)
	{	final String ext = extension.toLowerCase(Locale.ENGLISH);
		FilenameFilter result = new FilenameFilter()
		{	@Override
			public boolean accept(File folder, String name)
			{	name = name.toLowerCase(Locale.ENGLISH);
				boolean result = name.endsWith(ext);
				return result;
			}
		};
		return result;
	}
	
	/////////////////////////////////////////////////////////////////
	// READ				/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/**
	 * Open the file at the specified path, for reading.
	 * 
	 * @param filePath 
	 * 		File to open.
	 * @param encoding
	 * 		Encoding of the text file, generally {@code "UTF-8"} 
	 * 		or {@code "ISO-8859-1"}. Or {@code StandardCharsets.UTF_8.name()}.
	 * @return 
	 * 		Scanner object ready to read the file.
	 * 
	 * @throws FileNotFoundException 
	 * 		Problem while accessing the file.
	 * @throws UnsupportedEncodingException 
	 * 		Could not handle the encoding.
	 */
	public static Scanner openTextFileRead(String filePath, String encoding) throws FileNotFoundException, UnsupportedEncodingException
	{	File file = new File(filePath);
		Scanner result = openTextFileRead(file, encoding);
		return result;
	}
	
	/**
	 * Open the specified file for reading.
	 * 
	 * @param file
	 * 		File to open.
	 * @param encoding
	 * 		Encoding of the text file, generally {@code "UTF-8"} 
	 * 		or {@code "ISO-8859-1"}. Or {@code StandardCharsets.UTF_8.name()}.
	 * @return 
	 * 		Scanner object ready to read the file.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the file.
	 * @throws UnsupportedEncodingException
	 * 		Could not handle the encoding.
	 */
	public static Scanner openTextFileRead(File file, String encoding) throws FileNotFoundException, UnsupportedEncodingException
	{	FileInputStream fis = new FileInputStream(file);
//		InputStreamReader isr = new InputStreamReader(fis,"UTF-8");
		InputStreamReader isr;
		if(encoding!=null)
			isr = new InputStreamReader(fis, encoding);
		else
			isr = new InputStreamReader(fis);
		Scanner result = new Scanner(isr);
		return result;
	}

	/**
	 * Reads the specified text file and returns a String object.
	 * 
	 * @param file
	 * 		File to be read.
	 * @param encoding
	 * 		Encoding of the text file, generally {@code "UTF-8"} 
	 * 		or {@code "ISO-8859-1"}. Or {@code StandardCharsets.UTF_8.name()}.
	 * @return
	 * 		String corresponding to the file content.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the file.
	 * @throws UnsupportedEncodingException 
	 * 		Could not handle the encoding.
	 */
	public static String readTextFile(File file, String encoding) throws FileNotFoundException, UnsupportedEncodingException
	{	StringBuffer temp = new StringBuffer();
		Scanner scanner = FileTools.openTextFileRead(file, encoding);
		
		while(scanner.hasNextLine())
		{	String line = scanner.nextLine();
			temp.append(line+"\n");
		}
		
		scanner.close();
		String result = temp.toString();
		return result;
	}
	
	/**
	 * Reads the specified text file and returns a String object.
	 * 
	 * @param filePath
	 * 		Path of the file to be read.
	 * @param encoding
	 * 		Encoding of the text file, generally {@code "UTF-8"} 
	 * 		or {@code "ISO-8859-1"}. Or {@code StandardCharsets.UTF_8.name()}.
	 * @return
	 * 		String corresponding to the file content.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the file.
	 * @throws UnsupportedEncodingException 
	 * 		Could not handle the encoding.
	 */
	public static String readTextFile(String filePath, String encoding) throws FileNotFoundException, UnsupportedEncodingException
	{	File file = new File(filePath);
		
		String result = readTextFile(file, encoding);
		return result;
	}
	
	/////////////////////////////////////////////////////////////////
	// WRITE			/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/**
	 * Open the file at the specified path, for writing.
	 * 
	 * @param filePath
	 * 		File to open.
	 * @param encoding
	 * 		Encoding of the text file, generally {@code "UTF-8"} 
	 * 		or {@code "ISO-8859-1"}. Or {@code StandardCharsets.UTF_8.name()}.
	 * @return 
	 * 		PrintWriter object ready to write in the file.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the file.
	 * @throws UnsupportedEncodingException
	 * 		Could not handle the encoding.
	 */
	public static PrintWriter openTextFileWrite(String filePath, String encoding) throws UnsupportedEncodingException, FileNotFoundException
	{	File file = new File(filePath);
		PrintWriter result = openTextFileWrite(file, encoding);
		return result;
	}
	
	/**
	 * Open the specified file for writing.
	 * 
	 * @param file
	 * 		File to open.
	 * @param encoding
	 * 		Encoding of the text file, generally {@code "UTF-8"} 
	 * 		or {@code "ISO-8859-1"}. Or {@code StandardCharsets.UTF_8.name()}.
	 * @return 
	 * 		PrintWriter object ready to write in the file.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the file.
	 * @throws UnsupportedEncodingException
	 * 		Could not handle the encoding.
	 */
	public static PrintWriter openTextFileWrite(File file, String encoding) throws UnsupportedEncodingException, FileNotFoundException
	{	FileOutputStream fos = new FileOutputStream(file);
		OutputStreamWriter osw = new OutputStreamWriter(fos,encoding);
		PrintWriter result = new PrintWriter(osw);
		return result;
	}

	/**
	 * Records the specified String in a text file.
	 * 
	 * @param file
	 * 		File to be created.
	 * @param content
	 * 		String corresponding to the file content.
	 * @param encoding
	 * 		Encoding of the text file, generally {@code "UTF-8"} 
	 * 		or {@code "ISO-8859-1"}. Or {@code StandardCharsets.UTF_8.name()}.
	 * 
	 * @throws IOException
	 * 		Problem while accessing the file.
	 */
	public static void writeTextFile(File file, String content, String encoding) throws IOException
	{	File folder = file.getParentFile();
		if(!folder.exists())
			folder.mkdirs();
		
		PrintWriter pw = FileTools.openTextFileWrite(file,encoding);
		
		pw.print(content);
		
		pw.close();
	}
	
	/**
	 * Records the specified String in a text file.
	 * 
	 * @param filePath
	 * 		Path of the file to be created.
	 * @param content
	 * 		String corresponding to the file content.
	 * @param encoding
	 * 		Encoding of the text file, generally {@code "UTF-8"} 
	 * 		or {@code "ISO-8859-1"}. Or {@code StandardCharsets.UTF_8.name()}.
	 * 
	 * @throws IOException
	 * 		Problem while accessing the file.
	 */
	public static void writeTextFile(String filePath, String content, String encoding) throws IOException
	{	File file = new File(filePath);
		writeTextFile(file, content, encoding);
	}
	
	/////////////////////////////////////////////////////////////////
	// DELETE			/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/**
	 * Deletes a file or directory, even
	 * if the directory is not empty.
	 * 
	 * @param file
	 * 		File or directory to delete.
	 * @return
	 * 		{@code true} iff the file could be deleted.
	 */
	public static boolean delete(File file)
	{	boolean result = true;
	
		// parameter is a folder
		if(file.isDirectory())
		{	File files[] = file.listFiles();
			for(File f: files)
				result = delete(f) && result;
			result = result && file.delete();
		}
	
		// parameter is a file
		else
			result = file.delete();
		
		return result;
	}
	
	/**
	 * Deletes the specified list of files or directories.
	 * 
	 * @param files
	 * 		List of file paths.
	 * @return 
	 * 		{@code true} iff all files could be deleted.
	 */
	public static boolean delete(List<String> files)
	{	boolean result = true;
	
		for(String file: files)
		{	File f = new File(file);
			result = delete(f) && result;
		}		
		
		return result;
	}
	
	/**
	 * Deletes all the files or directory listed in the
	 * specified file. Each line is supposed to contain
	 * a single path.
	 * 
	 * @param listFile
	 * 		File containing the list of files to delete.
	 * @return 
	 * 		{@code true} iff all files could be deleted.
	 * 
	 * @throws UnsupportedEncodingException
	 * 		Problem while opening the list file.
	 * @throws FileNotFoundException 
	 * 		Problem while opening the list file.
	 */
	public static boolean deleteList(String listFile) throws FileNotFoundException, UnsupportedEncodingException
	{	boolean result = true;
		
		Scanner sc = openTextFileRead(listFile, StandardCharsets.UTF_8.name());
		while(sc.hasNextLine())
		{	String line = sc.nextLine().trim();
			File f = new File(line);
			result = delete(f) && result;
		}
		
		return result;
	}
	
	/////////////////////////////////////////////////////////////////
	// MOVE				/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/**
	 * Moves a file or directory, even if the directory is not empty.
	 * 
	 * @param oldFile
	 * 		File or directory to delete.
	 * @param newFile
	 * 		Resulting file or directory.
	 * @return
	 * 		{@code true} iff every folder and file could be moved.
	 */
	public static boolean move(File oldFile, File newFile)
	{	boolean result = true;
		
		// parameter is a folder
		if(oldFile.isDirectory())
		{	File files[] = oldFile.listFiles();
			for(File of: files)
			{	String path = newFile + File.separator + of.getName();
				File nf = new File(path);
				if(nf.exists())
					nf.delete();
				result = result && move(of,nf);
			}
			if(result)
				oldFile.delete();
		}
		
		// parameter is a file
		else
		{	File parent = newFile.getParentFile();
			if(parent!=null && !parent.exists())
				parent.mkdirs();
			result = oldFile.renameTo(newFile);
		}
		
		return result;
	}
	
	/////////////////////////////////////////////////////////////////
	// COPY				/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/**
	 * Copies a file or directory.
	 * 
	 * @param oldFile
	 * 		File or directory to copy.
	 * @param newFile
	 * 		Resulting file or directory.
	 * @return
	 * 		{@code true} iff every folder and file could be copied.
	 */
	public static boolean copy(File oldFile, File newFile)
	{	boolean result = true;
		
		// parameter is a folder
		if(oldFile.isDirectory())
		{	File files[] = oldFile.listFiles();
			for(File of: files)
			{	String path = newFile + File.separator + of.getName();
				File nf = new File(path);
				if(nf.exists())
					nf.delete();
				result = result && copy(of,nf);
			}
		}
		
		// parameter is a file
		else
		{	File parent = newFile.getParentFile();
			if(parent!=null && !parent.exists())
				parent.mkdirs();
			result = copyFile(oldFile,newFile);
		}
		
		return result;
	}
	
	/**
	 * Copy a single file. Source code adapted from
	 * <a href="http://stackoverflow.com/questions/106770/standard-concise-way-to-copy-a-file-in-java">
	 * StackOverflow</a>
	 * <br/>
	 * Note folders are not created by this function.
	 * 
	 * @param sourceFile
	 * 		Original file.
	 * @param destFile
	 * 		New file.
	 * @return
	 * 		{@code true} iff the original file could be copied.
	 */
	@SuppressWarnings("resource")
	private static boolean copyFile(File sourceFile, File destFile)
	{	boolean result = false;
		
		try
		{	if(!destFile.exists())
				destFile.createNewFile();
			result = true;
		}
		catch (IOException e)
		{	//e.printStackTrace();
		}
		
		if(result)
		{	FileChannel source = null;
			FileChannel destination = null;

		    try 
		    {	source = new FileInputStream(sourceFile).getChannel();
			    destination = new FileOutputStream(destFile).getChannel();
			    destination.transferFrom(source, 0, source.size());
			    result = true;
		    }
		    catch (Exception e)
		    {	//e.printStackTrace();
			}
			finally
			{	try
				{	if(source!=null)
						source.close();
					if(destination!=null)
						destination.close();
				}
				catch (IOException e)
				{	//e.printStackTrace();
				}
		    }
		}
		
		return result;
	}
	
	/////////////////////////////////////////////////////////////////
	// LIST				/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/**
	 * Returns a list of files whose name starts with the specified
	 * prefix, and which are located in the specified folder.
	 *  
	 * @param folder
	 * 		Folder directly containing the files.
	 * @param prefix
	 * 		Begining of the file name.
	 * @return
	 * 		A list of files contained in the folder and whose name starts like the prefix.
	 */
	public static List<File> getFilesStartingWith(String folder, String prefix)
	{	final String pfx = prefix.toLowerCase(Locale.ENGLISH);
		File ff = new File(folder);
		FileFilter filter = new FileFilter()
		{	@Override
			public boolean accept(File file)
			{	String fileName = file.getName().toLowerCase(Locale.ENGLISH);
				boolean result = fileName.startsWith(pfx);
				result = result && !file.isDirectory();
				return result;
			}
		};
		File files[] = ff.listFiles(filter);
		List<File> result =  new ArrayList<File>(Arrays.asList(files));
		Collections.sort(result);
		return result;
	}

	/**
	 * Returns a list of files whose name ends with the specified
	 * suffix, and which are located in the specified folder.
	 *  
	 * @param folder
	 * 		Folder directly containing the files.
	 * @param suffix
	 * 		End of the file name.
	 * @return
	 * 		A list of files contained in the folder and whose name ends like the suffix.
	 */
	public static List<File> getFilesEndingWith(String folder, String suffix)
	{	final String sfx = suffix.toLowerCase(Locale.ENGLISH);
		File ff = new File(folder);
		FileFilter filter = new FileFilter()
		{	@Override
			public boolean accept(File file)
			{	String fileName = file.getName().toLowerCase(Locale.ENGLISH);
				boolean result = fileName.endsWith(sfx);
				result = result && !file.isDirectory();
				return result;
			}
		};
		File files[] = ff.listFiles(filter);
		List<File> result =  new ArrayList<File>(Arrays.asList(files));
		Collections.sort(result);
		return result;
	}
}
