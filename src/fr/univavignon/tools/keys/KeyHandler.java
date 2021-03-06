package fr.univavignon.tools.keys;

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
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jdom2.Element;
import org.xml.sax.SAXException;

import fr.univavignon.tools.file.FileNames;
import fr.univavignon.tools.xml.XmlTools;

/**
 * This class handles the keys, passwords, etc., associated to the
 * access to certain services. 
 *  
 * @version 2
 * @author Vincent Labatut
 */
public class KeyHandler
{	
	/////////////////////////////////////////////////////////////////
	// DATA			/////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** Map containing all the keys */
	public static final Map<String,String> KEYS = new HashMap<String, String>();
	/** Map containing the ids associated to certain keys */
	public static final Map<String,String> IDS = new HashMap<String, String>();
	
	/** Loads the keys and ids */
	static
	{	loadData();
	}
	
	/////////////////////////////////////////////////////////////////
	// XML ATTRIBUTES		/////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** User id associated to a key */
	private static final String ATT_ID = "id";
	/** Name of a key */
	private static final String ATT_NAME = "name";
	/** Value associated to a key */
	private static final String ATT_VALUE = "value";

	/////////////////////////////////////////////////////////////////
	// XML ELEMENTS			/////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** Some text key */
	private static final String ELT_KEY = "key";
	
	/////////////////////////////////////////////////////////////////
	// LOADING		/////////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/**
	 * Loads the list of keys as set by the user in
	 * the appropriate XML file, as well as the
	 * ids possibly associated to certain keys.
	 */
	private static void loadData()
	{	// set up file names
		String dataFileName = FileNames.FO_MISC + File.separator + FileNames.FI_KEY_LIST;
		File dataFile = new File(dataFileName);
		String schemaFileName = FileNames.FO_SCHEMA + File.separator + FileNames.FI_KEY_SCHEMA;
		File schemaFile = new File(schemaFileName);
		
		try
		{	// load XML file
			Element keysElt = XmlTools.getRootFromFile(dataFile, schemaFile);
			
			// populate map
			List<Element> keyElts = keysElt.getChildren(ELT_KEY);
			for(Element keyElt: keyElts)
			{	String name = keyElt.getAttributeValue(ATT_NAME);
				String value = keyElt.getAttributeValue(ATT_VALUE);
				String id = keyElt.getAttributeValue(ATT_ID);
				
				// ignore empty keys or names
				if(!name.isEmpty() && !value.isEmpty())
				{	KEYS.put(name, value);
					if(id!=null)
						IDS.put(name, id);
				}
			}
//			System.out.println("Test");
		}
		catch (SAXException | IOException e)
		{	e.printStackTrace();
		}
	}
}
