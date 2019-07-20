package fr.univavignon.tools.freebase;

import fr.univavignon.tools.keys.KeyHandler;
import fr.univavignon.tools.log.HierarchicalLogger;
import fr.univavignon.tools.log.HierarchicalLoggerManager;

/**
 * This class contains methods implementing 
 * some processing related to Freebase.
 * <br/>
 * <b>Note:</b> in order to use Freebase, you need to
 * specify you user key in the res/misc/keys.xml file,
 * using the exact name "Freebase".
 * 
 * @author Vincent Labatut
 */
public class FbCommonTools
{	
	/////////////////////////////////////////////////////////////////
	// LOGGING			/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** Common object used for logging */
	protected static HierarchicalLogger logger = HierarchicalLoggerManager.getHierarchicalLogger();

	/////////////////////////////////////////////////////////////////
	// ACCESS KEY		/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** Key name for Freebase */
	public static final String KEY_NAME = "Freebase";
	
	/**
	 * Returns the key used to access Freebase.
	 *  
	 * @return
	 * 		A string corresponding to the Freebase key.
	 */
	protected static String getKey()
	{	String result = KeyHandler.KEYS.get(KEY_NAME);
		if(result==null)
			throw new NullPointerException("In order to access Freebase, you first need to set up you user key in file res/misc/keys.xml using the exact name \"Freebase\".");
		return result;
	}
	
	/////////////////////////////////////////////////////////////////
	// CACHE		 		/////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** Whether or not Freebase results should be cached */
	protected static boolean cache = true;
	
	/**
	 * Enable or disable the memory cache
	 * for Freebase requests.
	 *  
	 * @param enabled
	 * 		If {@code true}, the results from Freebase are
	 * 		stored in memory.
	 */
	public static void setCacheEnabled(boolean enabled)
	{	FbCommonTools.cache = enabled;
	}
	
	/////////////////////////////////////////////////////////////////
	// URL			 		/////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/** URL of Freebase's MQL-read API */
	protected final static String URL_MQL = "https://www.googleapis.com/freebase/v1/mqlread"; 
	/** URL of Freebase's Topic API */
	protected final static String URL_TOPIC = "https://www.googleapis.com/freebase/v1/topic"; 

	/////////////////////////////////////////////////////////////////
	// STRING		 		/////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	/**
	 * This method normalizes a string so that there is 
	 * no problem when sending it as a request to Freebase.
	 * 
	 * @param input
	 * 		String to be normalized.
	 * @return
	 * 		Normalized string.
	 */
	public static String escapeMqlKey(String input)
	{	String regex = "[A-Za-z0-9_-]";
		StringBuilder builder = new StringBuilder();
		int i=0;
		for(i=0; i<input.length(); i++)
		{	if(input.substring(i, i+1).matches(regex))
			{	builder.append(input.substring(i, i+1));
			}
			else
			{	int code = input.charAt(i);
				builder.append(String.format("$%04x", code).toUpperCase());
			}
		}
		return builder.toString();
	}
}